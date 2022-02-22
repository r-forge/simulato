statacc <- function(l=list(sumY=0,
                           sumSqY=0,
                           sumZ=0,
                           sumSqZ=0,
                           sumYZ=0,
                           nReg=0)) {
  structure(l,class="statacc")
}

add <- function(a,b) UseMethod("add",a,b)
add.statacc <- function(a,b){
  return(statacc(list(sumY=a$sumY + b$sumY,
                      sumSqY=a$sumSqY + b$sumSqY,
                      sumZ=a$sumZ + b$sumZ,
                      sumSqZ=a$sumSqZ + b$sumSqZ,
                      sumYZ=a$sumYZ + b$sumYZ,
                      nReg=a$nReg + b$nReg)))
}

inc <- function(a,rdp,rdt) UseMethod("inc",a)
inc.statacc <- function(a,rdp,rdt){
  return(statacc(list(sumY=a$sumY + rdp,
                      sumSqY=a$sumSqY + rdp^2,
                      sumZ=a$sumZ + rdt,
                      sumSqZ=a$sumSqZ + rdt^2,
                      sumYZ=a$sumYZ + rdp * rdt,
                      nReg=a$nReg + 1)))
}

getRegEst <- function(a,alpha) UseMethod("getRegEst",a)
getRegEst.statacc <- function(a,alpha=0.05){
  stopifnot(class(a)=="statacc",is.numeric(alpha))
  n <- a$nReg
  est <- a$sumY / a$sumZ # point estimate
  sq11 <- a$sumSqY / (n - 1) - a$sumY^2 / (n * (n - 1)) # variance of Y
  sq22 <- a$sumSqZ / (n - 1) - a$sumZ^2 / (n * (n - 1)) # variance of Z
  sq12 <- a$sumYZ / (n - 1) - a$sumY * a$sumZ / (n * (n - 1)) # covariance of YZ
  s <- sqrt(sq11 - 2 * est * sq12 + est^2 * sq22) # squared root of (variance esitmate for Y-est*Z)
  EZ <- a$sumZ / n # mean regenerative period length
  Kalpha <- qnorm(1 - alpha / 2) # normal distribution quantile
  lbound <- est - Kalpha * s / (EZ * sqrt(n)) # confidence interval
  rbound <- est + Kalpha * s / (EZ * sqrt(n))
  return(cbind(lbound, est, rbound))
}

gsmp <- function() {
  structure(list(state=numeric(),
                 clocks=numeric(),
                 gl=list()), 
            class="gsmp")
}

# these generic functions should be defined for any child of the gsmp class and depend on the model
isRegeneration <- function(m) UseMethod("isRegeneration",m)
getPerformance <- function(m) UseMethod("getPerformance",m)
getRates <- function(m) UseMethod("getRates",m)
getNewGSMP <- function(m,e) UseMethod("getNewGSMP",m)
getActiveEvents <- function(m) UseMethod("getActiveEvents",m)
getNewClocks <- function(m, e) UseMethod("getNewClocks",m)

getTimeDiff <- function(m) UseMethod("getTimeDiff",m)
getTimeDiff.gsmp <- function(m) min(ifelse(getRates(m) < .Machine$double.eps, Inf, m$clocks / getRates(m)))

step <- function(m) UseMethod("step",m)
step.gsmp <- function(m){
  dt <- getTimeDiff(m) # time to the next state transition
  events <- which(m$clocks/getRates(m) - dt < .Machine$double.eps) # trigger event set
  nm <- getNewGSMP(m, events) # obtaining a new state
  # taking care of the events and clocks
  p_events <- getActiveEvents(m) # active events for previous state
  a_events <- getActiveEvents(nm) # active events for new state
  #i_events <- setdiff(p_events, a_events) # interrupted events = remaining, but not old, these rates will be zero
  n_events <- union(setdiff(a_events, p_events), intersect(a_events, events)) # new events that need clock start
  
  nm$clocks[p_events] <- m$clocks[p_events] - dt * getRates(m)[p_events] # updated clocks for previously active events
  nm$clocks[events] <- 0 # events that happened, clocks are zero - this is just to avoid small non-zero values
  if (length(n_events) > 0) {
    nm$clocks[n_events] <- getNewClocks(nm, n_events)
  }
  return(nm)
  # taking care of rates
  #nm$rates <- update_rates(nm)
  #nm$rates[i_events] <- 0 # zeroing rates
}

trace <- function(m, numSteps, stopTime) UseMethod("trace",m)
trace.gsmp <- function(m, numSteps = 1e5, stopTime = Inf) { # performance calculating function per point
  rdp <- rdt <- dn <- gt <- 0 # dp, rdt, dn - deltas of performance, time and number of steps between regenerations; gt - global time
  s <- statacc() # initialize the accumulator
  while (gt < stopTime & dn < numSteps) { # stopping condition
    # In a Generalized Semi-Markov Process the decrease rates of the clocks are constant (various for different clocks) depending on the current state
    dt <- getTimeDiff(m) # time to the next state transition
    rdp <- rdp + getPerformance(m) * dt # accumulating time average statistics
    rdt <- rdt + dt # updating the time
    dn <- dn + 1 # updating point number
    gt <- gt + dt # updating global simulation time
    if (isRegeneration(m)) {
      s <- inc(s,rdp,rdt) # accumulate the cycles in the accumulator
      rdp <- rdt <- 0 # reset the inter-cycle statistics
    }
    nm <- step(m) 
    m <- nm
  }
  return(s)
}
