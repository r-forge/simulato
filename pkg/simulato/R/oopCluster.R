isRegeneration.mmcluster <- function(m) {
  return(m$state[m$gl$N+1]==0 & m$state[m$gl$N+2]==2)
}
getPerformance.mmcluster <- function(m){# what is the model performance measured at given point
  #sapply(0:10,function(i) m$state[m$gl$N+1]==i) # probabilities
  #sum(sp$state[1:(N+2)])==1 # prob of idle
  # m$state[m$gl$N+1]
  #sum(sp$state[1:(N+1)]>0)+sp$state[N+2]
  cumsumServers = cumsum(m$state[1:m$gl$N])
  nBusyServers = max(cumsumServers[which(cumsumServers<=m$gl$N)],0)
  return(ifelse(m$state[m$gl$N+2] == 1, ifelse(nBusyServers == 0, m$gl$pIdle[1],  m$gl$pLow[nBusyServers]), ifelse(nBusyServers == 0, m$gl$pIdle[2], m$gl$pHigh[nBusyServers])))
}
getRates.mmcluster <- function(m){
  r <- rep(0,m$gl$N+1)
  r[getActiveEvents(m)] <- m$gl$speed[m$state[m$gl$N+2]]
  r[m$gl$N+1]=1
  return(r)
}
getNewGSMP.mmcluster <- function(m,e){
  s=m$state
  N=m$gl$N
  p=m$gl$p
  pA=m$gl$pA
  pD=m$gl$pD
  nm <- m
  if(e==N+1){ # arrival
    nm$state[N+1] <- nm$state[N+1]+1
    if(nm$state[N+1]<=N)  # to queue if already not empty
      nm$state[nm$state[N+1]]=sample(1:N,size=1,prob=p)
    nm$state[N+2]=ifelse(runif(1)<=pA,2,nm$state[N+2])
  }
  else{ # departure
    nm$state=c((nm$state[1:N])[-e],Inf,nm$state[N+1],nm$state[N+2])
    if(nm$state[N+1]>N) # start serving the first customer from the queue
      nm$state[N]=sample(1:N,size=1,prob=p)
    nm$state[N+1] <- nm$state[N+1]-1
    nm$state[N+2]=ifelse(runif(1)<=pD,1,nm$state[N+2])
  }
  return(nm)
}

#' get active events
#'
#' @param m model queueing system
#' @return list of active events
#' @export
getActiveEvents.mmcluster <- function(m){
  if(m$state[m$gl$N+1]>0) return(c(which(cumsum(m$state[1:m$gl$N])<=m$gl$N),m$gl$N+1))
  return(m$gl$N+1)
}

#' get new clocks
#'
#' @param m model queueing system
#' @param e list of events
#' @return list of new clocks
#' @export
getNewClocks.mmcluster <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(e),rate=ifelse(e==m$gl$N+1,m$gl$lambda, m$gl$mu[m$state[e]])   ))
}

#' building trace
#'
#' @param m model queueing system
#' @param numSteps amount arrival customers
#' @param stopTime modeling time
#' @return numeric list of the accumulated parameters
#' @export
trace.mmcluster <- function(m, numSteps = 1e5, stopTime = Inf) {
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
