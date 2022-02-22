# state: 1:N - number of servers requested by ith oldest customer being served (if any), or zero; 
#        N+1: queue size (if any); N+2: speed
# clock: if i=1:N is served, then time of service, N+1: time to arrival 
# by default, it is an M/M/1 model with rho=0.5
mmcluster <- function(gl=list(N=1,
                      lambda = 1,
                      mu = 2,
                      p=1,
                      pA=1,
                      pD = 0,
                      speed=c(1,1))) {
  m=gsmp()
  class(m) <- append(class(m),"mmcluster")
  m$state <- c(1,rep(Inf,gl$N-1),1,1)
  m$gl <- gl
  m$clocks <- rep(Inf,gl$N+1)
  m$clocks[getActiveEvents(m)] <- getNewClocks(m,getActiveEvents(m))
  return(m)
}

isRegeneration.gsmp <- function(m) {
  return(m$state[m$gl$N+1]==0 & m$state[m$gl$N+2]==2)
}
getPerformance.gsmp <- function(m){# what is the model performance measured at given point
  #sapply(0:10,function(i) m$state[m$gl$N+1]==i) # probabilities
  #sum(sp$state[1:(N+2)])==1 # prob of idle
  m$state[m$gl$N+1]
  #sum(sp$state[1:(N+1)]>0)+sp$state[N+2]
}
getRates.gsmp <- function(m){
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
getActiveEvents.mmcluster <- function(m){
  if(m$state[m$gl$N+1]>0) return(c(which(cumsum(m$state[1:m$gl$N])<=m$gl$N),m$gl$N+1))
  return(m$gl$N+1)
}
getNewClocks.mmcluster <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(e),rate=ifelse(e==m$gl$N+1,m$gl$lambda, m$gl$mu[m$state[e]])   ))
}

# m=mmcluster(list(N=8,
#           lambda = 0.004638794,
#           mu = rep(4.697978e-08,8),
#           p=rep(1/8,8),
#           pA=1,
#           pD = 0.821,
#           speed=c(144531.85762576,313327.619175194)
#           ))
m=mmcluster()
#plot(getRegEst(trace(m,10000))[,"est"])


getRegEst(trace(m,100000))
