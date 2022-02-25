source("~/projects/simulato/pkg/simulato/R/oop.R")
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
getActiveEvents.mmcluster <- function(m){
  if(m$state[m$gl$N+1]>0) return(c(which(cumsum(m$state[1:m$gl$N])<=m$gl$N),m$gl$N+1))
  return(m$gl$N+1)
}
getNewClocks.mmcluster <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(e),rate=ifelse(e==m$gl$N+1,m$gl$lambda, m$gl$mu[m$state[e]])   ))
}

power_idle_1 = 11.0925788473718 # низкая скорость, простой
power_idle_2 = 11.1296306926904 # высокая скорость, простой
power_low_1 = 16.9887613853593 # низкая скорость, один сервер занят
power_low_2 = 17.7956569754339 # низкая скорость, два сервера заняты
power_high_1 = 23.6930784483827 # высокая скорость, один сервер занят
power_high_2 = 25.6341361256545 # высокая скорость, два сервера заняты

m=mmcluster(list(N=2,
          lambda = 0.004829525,
          mu = c(0.000000043216155,0.000000052819745),
          p=c(0.6,0.4),
          pA=1,
          pD = 0.8062846,
          speed=c(144531.85762576,313327.619175194),
          pIdle=c(power_idle_1, power_idle_2),
          pLow=c(power_low_1, power_low_2),
          pHigh=c(power_high_1, power_high_2)
          ))
# m=mmcluster()
#plot(getRegEst(trace(m,10000))[,"est"])


getRegEst(trace(m,100000))
