  # This file contains a class for the M/M/c-type supercomputer model with speed scaling
  
  # state: 1:N - number of servers requested by ith oldest customer being served (if any), or zero; 
  #        N+1: servers requested by first customer in the queue (if any); N+2: queue size (if any); N+3: speed
  # clock: if i=1:N is served, then remaining job amount, N+1: time to arrival 
  # by default, it is an M/M/1 model with rho=0.5
  cluster <- function(gl=list(N=1,
                                p=1,
                                pA=1,
                                cr=list(lambda=0.5,mu=1),
                                pD = 0,
                                speed=c(1,1))) {
    m=gsmp()
    class(m) <- append(class(m),"cluster")
    m$state <- c(1,rep(0,gl$N-1), # customers at service
                 0, # first in the queue class
                 0, # queue size
                 2) # speed
    m$gl <- gl
    m$clocks <- rep(Inf,gl$N+1)
    m$clocks[getActiveEvents(m)] <- getNewClocks(m,getActiveEvents(m))
    return(m)
  }
  
  isRegeneration.cluster <- function(m) {
    return(sum(m$state[1:(m$gl$N+2)])==0 & m$state[m$gl$N+3]==2)
  }
  getPerformance.cluster <- function(m){# what is the model performance measured at given point
    #sapply(0:20,function(i) sum(m$state[1:(m$gl$N+1)]>0)+m$state[m$gl$N+2]==i) # probabilities
    sum(m$state[1:(m$gl$N+1)]>0)+m$state[m$gl$N+2] # number of customers in the system
    #nothing serious below
    #cumsumServers = cumsum(m$state[1:m$gl$N])
    #nBusyServers = max(cumsumServers[which(cumsumServers<=m$gl$N)],0)
    #return(ifelse(m$state[m$gl$N+2] == 1, ifelse(nBusyServers == 0, m$gl$pIdle[1],  m$gl$pLow[nBusyServers]), ifelse(nBusyServers == 0, m$gl$pIdle[2], m$gl$pHigh[nBusyServers])))
  }
  getRates.cluster <- function(m){
    r <- rep(0,m$gl$N+1)
    r[getActiveEvents(m)] <- m$gl$speed[m$state[m$gl$N+3]]
    r[m$gl$N+1]=1
    return(r)
  }
  getNewGSMP.cluster <- function(m,e){
    N=m$gl$N
    p=m$gl$p
    pA=m$gl$pA
    pD=m$gl$pD
    nm <- m
    if(all(e==N+1)){ # arrival
      if(nm$state[N+1]>0) nm$state[N+2] <- nm$state[N+2]+1 # to queue if already not empty
      else{
        cc=sample(1:N,size=1,prob=p)
        if(sum(nm$state[1:N])+cc<=N) nm$state[which.min(nm$state[1:N])]=cc # to servers, if available
        else nm$state[N+1]=cc # to queue otherwise  
      }
      nm$state[N+3]=ifelse(runif(1)<=pA,2,nm$state[N+3])
    }
    else{ # departure
      nm$state[e]=0
      if(nm$state[N+1]>0 & sum(nm$state[1:N])+nm$state[N+1]<=N){ # start serving the first customer from the queue
        nm$state[which.min(nm$state)]=nm$state[N+1]
        nm$state[N+1]=0
        while(nm$state[N+2]>0 & sum(nm$state[1:N])<=N & nm$state[N+1]==0){
          cc=sample(1:N,size=1,prob=p)
          if(sum(nm$state[1:N])+cc<=N)
            nm$state[which.min(nm$state[1:N])] <- cc
          else
            nm$state[N+1] <- cc
          nm$state[N+2] <- nm$state[N+2]-1
        }
      }
      nm$state[N+3]=ifelse(runif(1)<=pD,1,nm$state[N+3])
    }
    return(nm)
  }
  getActiveEvents.cluster <- function(m){
    if(sum(m$state[1:m$gl$N])>0) return(c(which(m$state[1:m$gl$N]>0),m$gl$N+1))
    return(m$gl$N+1)
  }
  getNewClocks.cluster <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
    #return(rexp(length(e),rate=ifelse(e==m$gl$N+1,m$gl$cr$lambda, m$gl$cr$mu[m$state[e]])   ))
    return(ifelse(e==m$gl$N+1,rexp(1,rate=m$gl$cr$lambda), m$gl$cr$x0[m$state[e]]*(1-runif(length(e))*(1-(m$gl$cr$x0[m$state[e]]/1e8)^m$gl$cr$alpha[m$state[e]]) )^(-1/m$gl$cr$alpha[e])) )
  }
  
  m=cluster(list(N=8,
             cr=list(lambda = 0.004829525, 
                     x0 = c(17433825, 16980999, 16528172, 16075345, 15622519, 15169692, 14716865, 14264039), 
                     alpha=rep(2.5,8)),
             p=rep(1/8,8),
             pA=0.8,
             pD = 0.8,
             speed=c(146894.431400289,322042.540508258)
             ))
  #getRegEst(trace(m,1000000))
  
  require(parallel)
  require(doParallel)
  require(foreach)
  cl=makeCluster(detectCores(),type = "FORK")
  registerDoParallel(cl)
  res=foreach(1:detectCores(), .combine=add.statacc) %dopar% trace(m,1000000)
  getRegEst(res)[,"est"]
  stopCluster(cl)
# below there is a checker, just to see that it is working
#m=cluster()
#getRegEst(trace(m,100000)) # should give approx. 1
