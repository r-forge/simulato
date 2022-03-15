  # This file contains a class for the M/M/c-type supercomputer model with speed scaling
  
  # state: 1:N - number of servers requested by ith oldest customer being served (if any), or zero; 
  #        N+1: servers requested by first customer in the queue; N+2: queue size (if any); N+3: speed
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
    m$state <- c(1,rep(0,gl$N-1),0,0,1)
    m$gl <- gl
    m$clocks <- rep(Inf,gl$N+1)
    m$clocks[getActiveEvents(m)] <- getNewClocks(m,getActiveEvents(m))
    return(m)
  }
  
  isRegeneration.mmcluster <- function(m) {
    return(sum(m$state[1:(m$gl$N+2)])==0 & m$state[m$gl$N+3]==2)
  }
  getPerformance.mmcluster <- function(m){# what is the model performance measured at given point
    #sapply(0:10,function(i) sum(m$state[1:(m$gl$N+1)]>0)+m$state[m$gl$N+2]==i) # probabilities
    sum(m$state[1:(m$gl$N+1)]>0)+m$state[m$gl$N+2] # number of customers in the system
    #nothing serious below
    #cumsumServers = cumsum(m$state[1:m$gl$N])
    #nBusyServers = max(cumsumServers[which(cumsumServers<=m$gl$N)],0)
    #return(ifelse(m$state[m$gl$N+2] == 1, ifelse(nBusyServers == 0, m$gl$pIdle[1],  m$gl$pLow[nBusyServers]), ifelse(nBusyServers == 0, m$gl$pIdle[2], m$gl$pHigh[nBusyServers])))
  }
  getRates.mmcluster <- function(m){
    r <- rep(0,m$gl$N+1)
    r[getActiveEvents(m)] <- m$gl$speed[m$state[m$gl$N+3]]
    r[m$gl$N+1]=1
    return(r)
  }
  getNewGSMP.mmcluster <- function(m,e){
    N=m$gl$N
    p=m$gl$p
    pA=m$gl$pA
    pD=m$gl$pD
    nm <- m
    if(e==N+1){ # arrival
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
  getActiveEvents.mmcluster <- function(m){
    if(sum(m$state[1:m$gl$N])>0) return(c(which(m$state[1:m$gl$N]>0),m$gl$N+1))
    return(m$gl$N+1)
  }
  getNewClocks.mmcluster <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
    return(rexp(length(e),rate=ifelse(e==m$gl$N+1,m$gl$lambda, m$gl$mu[m$state[e]])   ))
  }
  
  #  m=mmcluster(list(N=8,
  #            lambda = 0.004638794,
  #            mu = rep(4.697978e-08,8),
  #            p=rep(1/8,8),
  #            pA=1,
  #            pD = 0.821,
  #            speed=c(144531.85762576,313327.619175194)
  #            ))
  # 
  # plot(getRegEst(trace(m,10000))[,"est"])
  
  # below there is a checker, just to see that it is working
  m=mmcluster()
  getRegEst(trace(m,100000)) # should give approx. 1
