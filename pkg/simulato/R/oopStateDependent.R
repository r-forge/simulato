# This file contains a class for the multiclass inventory-retrial model

#globalcounter=0

# State:
# 1:S -- number of this class customers in service zone
# S+1 -- orbit size
# S+2 -- inventory size
# Clock:
# 1:S -- residual service time of this class
# S+1 -- residual retrial time
# S+2 -- residual interarrival time
# S+3 -- residual replenishment time
queueinventory <- function(gl=list(S=3, # number of classes = inventory size
                                   s=1, # inventory parameter, reorder point
                                   gamma= 3, # replenishment rate
                                   psS=0.5, # type of replenishment
                                   lambda = 0.2, # arrival rate
                                   p=(1:3)/sum(1:3), # customer class arrival probability
                                   mu = (3:1)/5, # service rates vector
                                   eta = 2, # retrial rate
                                   phi=rep(0.1,3))) {# orbit joining probability
  #if(with(gl,sum(lambda*p/mu)+max(lambda*p/(lambda*p+eta))>1)) stop("Unstable")
  m=gsmp()
  class(m) <- append(class(m),"queueinventory")
  m$state <- c(1,rep(0,gl$S-1),0,gl$S) # start by serving a single class-1 customer
  m$gl <- gl
  m$clocks <- rep(Inf,gl$S+3)
  m$clocks[getActiveEvents(m)] <- getNewClocks(m,getActiveEvents(m))
  return(m)
}

isRegeneration.queueinventory <- function(m) {
  return(all(m$state[1:(m$gl$S+1)]==0) & m$state[m$gl$S+2]==m$gl$S)
}
getPerformance.queueinventory <- function(m){# what is the model performance measured at given point
  #m$state[m$gl$N+1]>0 # busy probability
  #m$state[m$gl$N+1]==1:m$gl$N # per class busy probability
  #m$state[m$gl$N+1]==0 & m$state[1:m$gl$N]==0 # per class idle with idle orbit
  #m$state[m$gl$N+1]==0 & m$state[1:m$gl$N]>0 # per class idle with busy orbit
  #return(m$state[m$gl$S+1]) # orbit-queue size
  return(all(m$state==0)) # idle probability
  #return(m$state[m$gl$S+2]) # inventory size #c(m$state[1],m$state[2],m$state[1]^2,m$state[2]^2,m$state[1]*m$state[2]))
}
getRates.queueinventory <- function(m){
  r <- rep(0,length(m$clocks))
  r[getActiveEvents(m)] <- 1
  return(r)
}
getNewGSMP.queueinventory <- function(m,e){
  S <- m$gl$S
  nm <- m
  fits=function(m,clarr) return(sum((1:S)*m$state[1:S])+clarr<=m$state[S+2])
  if(e==S+2){ # arrival
    clarr=sample.int(S,size=1,prob = m$gl$p)
    if(fits(m,clarr)) # can be accomodated in the service zone
      nm$state[clarr] <- m$state[clarr]+1 # this arrival goes to service
    else if(runif(1)<=m$gl$phi[clarr]) # decides to join the orbit
      nm$state[S+1] <- m$state[S+1]+1
  } else if(e<=S){ # departure from service zone
    nm$state[e]=m$state[e]-1
    nm$state[S+2]=m$state[S+2]-e
  } else if(e==S+1){ # retrial
    clarr=sample.int(S,size=1,prob = m$gl$p)
    if(fits(m,clarr)){ # successful
      nm$state[clarr] <- m$state[clarr]+1
      nm$state[e] <- m$state[e]-1
    }
  } else if(e==S+3){ # replenishment
    nm$state[S+2]=ifelse(runif(1)<m$gl$psS,m$gl$S,nm$state[S+2]+m$gl$S-m$gl$s)
    #globalcounter <<- globalcounter+1 # to count replenishments
  }
  return(nm)
}
getActiveEvents.queueinventory <- function(m){
  a=which(m$state[1:(m$gl$S+1)]>0)
  a=c(a,m$gl$S+2)
  if(m$state[m$gl$S+2]<=m$gl$s)
    a=c(a,m$gl$S+3)
  return(a)
}
getNewClocks.queueinventory <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
  rates=c(m$gl$mu,m$gl$eta,m$gl$lambda,m$gl$gamma)[e]
  return(rexp(length(e),rate=rates))#ifelse(e==m$gl$S+2,m$gl$lambda, # arrival
                                    #ifelse(e<=m$gl$S,m$gl$mu[e], # service
                                    #ifelse(e==m$gl$S+3,m$gl$gamma, # replenishment
                                    #       m$gl$eta))   ))) # retrial
}
