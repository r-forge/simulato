
# This file contains a class for the multiclass M/M/1-type multiple orbits model with constant retrial rates

# state: 1:N - number of customers at orbit 1...N
#        N+1: server state (0 if idle, or customer class)
#        N+2: next arriving customer class
# clock: i=1:N - inter-retrial times (if any), N+1: remaining service time, N+2 - time to arrival 
manyorbits <- function(gl=list(N=2, # number of orbits
                              lambda = 0.2, # arrival rate
                              p=c(0.99,0.01), # customer class arrival probability
                              mu = c(20,0.01), # service rates vector
                              lambda_r = c(1,1), # retrial rates vector
                              b=c(1,1))) {# orbit joining probability
  if(with(gl,sum(lambda*p/mu)+max(lambda*p/(lambda*p+lambda_r))>1)) stop("Unstable")
  m=gsmp()
  class(m) <- append(class(m),"manyorbits")
  m$state <- c(rep(0,gl$N),1,1) # start by serving a single class-1 customer
  m$gl <- gl
  m$clocks <- rep(Inf,gl$N+2)
  m$clocks[getActiveEvents(m)] <- getNewClocks(m,getActiveEvents(m))
  return(m)
}

isRegeneration.manyorbits <- function(m) {
  return(all(m$state[1:m$gl$N]==0) & m$state[m$gl$N+2]==1)
}
getPerformance.manyorbits <- function(m){# what is the model performance measured at given point
  #m$state[m$gl$N+1]>0 # busy probability
  #m$state[m$gl$N+1]==1:m$gl$N # per class busy probability
  #m$state[m$gl$N+1]==0 & m$state[1:m$gl$N]==0 # per class idle with idle orbit
  #m$state[m$gl$N+1]==0 & m$state[1:m$gl$N]>0 # per class idle with busy orbit
  return(c(m$state[1],m$state[2],m$state[1]^2,m$state[2]^2,m$state[1]*m$state[2]))
}
getRates.manyorbits <- function(m){
  r <- rep(0,m$gl$N+2)
  r[getActiveEvents(m)] <- 1
  r[m$gl$N+2]=1
  return(r)
}
getNewGSMP.manyorbits <- function(m,e){
  s=m$state
  N=m$gl$N
  nm <- m
  if(e==N+2){ # arrival
    if(nm$state[N+1]==0)
      nm$state[N+1] <- m$state[N+2] # this arrival goes to service
    else if(runif(1)<=m$gl$b[m$state[N+2]])
      nm$state[m$state[N+2]] <- m$state[m$state[N+2]]+1
    nm$state[N+2] <- sample(N,1,prob=m$gl$p) # sample class of the next arrival
  }
  else if(e==N+1){ # departure
    nm$state[N+1]=0
  }
  else{ # retrial
    if(nm$state[N+1]==0){ # successful
      nm$state[N+1] <- e
      nm$state[e] <- m$state[e]-1
    }
  }
  return(nm)
}
getActiveEvents.manyorbits <- function(m){
  return(which(m$state>0))
}
getNewClocks.manyorbits <- function(m, e)  {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(e),rate=ifelse(e==m$gl$N+2,m$gl$lambda, ifelse(e==m$gl$N+1,m$gl$mu[m$state[m$gl$N+1]],m$gl$lambda_r[e]))   ))
}

getExplicitSolution <- function(m){ #explicit solutions for the model
  rho_v=with(m$gl,(lambda*p/mu))
  rho=sum(rho_v)
  C=1+rho-sum(rho_v*m$gl$b)
  Pb=rho/C # busy probability
  Pb_v=rho_v*(1-Pb*(1-m$gl$b)) # per class busy probability
  P0b_v=with(m$gl,lambda*p*b/lambda_r)*Pb # per class orbit busy with idle server
  P00_v=1-Pb-P0b_v # per class orbit idle with idle server
  return(list(Pb=Pb,Pb_v=Pb_v,P0b_v=P0b_v,P00_v=P00_v))
}

#plot(getRegEst(trace(m,10000))[,"est"])

# below there is a checker, just to see that it is working
correxp=function(b2,nruns){
  m=manyorbits(gl=list(N=2, # number of orbits
                       lambda = 0.2, # arrival rate
                       p=c(0.99,0.01), # customer class arrival probability
                       mu = c(20,0.01), # service rates vector
                       lambda_r = c(1,1), # retrial rates vector
                       b=c(1,b2)))
  co=getRegEst(trace(m,nruns))[,"est"]
  return((co[5]-co[1]*co[2])/(sqrt(co[3]-co[1]^2)*sqrt(co[4]-co[2]^2)))
}

# v=0
# for(i in seq(0.01,1,0.01))
#   v=append(v,correxp(i,1000000))
# #getExplicitSolution(m)

#library(doParallel)
#library(foreach)
#nCores <- detectCores() - 1
#cl <- makeCluster(nCores)
#registerDoParallel(cl)
#v2 <- foreach(i = seq(0.01,1,0.05),.export=ls(), .combine = c) %dopar% correxp(i,10000000)
#save(v2,file="~/Desktop-WORK/v2.Rd")

#plot( seq(0.01,1,0.05),v2,type="l")
