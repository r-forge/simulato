# M/M/N-type Cluster model
#lambda=0.004556832
# 2 CORES AND optimal #MAXIMAL
#pA=1.0
#pD=0.8062846#0.0
#p=c(0.6,0.4)
#lambda=0.004829525
#speed=c(144531.85762576,313327.619175194)
#mu=c(4.801795000000E-08*0.9,4.801795000000E-08*1.1)
#N=2


# 8 CORES
lambda = 0.004638794
##mu=4.801795000000E-08
mu = rep(4.697978e-08,8)
N=8
##p=rep(1/8,8)
p = c(0.130, 0.128, 0.130, 0.134, 0.115, 0.122, 0.107, 0.134)
pA=1
##pD=0.8062846
pD = 0.821
speed=c(144531.85762576,313327.619175194)


is_regeneration <- function(sp){
  return(sp$state[N+1]==0 & sp$state[N+2]==2)
}

# state: 1:N - number of servers requested by ith oldest customer being served (if any), or zero; N+1: queue size (if any); N+2: speed
# clock: if i=1:N is served, then time of service, N+1: time to arrival 
simpoint_init <- function(){ # here we need to initialize the simulation
  list(state=c(1,rep(Inf,N-1),1,1),
       clocks=c(rexp(1,rate=mu[1]),rep(0,N-1),rexp(1,lambda)),
       rates=c(speed[1],rep(0,N-1),1))
}
update_state <- function(sp,events){   # here we need to switch the state based on the triggering event
  s=sp$state
  if(events==N+1){ # arrival
    s[N+1] <- s[N+1]+1
    if(s[N+1]<=N)  # to queue if already not empty
      s[s[N+1]]=sample(1:N,size=1,prob=p)
    s[N+2]=ifelse(runif(1)<=pA,2,s[N+2])
  }
  else{ # departure
    s=c((s[1:N])[-events],Inf,s[N+1],s[N+2])
    if(s[N+1]>N) # start serving the first customer from the queue
        s[N]=sample(1:N,size=1,prob=p)
    s[N+1] <- s[N+1]-1
    s[N+2]=ifelse(runif(1)<=pD,1,s[N+2])
  }
  return(s)
}
update_rates <- function(sp){  # here we need to take care of rates
  r <- rep(0,N+1)
  r[active_events(sp)] <- speed[sp$state[N+2]]
  r[N+1]=1
  return(r)
}
active_events <- function(sp) {
  if(sp$state[N+1]>0) return(c(which(cumsum(sp$state[1:N])<=N),N+1))
  return(N+1)
}
start_clocks <- function(sp, new_events) {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(new_events),rate=ifelse(new_events==N+1,lambda, mu[sp$state[new_events]])   ))
}
performance <- function(sp) {# what is the model performance measured at given point
  (sp$state[N+1]==WHATTO) # probabilities
  #sum(sp$state[1:(N+2)])==1 # prob of idle
  #sum(sp$state[1:(N+1)]>0)+sp$state[N+2]
}
