# M/M/N-type Cluster model
lambda=1
mu=1
N=2
p=c(.5,.5)
#psw=0.01
speed=c(0.9,1.2)
#p=c(.3,.2,.3,.2)

# state: 1:N - number of servers requested by ith oldest customer being served (if any), or zero; N+1: servers requested by first customer in the queue; N+2: queue size (if any); N+3: speed
# clock: if i=1:N is served, then time of service, N+1: time to arrival 
simpoint_init <- function(){ # here we need to initialize the simulation
  list(state=c(1,rep(0,N+1),2),
       clocks=c(rexp(1,rate=mu),rep(0,N-1),rexp(1,lambda)),
       rates=c(speed[2],rep(0,N-1),1))
}
update_state <- function(sp,events){   # here we need to switch the state based on the triggering event
  s=sp$state
  if(events==N+1){ # arrival
    if(s[N+1]>0) s[N+2] <- s[N+2]+1 # to queue if already not empty
    else{
      customer_class=sample(1:N,size=1,prob=p)
      if(sum(s[1:N])+customer_class<=N) s[which.min(s[1:N])]=customer_class # to servers, if available
      else s[N+1]=customer_class # to queue otherwise  
    }
    s[N+3]=ifelse(runif(1)<=psw,2,s[N+3])
  }
  else{ # departure
    s[events]=0
    if(s[N+1]>0 & sum(s[1:N])+s[N+1]<=N){ # start serving the first customer from the queue
      s[which.min(s)]=s[N+1]
      s[N+1]=0
      while(s[N+2]>0 & sum(s[1:N])<=N & s[N+1]==0){
        customer_class=sample(1:N,size=1,prob=p)
        if(sum(s[1:N])+customer_class<=N)
          s[which.min(s[1:N])] <- customer_class
        else
          s[N+1] <- customer_class
        s[N+2] <- s[N+2]-1
      }
    }
    s[N+3]=ifelse(runif(1)<=psw,1,s[N+3])
  }
  return(s)
}
update_rates <- function(sp){  # here we need to take care of rates
  r <- rep(0,N+1)
  r[active_events(sp)] <- speed[sp$state[N+3]]
  r[N+1]=1
  return(r)
}
active_events <- function(sp) {
  if(sum(sp$state)>0) return(c(which(sp$state[1:N]>0),N+1))
  return(N+1)
}
start_clocks <- function(sp, new_events) {# here we need to process the clocks based on the new events (their numbers)
  return(rexp(length(new_events),rate=ifelse(new_events==N+1,lambda,mu)))
}
performance <- function(sp) {# what is the model performance measured at given point
  #(sum(sp$state[1:(N+1)]>0)+sp$state[N+2]==0) # probabilities
  sum(sp$state[1:(N+2)])==0
}