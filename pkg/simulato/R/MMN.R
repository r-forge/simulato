# M/M/N
lambda=1
mu=2
N=3
simpoint_init <- function(){ # here we need to initialize the simulation
  list(state=c(1,rep(0,N)), # N servers, last position = queue
       clocks=c(rexp(1,lambda),rexp(1,rate=mu),rep(0,N-1)),
       rates=c(1,1,rep(0,N-1))) 
}
update_state <- function(sp,events){   # here we need to switch the state based on the triggering event
  s=sp$state
  if(events==1){ # arrival
    if(sum(s)<N) s[which.min(s)]=1 # to server, if available
    else s[N+1]=s[N+1]+1 # to queue otherwise
  }
  else{ # departure
    s[events-1]=0
    if(s[N+1]>0){ # start serving a customer from the queue
      s[N+1]=s[N+1]-1
      s[which.min(s)]=1
    }
  }
  return(s)
}
update_rates <- function(sp){  # here we need to take care of rates
  r=rep(0,N+1)
  r[active_events(sp)]=1
  return(r)
}
active_events <- function(sp) {
  if(sum(sp$state)>0) return(c(1,1+setdiff(which(sp$state>0),N+1)))
  return(1)
}
start_clocks <- function(sp, new_events) {# here we need to process the clocks based on the new events (their numbers)
  ifelse(new_events==1,rexp(1,rate=lambda),rexp(1,rate=mu))
}
performance <- function(sp) {# what is the model performance measured at given point
  sum(sp$state)
}