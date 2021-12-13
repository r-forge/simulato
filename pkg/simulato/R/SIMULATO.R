rm(list=ls())
tol=1e-8 # machine epsilon, or tolerance in double comparison

# TQ - accumulated performance for cycles; TC - length of cycles; alpha - significance level
reg_est=function(TQ,TC,alpha){
  Y=TC$RC*0 # here we accumulate the times for indicators of events, e.g. the queue size equals i
  Y[TQ$RC[TQ$Q==i]]=TQ$x[TQ$Q==i] # to those cycles only where these happened
  Z=TC$x # cycle lengths
  n=length(Z) 
  est=sum(Y)/sum(Z) # point estimate
  sq11=sum(Y^2)/(n-1)-sum(Y)^2/(n*(n-1)) # variance of Y
  sq22=sum(Z^2)/(n-1)-sum(Z)^2/(n*(n-1)) # variance of Z
  sq12=sum(Y*Z)/(n-1)-sum(Y)*sum(Z)/(n*(n-1)) # covariance of YZ
  s2=sq11-2*est*sq12+est^2*sq22 # variance esitmate for Y-est*Z
  s=sqrt(s2) # squared deviation of the same
  EZ=mean(Z) # mean regenerative period length
  Kalpha=qnorm(1-alpha/2) # normal distribution quantile
  lbound=est-Kalpha*s/(EZ*sqrt(n)) # confidence interval
  rbound=est+Kalpha*s/(EZ*sqrt(n))
  return(c(lbound,est,rbound))
}

trace <- function(num_steps=1e5, stop_time=Inf, # default values: 10^5 iterations, unlimited modeling time
                  simpoint_init, # initial state for simulation
                  update_state, # state updating function
                  active_events, # obtaining active events for a state
                  start_clocks, # clock intializing function
                  update_rates, # rate updating function
                  is_regeneration, # regeneration checking condition
                  performance) { # performance calculating function per point
  perf_ta <- sim_t <- sim_n <- 0
  n_sp <- sp <- simpoint_init()
  while(sim_t<stop_time & sim_n<num_steps) { # stopping condition
    # In a Generalized Semi-Markov Process the decrease rates of the clocks are constant (various for different clocks) depending on the current state
    times=ifelse(sp$rates<tol,Inf,sp$clocks/sp$rates) # convert clocks to modeling time, conventionally Inf if rate is zero 
    dt <- min(times) # time to the next state transition
    events <- which(times-dt<tol) # trigger event set
    
    # here we also need to grab some statistics
    # regenerative estimation here?: TODO
    perf_ta <- perf_ta+performance(sp)*dt # accumulating time average statistics
     
    if(is_regeneration(sp)) print("WOW")
    else print("FOO")
    # performing the transition and updating the point
    sim_t <- sim_t+dt                     # updating the time
    sim_n <- sim_n+1                      # updating point number
    n_sp$state <- update_state(sp,events) # obtaining a new state

    # taking care of the events and clocks
    p_events <- active_events(sp)           # active events for previous state
    a_events <- active_events(n_sp)         # active events for new state
    i_events <- setdiff(p_events,a_events)  # interrupted events = remaining, but not old, these rates will be zero
    n_events <- union(setdiff(a_events,p_events),intersect(a_events,events)) # new events that need clock start
  
    n_sp$clocks[p_events] <- sp$clocks[p_events]-dt*sp$rates[p_events] # updated clocks for previously active events
    n_sp$clocks[events] <- 0 # events that happened, clocks are zero - this is just to avoid small non-zero values
    if(length(n_events)>0)
      n_sp$clocks[n_events] <- start_clocks(n_sp,n_events)

    # taking care of rates
    n_sp$rates <- update_rates(n_sp)
    n_sp$rates[i_events] <- 0 # zeroing rates
    sp <- n_sp
  }

  perf_ta <- perf_ta / sim_t
  
  # call reg_est here
  
  return(list(perf_ta=perf_ta))
}


############################################################################ BELOW HAS TO BE SET UP FOR EACH MODEL

#setwd("~/Desktop/R/SIMULATO/")
#setwd("/home/ar0/Seafile/My Library/R/SIMULATO")

source("CLUSTER_SpeedScaling.R")

(trace(num_steps = 1000, stop_time = Inf, simpoint_init,update_state,active_events, start_clocks,update_rates,is_regeneration,performance))
