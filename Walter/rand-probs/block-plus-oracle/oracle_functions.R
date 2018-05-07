oracle.remainder.fn <- function(current.time, X.t) {
  ## Returns number of X.t == 1 left
  block.current = block.steps[current.time]
  start.block.time = min(which(block.steps == block.current))
  time.in.block = current.time - start.block.time + 1
  blockX.t = X.t[block.steps == block.current]
  if(time.in.block == length(blockX.t)) { 
    return(0)
  } else {
    return( sum(blockX.t[(time.in.block+1):length(blockX.t)] == 1) )
  }
}

weighted.history <- function(current.state, time.diff, old.states, lambda, old.A, old.rho) {
  ## Take the history and spit out 
  ## the weighted version according to lambda
  
  sum (( lambda^time.diff * old.A + (1-lambda^time.diff) * old.rho ) * (old.states == current.state))
  
}

randomization.probability <- function(N, current.time, H.t, X.t, lambda, eta, block.steps, max.prob = 0.995, min.prob = 0.005) {
  ## Randomization probabilifty function 
  ## depending on weighted history and full.remainder.fn.
  
  if (current.state == FALSE) {
    return(0) 
  } else {
    temp = (N[current.state+1] - weighted.history(current.state, H.t$time.diff, H.t$old.states, lambda, H.t$old.A, H.t$old.rho))/
             (1 + oracle.remainder.fn(current.time, X.t, block.steps))
    return(min(max(temp,min.prob),max.prob))
  }
  
}  

which.block <- function(current.hour) {
  if( (current.hour >= buckets[[1]][1]) & (current.hour <= buckets[[1]][2])) {
    return(1)
  } else if ( (current.hour >= buckets[[2]][1]) & (current.hour <= buckets[[2]][2]) ) {
    return(2)
  } else if ( (current.hour >= buckets[[3]][1]) | (current.hour <= buckets[[3]][2]) ) {
    return(3)
  } else { 
    return(0) 
  }
}

action.assignment <- function(N, lambda, eta, X.t, buckets) {
  ## Application of the randomization probability
  ## to a particular sequence~$X_t$
  
  H.t = data.frame(
    old.states = rep(0,0),
    old.A = rep(0,0),
    old.rho = rep(0,0),
    time.diff = rep(0,0))
  time.steps = 1:length(X.t)
  hour = (floor(time.steps/12)+14)%%24
  block.steps = unlist(lapply(hour, FUN = which.block))
  A.t = vector(length = length(time.steps))
  
  for (t in 1:length(time.steps)) {
    current.state = X.t[t]
    current.hour = hour[t]
    current.block = block.steps[t]
    which.blocks = which(block.steps == current.block)
    start.block = min(which.blocks); stop.block = max(which.blocks)
    current.run.length = t+1 - max(which(X.t == current.state & 1:length(X.t) <= t))
    # remaining.time = length(time.steps) - (t-1)
    remaining.time.in.block = stop.block - (t - 1)
    if(any(A.t[(max(1,t-12)):(t-1)] == 1)) {
      rho.t = 0
      A.t[t] = 0
    } else {
      rho.t = randomization.probability(N, t, H.t, X.t, lambda, eta, block.steps)
      A.t[t] = rbinom(n = 1, size = 1, prob = rho.t)
    }    
    if (t == stop.block) {
      H.t = data.frame(
        old.states = rep(0,0),
        old.A = rep(0,0),
        old.rho = rep(0,0),
        time.diff = rep(0,0))
    }  else {
      H.t = data.frame(
        old.states = c(H.t$old.states, X.t[t]),
        old.A = c(H.t$old.A, A.t[t]),
        old.rho = c(H.t$old.rho, rho.t),
        time.diff = c(t+1 - 1:t)
      )
    }  
  }
  return(A.t)
}

action.assignment.avail <- function(N, lambda, eta, X.t, buckets) {
  ## Application of the randomization probability
  ## to a particular sequence~$X_t$
  
  H.t = data.frame(
    old.states = rep(0,0),
    old.A = rep(0,0),
    old.rho = rep(0,0),
    time.diff = rep(0,0))
  time.steps = 1:length(X.t)
  hour = (floor(time.steps/12)+14)%%24
  block.steps = unlist(lapply(hour, FUN = which.block))
  A.t = vector(length = length(time.steps))
  
  for (t in 1:length(time.steps)) {
    current.state = X.t[t]
    current.hour = hour[t]
    current.block = block.steps[t]
    which.blocks = which(block.steps == current.block)
    start.block = min(which.blocks); stop.block = max(which.blocks)
    current.run.length = t+1 - max(which(X.t == current.state & 1:length(X.t) <= t))
    # remaining.time = length(time.steps) - (t-1)
    remaining.time.in.block = stop.block - (t - 1)
    if(any(A.t[(max(1,t-12)):(t-1)] == 1)) {
      rho.t = 0
      A.t[t] = -1
    } else {
      rho.t = randomization.probability(N, t, X.t, lambda, eta, block.steps)
      A.t[t] = rbinom(n = 1, size = 1, prob = rho.t)
    }    
    if (t == stop.block) {
      H.t = data.frame(
        old.states = rep(0,0),
        old.A = rep(0,0),
        old.rho = rep(0,0),
        time.diff = rep(0,0))
    }  else {
      H.t = data.frame(
        old.states = c(H.t$old.states, X.t[t]),
        old.A = c(H.t$old.A, A.t[t]),
        old.rho = c(H.t$old.rho, rho.t),
        time.diff = c(t+1 - 1:t)
      )
    }  
  }
  return(A.t)
}

random.assignment.fn <- function(all.persondays) {
  
  sampled.obs = sample(1:nrow(all.persondays),size = 1)
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(N, lambda, eta, X.t)
  
  return( 
    c( A.t[1:min(136,length(A.t))], 
       rep(0,max(0,136-length(A.t))), 
       X.t[1:min(136,length(X.t))], 
       rep(0,max(0,136-length(X.t))) 
       )
  )  
}



## Functions for crossvalid.R

which.partition <- function(x) {
  ## Block assignment function
  ceiling(which(partitions == x)/block.size)
}

otherblock.assignment.fn <- function(all.persondays, blockid, N) {
  ## Give this the full data and the hold out block id
  ## AND the N choice.
  ## Returns mean number of actions
  set.seed("541891")
  
  subset.persondays = subset(all.persondays, block != blockid)
  
  return(
    mean(sapply(1:nrow(subset.persondays), mean.assignment.fn, subset.persondays, N))
  )  
}

mean.assignment.fn <- function(obs, subset.persondays, N) {
  userday.combo = as.numeric(subset.persondays[obs,])
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]

  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(N, lambda, eta, X.t)

  return( 
    sum(c( A.t[1:min(136,length(A.t))], 
            rep(0,max(0,136-length(A.t)))))
  )  
}

cv.assignment.fn <- function(sampled.obs, all.persondays, all.Ns) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  N = c(0,all.Ns[all.persondays[sampled.obs,3]])
  
  A.t <- action.assignment(N, lambda, eta, X.t)
  
  return( 
    c( A.t[1:min(136,length(A.t))], 
       rep(0,max(0,136-length(A.t))), 
       X.t[1:min(136,length(X.t))], 
       rep(0,max(0,136-length(X.t))) 
    )
  )  
}

cv.assignment.multiple.fn <- function(sampled.obs, all.persondays, all.Ns, num.iters) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  N = c(0,all.Ns[all.persondays[sampled.obs,3]])
  
  A.t = replicate(n=num.iters, action.assignment.avail(N, lambda, eta, X.t, buckets))
  ## Rewrite to be a replicate function!
  # system.time(
  # for (i in 2:num.iters) {
  #   A.t[i,] = action.assignment.avail(N, lambda, eta, X.t, buckets)
  # }
  # )

  avail_and_act = matrix(A.t==1, nrow = nrow(A.t), ncol = ncol(A.t))
  
  p.hat = rowMeans(avail_and_act)
  
  return( 
    c( p.hat[1:min(136,length(p.hat))], 
       rep(0,max(0,136-length(p.hat))), 
       X.t[1:min(136,length(X.t))], 
       rep(0,max(0,136-length(X.t))) 
    )
  )  
}

