calc.prob.buckets <- function(blockid, all.persondays, window.time, N, offset) {
  
  block.persondays = all.persondays[all.persondays$block!=blockid, 1:2]
  
  obs = is.element(window.time$user, block.persondays$user) & is.element(window.time$study.day, block.persondays$study.day) 
  
  window.time.block = window.time[obs,]
  
  obs.bucket1 = (hours(window.time.block$window.utime) >= bucket1[1]) & (hours(window.time.block$window.utime) <= bucket1[2])
  obs.bucket2 = (hours(window.time.block$window.utime) >= bucket2[1]) & (hours(window.time.block$window.utime) <= bucket2[2])
  obs.bucket3 = (hours(window.time.block$window.utime) >= bucket3[1]) | (hours(window.time.block$window.utime) <= bucket3[2])
  
  fracsed.bucket1 = mean(window.time.block$sedentary.width[obs.bucket1])
  fracsed.bucket2 = mean(window.time.block$sedentary.width[obs.bucket2])
  fracsed.bucket3 = mean(window.time.block$sedentary.width[obs.bucket3])
  
  prob.bucket1 = N/(12*4*fracsed.bucket1 - offset)
  prob.bucket2 = N/(12*4*fracsed.bucket2 - offset)
  prob.bucket3 = N/(12*4*fracsed.bucket3 - offset)
  
  return( 
    c(prob.bucket1, prob.bucket2, prob.bucket3)
  )
}

randomization.probability <- function(current.state, current.hour, prob.buckets) {
  ## Randomization probabilifty function  
  bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
  if (current.state == 1) {
    if( (current.hour >= bucket1[1]) & (current.hour <= bucket1[2])) {
      return(prob.buckets[1])
    } else if ( (current.hour >= bucket2[1]) & (current.hour <= bucket2[2]) ) {
      return(prob.buckets[2])
    } else if ( (current.hour >= bucket3[1]) | (current.hour <= bucket3[2]) ) {
      return(prob.buckets[3])
    } else { 
      return(0) 
    }
  } else { 
    return(0) 
  }
}

action.assignment <- function(X.t, prob.buckets) {
  ## Application of the randomization probability
  ## to a particular sequence~$X_t$
  
  time.steps = 1:length(X.t)
  hour = (floor(time.steps/12)+14)%%24
  A.t = vector(length = length(time.steps))
  
  for (t in 1:length(time.steps)) {
    current.state = X.t[t]
    current.hour = hour[t]
    if(any(A.t[(max(1,t-12)):(t-1)] == 1)) {
      rho.t = 0
      A.t[t] = 0
    } else {
      rho.t = randomization.probability(current.state, current.hour, prob.buckets)
      A.t[t] = rbinom(n = 1, size = 1, prob = rho.t)
    }
  }  
  return(A.t)
}

action.assignment.avail <- function(X.t, prob.buckets) {
  ## Application of the randomization probability
  ## to a particular sequence~$X_t$
  
  time.steps = 1:length(X.t)
  hour = (floor(time.steps/12)+14)%%24
  A.t = vector(length = length(time.steps))
  
  for (t in 1:length(time.steps)) {
    current.state = X.t[t]
    current.hour = hour[t]
    if(any(A.t[(max(1,t-12)):(t-1)] == 1)) {
      rho.t = 0
      A.t[t] = -1
    } else {
      rho.t = randomization.probability(current.state, current.hour, prob.buckets)
      A.t[t] = rbinom(n = 1, size = 1, prob = rho.t)
    }
  }  
  return(A.t)
}

random.assignment.fn <- function(all.persondays) {
  
  sampled.obs = sample(1:nrow(all.persondays),size = 1)
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(X.t, prob.buckets)
  
  return( 
    c( A.t[1:min(144,length(A.t))], 
       rep(0,max(0,144-length(A.t))), 
       X.t[1:min(144,length(X.t))], 
       rep(0,max(0,144-length(X.t))) 
    )
  )  
}



## Functions for crossvalid.R

which.partition <- function(x) {
  ## Block assignment function
  ceiling(which(partitions == x)/block.size)
}

otherblock.assignment.fn <- function(all.persondays, blockid, N, prob.buckets) {
  ## Give this the full data and the hold out block id
  ## AND the N choice.
  ## Returns mean number of actions
  set.seed("541891")
  
  subset.persondays = subset(all.persondays, block != blockid)
  
  # prob.buckets =prob.bucket calc.prob.buckets(blockid, all.persondays, window.time, N)
  
  return(
    mean(sapply(1:nrow(subset.persondays), mean.avail.fn, subset.persondays, prob.buckets))
  )  
}

mean.assignment.fn <- function(obs, subset.persondays, prob.buckets) {
  userday.combo = as.numeric(subset.persondays[obs,])
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(X.t, prob.buckets)
  
  return( 
    sum(c( A.t[1:min(144,length(A.t))], 
           rep(0,max(0,144-length(A.t)))))
  )  
}

mean.avail.fn <- function(obs, subset.persondays, prob.buckets) {
  userday.combo = as.numeric(subset.persondays[obs,])
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment.avail(X.t, prob.buckets)
  
  return( 
    sum(A.t[X.t==1] == -1)
  )  
}

cv.assignment.fn <- function(sampled.obs, all.persondays, all.Ns, prob.buckets.list) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  blockid = all.persondays[sampled.obs,3]
  
  N = all.Ns[blockid]
  prob.buckets = prob.buckets.list[[blockid]] # calc.prob.buckets(blockid, all.persondays, window.time, N)
  
  A.t <- action.assignment(X.t, prob.buckets)
  
  return( 
    c( A.t[1:min(144,length(A.t))], 
       rep(0,max(0,144-length(A.t))), 
       X.t[1:min(144,length(X.t))], 
       rep(0,max(0,144-length(X.t))) 
    )
  )  
}


cv.assignment.multiple.fn <- function(sampled.obs, all.persondays, all.Ns, num.iters, prob.buckets.list) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  blockid = all.persondays[sampled.obs,3]
  
  N = all.Ns[blockid]
  prob.buckets = prob.buckets.list[[userday.combo[3]]]
  A.t = replicate(n=num.iters, action.assignment(X.t, prob.buckets))
  ## Rewrite to be a replicate function!
  # system.time(
  # for (i in 2:num.iters) {
  #   A.t[i,] = action.assignment.avail(N, lambda, eta, X.t, buckets)
  # }
  # )
  
  p.hat = rowMeans(A.t)
  mean.sumAt = mean(colSums(A.t))
  
  return( 
    c( mean.sumAt, 
       p.hat[1:min(144,length(p.hat))], 
       rep(0,max(0,144-length(p.hat))), 
       X.t[1:min(144,length(X.t))], 
       rep(0,max(0,144-length(X.t)))
    )
  ) 
}