calc.prob.buckets <- function(blockid, all.persondays, window.time, N, offset) {
  
  block.persondays = all.persondays[all.persondays$block!=blockid, 1:2]
  
  obs.bucket1 = (hours(window.time$window.utime) >= bucket1[1]) & (hours(window.time.block$window.utime) <= bucket1[2])
  obs.bucket2 = (hours(window.time$window.utime) >= bucket2[1]) & (hours(window.time.block$window.utime) <= bucket2[2])
  obs.bucket3 = (hours(window.time$window.utime) >= bucket3[1]) | (hours(window.time.block$window.utime) <= bucket3[2])
  
  data.bucket1 = aggregate(sedentary.width ~ user + study.day, subset(window.time.block, obs.bucket1), FUN = mean)
  data.bucket2 = aggregate(sedentary.width ~ user + study.day, subset(window.time.block, obs.bucket2), FUN = mean)
  data.bucket3 = aggregate(sedentary.width ~ user + study.day, subset(window.time.block, obs.bucket3), FUN = mean)
  
  obs = is.element(data.bucket1$user, block.persondays$user) & is.element(data.bucket1$study.day, block.persondays$study.day) 
  
  window.time.block = window.time[obs,]
  
  model.bucket1 = lmer(sedentary.width ~ 1 + (1 | user), data.bucket1)
  model.bucket2 = lmer(sedentary.width ~ 1 + (1 | user), data.bucket2)
  model.bucket3 = lmer(sedentary.width ~ 1 + (1 | user), data.bucket3)
  
  ### Extract each user-block fitted value
  personalized.prob <- function(user, day, agg.data1, agg.data2, agg.data3) {
    ## Perform Prediction per bucket
    temp = data.bucket1$sedentary.width[data.bucket1$user == user & data.bucket1$study.day < day]
    varcor.temp = as.data.frame(VarCorr(model.bucket1))
    sum.mb = summary(model.bucket1)
    Sigma.temp = varcor.temp$vcov[1]*matrix(1, nrow = length(temp), ncol = length(temp)) + 
      varcor.temp$vcov[2]*diag(1, length(temp) )
    fracsed.bucket1 = sum(solve(Sigma.temp, temp - sum.mb$coefficients[1]))*varcor.temp$vcov[1]+sum.mb$coefficients[1]
    
    temp = data.bucket2$sedentary.width[data.bucket2$user == user & data.bucket2$study.day < day ]
    varcor.temp = as.data.frame(VarCorr(model.bucket2))
    sum.mb = summary(model.bucket2)
    Sigma.temp = varcor.temp$vcov[1]*matrix(1, nrow = length(temp), ncol = length(temp)) + 
      varcor.temp$vcov[2]*diag(1, length(temp) )
    fracsed.bucket2 = sum(solve(Sigma.temp, temp - sum.mb$coefficients[1]))*varcor.temp$vcov[1]+sum.mb$coefficients[1]
    
    temp = data.bucket2$sedentary.width[data.bucket3$user == user & data.bucket3$study.day < day ]
    varcor.temp = as.data.frame(VarCorr(model.bucket3))
    sum.mb = summary(model.bucket3)
    Sigma.temp = varcor.temp$vcov[1]*matrix(1, nrow = length(temp), ncol = length(temp)) + 
      varcor.temp$vcov[2]*diag(1, length(temp) )
    fracsed.bucket3 = sum(solve(Sigma.temp, temp - sum.mb$coefficients[1]))*varcor.temp$vcov[1]+sum.mb$coefficients[1]
    
    prob.bucket1 = N/(12*4*fracsed.bucket1 - offset)
    prob.bucket2 = N/(12*4*fracsed.bucket2 - offset)
    prob.bucket3 = N/(12*4*fracsed.bucket3 - offset)
    
    return( 
      c(prob.bucket1, prob.bucket2, prob.bucket3)
    )
    
  }
  
  return( 
    personalized.prob
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