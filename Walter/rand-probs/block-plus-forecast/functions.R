exp.time.rem.in.state <- function(remaining.time, current.state, current.run.length, max.val = 100) {
  ## Computes the expected remaining time in Sedendatry 
  ## or Not Sedentary state
  ## Within each bucket
  
  temp = Sedentary.length[Sedentary.values == current.state 
                          & Sedentary.length > current.run.length
                          & Sedentary.length < max.val] - current.run.length
  r_min_x.temp = remaining.time * (temp > remaining.time) + temp * (temp <= remaining.time)
  r_minus_x_plus.temp = (remaining.time - temp)* ((remaining.time - temp) > 0)
  return(list("r_min_x.mean" = mean(r_min_x.temp), 
              "r_min_x.var" = var(r_min_x.temp),
              "r_minus_x_plus.mean" = mean(r_minus_x_plus.temp), 
              "r_minus_x_plus.var" = var(r_minus_x_plus.temp),
              "r_min_x.r_minus_x_plus.cov" = cov(r_min_x.temp, r_minus_x_plus.temp)
  ))
}

fraction.time.in.state <- function(current.hour) {
  ## Computes fraction of time in Sedentary 
  ## or Not Sedentary for remainder of study at the 
  ## hour level
  
  if(current.hour > 24) {stop("Hour outside normal range")}
  if(current.hour < 3) {
    remaining.data = window.time[hour(window.time$window.utime) >= current.hour
                                 & hour(window.time$window.utime) < 3, ] 
  } else {
    remaining.data = window.time[hour(window.time$window.utime) >= current.hour
                                 | hour(window.time$window.utime) < 3, ] 
  }
  
  temp = aggregate(sedentary.width ~ user + study.day, 
                   data = remaining.data,
                   FUN = function(x) mean(x == TRUE))
  
  return(list("mean" = mean(temp[,3]), "var" = var(temp[,3])))
}

fraction.time.in.state.user.re <- function(current.hour) {
  ## Computes fraction of time in Sedentary 
  ## or Not Sedentary for remainder of study at the 
  ## hour level
  
  if(current.hour > 24) {stop("Hour outside normal range")}
  if(current.hour < 3) {
    remaining.data = window.time[hour(window.time$window.utime) >= current.hour
                                 & hour(window.time$window.utime) < 3, ] 
  } else {
    remaining.data = window.time[hour(window.time$window.utime) >= current.hour
                                 | hour(window.time$window.utime) < 3, ] 
  }
  
  temp = aggregate(sedentary.width ~ user + study.day, 
                   data = remaining.data,
                   FUN = function(x) mean(x == TRUE))
   
  temp.lmer = lmer(sedentary.width ~ 1 + (1 | user), data = temp)
  summary.lmer = summary(temp.lmer)
  
  return(list(
    "mean" = summary.lmer$coefficients[1], 
    "varcov" = as.numeric(c(summary.lmer$varcor$user, summary.lmer$sigma^2)) 
  )
  )
}

full.remainder.fn <- function(remaining.time, current.state, current.run.length, current.hour, eta) {
  ## Combines exp. and fraction. functions to 
  ## Produce estimate of expectation and standard 
  ## deviation e(i,r) and sd(i,r).  Use these with
  ## eta to compute remainder function.
  
  remaining.temp = exp.time.rem.in.state(remaining.time, current.state, current.run.length)
  fraction.temp = fraction.df[fraction.df$current.hour == current.hour,]
  
  complete.mean = (current.state==TRUE)*remaining.temp$r_min_x.mean +
    remaining.temp$r_minus_x_plus.mean * fraction.temp$mean     
  complete.variance = (current.state==TRUE)*(remaining.temp$r_min_x.var +
                                               fraction.temp$mean * remaining.temp$r_min_x.r_minus_x_plus.cov) +
    fraction.temp$var * remaining.temp$r_minus_x_plus.var +
    fraction.temp$mean^2 * remaining.temp$r_minus_x_plus.var +
    fraction.temp$var * remaining.temp$r_minus_x_plus.mean^2
  return(complete.mean - eta*sqrt(complete.variance))
}


weighted.history <- function(current.state, time.diff, old.states, lambda, old.A, old.rho) {
  ## Take the history and spit out 
  ## the weighted version according to lambda
  
  sum (( lambda^time.diff * old.A + (1-lambda^time.diff) * old.rho ) * (old.states == current.state))
  
}

randomization.probability <- function(N, current.state, remaining.time, current.run.length, current.hour, H.t, lambda, eta, max.prob = 0.995, min.prob = 0.005) {
  ## Randomization probabilifty function 
  ## depending on weighted history and full.remainder.fn.
  
  if (current.state == FALSE) {
    return(0) 
  } else {
    temp = (N[current.state+1] - weighted.history(current.state, H.t$time.diff, H.t$old.states, lambda, H.t$old.A, H.t$old.rho))/
             full.remainder.fn(remaining.time, current.state, current.run.length, current.hour, eta)
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
      rho.t = randomization.probability(N, current.state, remaining.time.in.block, current.run.length, current.hour, H.t, lambda, eta)
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

cv.assignment.multiple.fn <- function(sampled.obs, all.persondays, all.Ns) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  
  N = c(0,all.Ns[all.persondays[sampled.obs,3]])
  
  A.t = matrix(nrow = num.iters, ncol = length(X.t))
  A.t[1,] = action.assignment.avail(X.t, prob.buckets)
  for (i in 2:num.iters) {
    A.t[i,] = action.assignment.avail(X.t, prob.buckets)
  }
  
  avail_and_act = matrix(A.t==1, nrow = nrow(A.t), ncol = ncol(A.t))
  
  p.hat = colMeans(avail_and_act)
  
  return( 
    c( p.hat[1:min(136,length(p.hat))], 
       rep(0,max(0,136-length(p.hat))), 
       X.t[1:min(136,length(X.t))], 
       rep(0,max(0,136-length(X.t))) 
    )
  )  
}

