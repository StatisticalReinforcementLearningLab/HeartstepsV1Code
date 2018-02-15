exp.time.rem.in.state <- function(remaining.time, current.state, current.run.length, max.val = 100) {
  ## Computes the expected remaining time in Sedendatry 
  ## or Not Sedentary state
  
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

fraction.time.in.state.user.re <- function(current.hour, all.persons, blockid) {
  ## Computes fraction of time in Sedentary 
  ## or Not Sedentary for remainder of study at the 
  ## hour level
  training.persons = all.persons[all.persons[,2] != blockid,1]
  obs= is.element(window.time$user, training.persons)
  block.window.time = window.time[obs,]
  
  if(current.hour > 24) {stop("Hour outside normal range")}
  if(current.hour < 3) {
    remaining.data = block.window.time[hour(block.window.time$window.utime) >= current.hour
                                 & hour(block.window.time$window.utime) < 3, ] 
  } else {
    remaining.data = block.window.time[hour(block.window.time$window.utime) >= current.hour
                                 | hour(block.window.time$window.utime) < 3, ] 
  }
  
  temp = aggregate(sedentary.width ~ user + study.day, 
                   data = remaining.data,
                   FUN = function(x) mean(x == TRUE))
   
  temp.lmer = lmer(sedentary.width ~ 1 + (1 | user), data = temp)
  summary.lmer = summary(temp.lmer)
  
  return(list(
    "mean" = summary.lmer$coefficients[1], 
    "varcov" = as.numeric(c(summary.lmer$varcor$user, summary.lmer$sigma^2)),
    "block.data" = temp 
  )
  )
}

Bayes.fraction.time <- function(current.hour, current.person, current.day, blockid, param.list, sedwidth.df) {
  ## Returns Bayes estimate of fraction of time
  ## Given parameters and sedentary width data frame
  y = sedwidth.df$sedentary.width[sedwidth.df$user == current.person & sedwidth.df$study.day <= current.day]
  mu = param.list[[1]][blockid, is.element(seq.hour,current.hour)]; 
  var.cov = c( param.list[[2]][blockid, is.element(seq.hour,current.hour)], 
               param.list[[3]][blockid, is.element(seq.hour,current.hour)] )
  ones = rep(1,length(y))
  var.cov = as.numeric(c(summary.lmer$varcor$user, summary.lmer$sigma^2)) 
  Sigma = var.cov[1] * outer(ones,ones)  + var.cov[2] * diag(ones)
  
  return( 
    mu + var.cov[1]*t(ones)%*%solve(Sigma, (y-mu))
  )
}

full.remainder.fn <- function(remaining.time, current.state, current.run.length, current.hour, eta, current.person, current.day, blockid, param.list, sedwidth.df) {
  ## Combines exp. and fraction. functions to 
  ## Produce estimate of expectation and standard 
  ## deviation e(i,r) and sd(i,r).  Use these with
  ## eta to compute remainder function.
  
  remaining.temp = exp.time.rem.in.state(remaining.time, current.state, 
                                         current.run.length)
  fraction.temp = Bayes.fraction.time(current.hour, current.person, current.day, 
                                      blockid, param.list, sedwidth.df)

  complete.mean = (current.state==TRUE)*remaining.temp$r_min_x.mean +
    remaining.temp$r_minus_x_plus.mean * fraction.temp     
  return(complete.mean)
}


weighted.history <- function(current.state, time.diff, old.states, lambda, old.A, old.rho) {
  ## Take the history and spit out 
  ## the weighted version according to lambda
  
  sum (( lambda^time.diff * old.A + (1-lambda^time.diff) * old.rho ) * (old.states == current.state))
  
}

randomization.probability <- function(N, current.state, remaining.time, current.run.length, current.hour, 
                                      H.t, lambda, eta, current.person, current.day, blockid, param.list,
                                      sedwidth.df, max.prob = 0.995, min.prob = 0.005) {
  ## Randomization probabilifty function 
  ## depending on weighted history and full.remainder.fn.
  
  if (current.state == FALSE) {
    return(0) 
  } else {
    temp = (N[current.state+1] - weighted.history(current.state, H.t$time.diff, H.t$old.states, lambda, H.t$old.A, H.t$old.rho))/
      full.remainder.fn(remaining.time, current.state, current.run.length, 
                        current.hour, eta, current.person, current.day, 
                        blockid, param.list, sedwidth.df)
  
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

action.assignment <- function(N, lambda, eta, X.t, buckets, 
                              current.person, current.day, 
                              blockid, param.list, sedwidthdf.list) {
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
    sedwidth.df = sedwidthdf.list[[which(seq.hour == current.hour)]]
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
      rho.t = randomization.probability(N, current.state, remaining.time, current.run.length, 
                                        current.hour, H.t, lambda, eta, current.person, 
                                        current.day, blockid, param.list, sedwidth.df)
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

random.assignment.fn <- function(all.persondays, all.persons, param.list, sedwidthdf.list) {
  
  sampled.obs = sample(1:nrow(all.persondays),size = 1)
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  current.person = userday.combo[1]
  current.day = userday.combo[2]
  blockid = all.persons[all.persons[,1] == current.person,2]
  
  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(N, lambda, eta, X.t, buckets, 
                           current.person, current.day, 
                           blockid, param.list, sedwidthdf.list)
  
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

otherblock.assignment.fn <- function(all.persondays, blockid, N, all.persons, param.list, sedwidth.df) {
  ## Give this the full data and the hold out block id
  ## AND the N choice.
  ## Returns mean number of actions
  set.seed("541891")
  
  subset.persondays = subset(all.persondays, block != blockid)
  
  return(
    mean(sapply(1:nrow(subset.persondays), mean.assignment.fn, 
                subset.persondays, N, all.persons, param.list, sedwidthdf.list))
  )  
}

mean.assignment.fn <- function(obs, subset.persondays, N, all.persons, param.list, sedwidthdf.list) {
  userday.combo = as.numeric(subset.persondays[obs,])
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  current.person = userday.combo[1]
  current.day = userday.combo[2]
  blockid = all.persons[all.persons[,1] == current.person,2]

  X.t = sampled.personday$sedentary.width
  
  A.t <- action.assignment(N, lambda, eta, X.t, buckets, 
                           current.person, current.day, 
                           blockid, param.list, sedwidthdf.list)

  return( 
    sum(c( A.t[1:min(136,length(A.t))], 
            rep(0,max(0,136-length(A.t)))))
  )  
}

cv.assignment.fn <- function(sampled.obs, all.persondays, all.Ns, param.list, sedwidthdf.list) {
  
  userday.combo = as.numeric(all.persondays[sampled.obs,])
  
  sampled.personday = window.time[window.time$user==userday.combo[1] & window.time$study.day==userday.combo[2],]
  
  X.t = sampled.personday$sedentary.width
  current.person = userday.combo[1]
  current.day = userday.combo[2]
  blockid = all.persons[all.persons[,1] == current.person,2]

  N = c(0,all.Ns[blockid])
  
  A.t <- action.assignment(N, lambda, eta, X.t, buckets, 
                           current.person, current.day, 
                           blockid, param.list, sedwidthdf.list)
  
  return( 
    c( A.t[1:min(136,length(A.t))], 
       rep(0,max(0,136-length(A.t))), 
       X.t[1:min(136,length(X.t))], 
       rep(0,max(0,136-length(X.t))) 
    )
  )  
}

allmodel.params <- function(seq.hour, all.persons) {
  # Fit models for each block 
  # and for each Hour
  num.blocks = max(all.persons[,2])
  param.list = list();
  param.list[[1]] = param.list[[2]] = param.list[[3]] = matrix(0, nrow = num.blocks, ncol = length(seq.hour))
  for (i in 1:num.blocks) {
    for (j in 1:length(seq.hour)) {
      current.hour = seq.hour[j]
      model.fit = fraction.time.in.state.user.re(current.hour, all.persons, i)  
      param.list[[1]][i,j] = model.fit$mean
      param.list[[2]][i,j] = model.fit$varcov[1]
      param.list[[3]][i,j] = model.fit$varcov[2]
    }
  }  
  return(param.list)
}

sedentarywidth.df <- function(current.hour, window.time) {
  
  if(current.hour > 24) {stop("Hour outside normal range")}
  if(current.hour < 3) {
    remaining.data = window.time[ (hour(window.time$window.utime) >= current.hour
                                    & hour(window.time$window.utime) < 3), ] 
  } else {
    remaining.data = window.time[ ( hour(window.time$window.utime) >= current.hour
                                      | hour(window.time$window.utime) < 3) , ] 
  }
  
  temp = aggregate(sedentary.width ~ user + study.day, 
                   data = remaining.data,
                   FUN = function(x) mean(x == TRUE))
  
  return(temp)
  
}

list.of.sedwidthdfs <- function(seq.hour, window.time) {
  sedwidthdf.list = list()
  for (i in 1:length(seq.hour)) {
    current.hour = seq.hour[i]
    sedwidthdf.list[[i]] = sedentarywidth.df(current.hour, window.time)
  }
  return(sedwidthdf.list)
}