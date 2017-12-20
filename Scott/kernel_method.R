library(ggplot2)
library(zoo)
require(lubridate)
setwd("/Volumes/dav/HeartSteps/Scott/")
window.time = read.csv("minute_data.csv")
window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") 


set.seed(300)
window.time <- subset(window.time, select = -c(daily.precip_mean, temp_mean, study.date))
window.time$steps[strftime(window.time$window.utime, format="%H:%M") == "08:00"] <- NA
attach(window.time)
window.time <- window.time[order(user, window.utime), ]

users = unique(unlist(window.time$user))

#all values before wake up (first time steps taken after 8am GMT time) set to unsedentary (0) 
window.time$time <- NA
for (usernum in users){
  start.utime = window.time$window.utime[window.time$user == usernum & strftime(window.time$window.utime, format="%H:%M") == "08:00" & window.time$study.day == 0]
  
  unique.days = unique(unlist(window.time$study.day[window.time$user == usernum]))
  for (day in unique.days){
    window.time$time[window.time$user == usernum & window.time$study.day == day] <- 1:(length(window.time$time[window.time$user == usernum & window.time$study.day == day]))
    #setup time remaining too
    window.time$time_r[window.time$user == usernum & window.time$study.day == day] <- 1 + length(window.time$time[window.time$user == usernum & window.time$study.day == day]) - window.time$time[window.time$user == usernum & window.time$study.day == day]
    wakeup <- which(window.time$steps[window.time$user == usernum & window.time$study.day == day]>0)
    if (length(wakeup) > 0){
      wakeup <- min(wakeup)
      window.time$sedentary.width[window.time$user == usernum & window.time$study.day == day & window.time$time < wakeup] <- 0
    }
  }  
}
window.time <- subset(window.time, select = -c(steps))

#initialize number of remaining sedentary events in day and run lengths
window.time$sedentary.remaining <- NA
window.time$run <- NA
for (usernum in users){
  unique.days = unique(unlist(window.time$study.day[window.time$user == usernum]))
  for (day in unique.days){
    temp = window.time$sedentary.width[window.time$user == usernum & window.time$study.day == day]
    
    sedentary_total = sum(temp)
    window.time$sedentary.remaining[window.time$user == usernum & window.time$study.day == day] = sedentary_total - cumsum(temp) + 1
    window.time$run[window.time$user == usernum & window.time$study.day == day] = sequence(rle(temp)$lengths)
  }
}



window.time = subset(window.time, window.time$run < 21)
#only working with sedentary events now
window.time = subset(window.time, window.time$sedentary.width == 1)
window.time <- subset(window.time, select = -c(window.utime, time))


#split into test and train set.
users.test <- sample(users, 7)
test <- subset(window.time, window.time$user %in% users.test)
train <- subset(window.time, (!window.time$user %in% users.test))

#split train into train and validation
users.train = unique(unlist(train$user))
users.valid <- sample(users, 6)
valid <- subset(train, train$user %in% users.valid)
train <- subset(train, (!train$user %in% users.valid))


time_r.max = max(train$time_r)
run.max = max(train$run)

#make matrix of training values
Y = rep(0, time_r.max * run.max)

#make weights matrix
W = matrix(0L, nrow = time_r.max * run.max, ncol = time_r.max * run.max)
#in the vector we do all of a given time then move on
for (current_time in 1:time_r.max){
  for(current_run in 1:run.max){
    temp = train$sedentary.remaining[train$run == current_run & train$time_r == current_time]
    index = (current_time - 1)*run.max + current_run
    if (length(temp) != 0){
      Y[index] = mean(temp) / current_time
    }
    W[index, index] = length(temp)
  }
}
Y = Y - mean(Y)
W <- diag(1, length(Y))
Y.mat <- matrix(Y, nrow = 216, ncol = 20, byrow=TRUE)

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 -x1)^2 + (y2-y1)^2)
}

#setup basis function
gamma <- sqrt(10)
rbf <- function(r) {
  exp(-r^2/gamma^2)
}

#setup the kernel matrix
kern = matrix(0L, nrow = time_r.max * run.max, ncol = time_r.max * run.max)
for (current_time1 in 1:time_r.max){
  for(current_run1 in 1:run.max){
    index1 = (current_time1 - 1)*run.max + current_run1
    
    for (current_time2 in current_time1:time_r.max){
      for(current_run2 in current_run1:run.max){
        index2 = (current_time2 - 1)*run.max + current_run2;
        rr <- distance(current_time1, current_run1, current_time2, current_run2)
        tmp <- rbf(rr)
        kern[index1, index2] = tmp
        #print(tmp)
      }
    }
  }
}
kern = t(kern) + kern - diag(diag(kern))

identity = diag(time_r.max * run.max)

lambda = 4.5

#coefs = solve(W %*% kern + lambda * identity) %*% W %*% Y
coefs = solve(W %*% kern + lambda * identity, W %*% Y)

pred <- function(t, k) {
  output = 0
  output = output + kern[(t - 1)*run.max + k, ] %*% coefs
}

x <- kern %*% coefs
mean((Y - x)^2)
plot(Y, col=2, type = "l")
points(x, type = "l")



x <- sapply(1:216, function(x) pred(x, k=1))

plot(Y.mat[, 1], col=2, type = "l")
points(x, type = "l")

points(x = Y[(t - 1)*run.max + 1], y = train$sedentary.remaining[train$run == 1], col = 2)
#calculate batch loss
loss = sum((train$sedentary.remaining - sapply(pred, train$time_r, train$run))^2) 
  
