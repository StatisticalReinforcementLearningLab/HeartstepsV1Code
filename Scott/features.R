library(ggplot2)
library(zoo)
require(lubridate)
setwd("/Volumes/dav/HeartSteps/Scott/")
window.time = read.csv("minute_data.csv")
window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") 


set.seed(300)
window.time <- subset(window.time, !(is.na(window.time$temp_mean) | is.na(window.time$daily.precip_mean)))
attach(window.time)
window.time <- window.time[order(user, window.utime), ]
users = unique(unlist(window.time$user))


#set NAs to 0 and remove rolled over values
window.time$steps[strftime(window.time$window.utime, format="%H:%M") == "08:00"] <- NA
window.time$steps[is.na(window.time$steps)] <- 0

#setup column to be predicted as steps but with first week removed, and all values before wake up (first steps after 4am loal time) set to NA
window.time$steps.predict <- window.time$steps
window.time$time <- NA
for (usernum in users){
  start.utime = window.time$window.utime[window.time$user == usernum & strftime(window.time$window.utime, format="%H:%M") == "08:00" & window.time$study.day == 0]
  window.time$steps.predict[window.time$user == usernum & window.time$window.utime < start.utime + days(7)] <- NA
  
  unique.days = unique(unlist(window.time$study.day[window.time$user == usernum]))
  for (day in unique.days){
    window.time$time[window.time$user == usernum & window.time$study.day == day] <- 1:(length(window.time$time[window.time$user == usernum & window.time$study.day == day]))
    wakeup <- which(window.time$steps.predict[window.time$user == usernum & window.time$study.day == day]>0)
    if (length(wakeup) > 0){
      wakeup <- min(wakeup)
      window.time$steps.predict[window.time$user == usernum & window.time$study.day == day & window.time$time < wakeup] <- NA
    }
  }  
}

#setup steps.catagories columns (binary for steps/nosteps, N/A and value for less than 10, N/A and value for over 20)
window.time$steps.any <- as.numeric(window.time$steps.predict > 0)

window.time$steps.small <- window.time$steps.predict
window.time$steps.small[window.time$steps.predict == 0 | window.time$steps.predict > 10] <- NA

window.time$steps.large <- window.time$steps.predict
window.time$steps.large[window.time$steps.predict < 11] <- NA


day.length = length(window.time$steps[user == 1 & study.day==0])
#setup yesterday steps column
#if not available for yesterday, just takes the day before 
yesterday <- function(width){
  temp = window.time$steps
  temp[is.na(temp)] = 0.0
  window.time$steps.yesterday <- NA
  chosen.width = width
  steps.width = rollapply(temp,FUN = mean, width = chosen.width)
  window.time$steps.yesterday = c(rep(0,chosen.width/2 + day.length), head(steps.width, -(chosen.width/2 + day.length - chosen.width + 1)))
  for (i in 0:round(chosen.width/2)){
    window.time$steps.yesterday[strftime(window.time$window.utime - minutes(5*i), format="%H:%M") == "08:00"] <- NA
    window.time$steps.yesterday[strftime(window.time$window.utime + minutes(5*i), format="%H:%M") == "04:55"] <- NA
  }
  window.time$steps.yesterday[study.day < 1] = NA
  window.time
}

window.time <- yesterday(12) # 1 hour

#setup weeks steps column
#first setup hour band column
#if not available for last week, just set as NA
hourband <- function(width){
  temp = window.time$steps
  temp[is.na(temp)] = 0.0
  window.time$steps.hourband <- NA
  chosen.width = width
  hoursteps.width = rollapply(temp,FUN = mean, width = chosen.width)
  window.time$steps.hourband = c(rep(0,chosen.width/2), head(hoursteps.width, -1), rep(0,chosen.width/2))
  for (i in 0:round(chosen.width/2)){
    window.time$steps.hourband[strftime(window.time$window.utime - minutes(5*i), format="%H:%M") == "08:00"] <- NA
    window.time$steps.hourband[strftime(window.time$window.utime + minutes(5*i), format="%H:%M") == "04:55"] <- NA
  }
  window.time
}

window.time <- hourband(12)

window.time$steps.week <- NA
for (usernum in users){
  unique.days = unique(unlist(window.time$study.day[window.time$user == usernum]))
  for (day in unique.days){
    if ((day-7) %in% unique.days){
      window.time$steps.week[window.time$user == usernum & window.time$study.day == day] = window.time$steps.hourband[window.time$user == usernum & window.time$study.day == (day-7)]
    } else{
      window.time$steps.week[window.time$user == usernum & window.time$study.day == day] <- NA
    }
  }
}

#setup 5 min previous steps column
temp = window.time$steps
temp[is.na(temp)] = 0.0
window.time$steps.5min <- NA
chosen.width = 1 
steps.width = rollapply(temp,FUN = mean, width = chosen.width)
window.time$steps.5min = c(rep(0,chosen.width), head(steps.width, -1))
window.time$steps.5min[strftime(window.time$window.utime, format="%H:%M") == "08:00"] <- NA

#some other features
window.time$steps.5min.any <- as.numeric(window.time$steps.5min > 0)
window.time$steps.5min.any <- as.numeric(window.time$steps.5min > 0)
window.time$steps.week.any <- as.numeric(window.time$steps.week > 0)
window.time$steps.yesterday.any <- as.numeric(window.time$steps.yesterday > 0)
window.time$steps.yesterday.fifty <- as.numeric(window.time$steps.yesterday > 50)
window.time$steps.week.fifty <- as.numeric(window.time$steps.week > 50)

#split into test and train set.
users.test <- sample(users, 7)
test <- subset(window.time, window.time$user %in% users.test)
train <- subset(window.time, (!window.time$user %in% users.test))
users.train = unique(unlist(train$user))
users.valid <- sample(users, 6)

valid <- subset(train, train$user %in% users.valid)
train <- subset(train, (!train$user %in% users.valid))

model1 <- lm(log(train$steps.large+5) ~ (train$steps.yesterday + train$steps.5min + train$temp_mean + train$daily.precip_mean + train$steps.week))
summary(model1)
