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

#weekend vs weekday column 
window.time$weekend <- as.numeric(weekdays(window.time$window.utime - hours(8)) %in% c("Saturday", "Sunday"))

#setup column to be predicted as steps but with first week removed, and all values before wake up (first steps after 4am local time) set to NA including 40 minutes after wake up
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
      window.time$steps.predict[window.time$user == usernum & window.time$study.day == day & window.time$time < wakeup + 8] <- NA
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
  temp = log(window.time$steps + 5)
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
  temp = log(window.time$steps+5)
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

#setup x min previous steps column
previous <- function(width){
  temp = window.time$steps
  temp[is.na(temp)] = 0.0
  chosen.width = width 
  steps.width = rollapply(temp,FUN = mean, width = 1)
  column = c(rep(0,chosen.width), head(steps.width, -chosen.width))
  column[strftime(window.time$window.utime, format="%H:%M") == "08:00"] <- NA
  for (i in 0:(chosen.width - 1)){
    column[strftime(window.time$window.utime - minutes(5*i), format="%H:%M") == "08:00"] <- NA
  }
  column
}

window.time$steps.5min <- previous(1)
window.time$steps.10min <- previous(2)
window.time$steps.15min <- previous(3)
window.time$steps.20min <- previous(4)
window.time$steps.25min <- previous(5)
window.time$steps.30min <- previous(6)
window.time$steps.35min <- previous(7)
window.time$steps.40min <- previous(8)

#some other features
window.time$steps.5min.any <- as.numeric(window.time$steps.5min > 0)
window.time$steps.10min.any <- as.numeric(window.time$steps.10min > 0)
window.time$steps.15min.any <- as.numeric(window.time$steps.15min > 0)
window.time$steps.20min.any <- as.numeric(window.time$steps.20min > 0)
window.time$steps.25min.any <- as.numeric(window.time$steps.25min > 0)
window.time$steps.30min.any <- as.numeric(window.time$steps.30min > 0)
window.time$steps.35min.any <- as.numeric(window.time$steps.35min > 0)
window.time$steps.40min.any <- as.numeric(window.time$steps.40min > 0)
window.time$steps.week.any <- as.numeric(window.time$steps.week > 0)
window.time$steps.yesterday.any <- as.numeric(window.time$steps.yesterday > 0)
window.time$steps.yesterday.three <- as.numeric(window.time$steps.yesterday > 3)
window.time$steps.week.three <- as.numeric(window.time$steps.week > 3)

#split into test and train set.
users.test <- sample(users, 7)
test <- subset(window.time, window.time$user %in% users.test)
train <- subset(window.time, (!window.time$user %in% users.test))
users.train = unique(unlist(train$user))
users.valid <- sample(users, 6)

valid <- subset(train, train$user %in% users.valid)
train <- subset(train, (!train$user %in% users.valid))

#just the features we wish to use
train_min = subset(train, select=-c(user, window.utime, study.day, study.date, sedentary.width))


#large steps model
model1data = subset(train, select = c(steps.large, weekend, steps.yesterday, steps.5min, steps.10min, steps.15min, steps.20min, steps.25min, steps.30min, steps.35min, steps.40min, temp_mean, daily.precip_mean, steps.week))
model1data_min <- data.frame(lapply(model1data, function(x) scale(x)))

model1 <- lm(log(steps.large+5) ~ (weekend + steps.yesterday + log(steps.5min + 5) + log(steps.10min + 5) + log(steps.15min + 5) + log(steps.20min + 5) + log(steps.25min + 5) + log(steps.30min + 5) + log(steps.35min + 5) + log(steps.40min + 5) + temp_mean + daily.precip_mean + steps.week), data = model1data_min)
summary(model1)
plot(hist(train$steps.large))
hist(log(train$steps.large+5))
hist(log(train$steps.large+5))
hist(log(train$steps.yesterday+1))
plot(model1)


id <- 2
pred <-predict.lm(model1, subset(train, user == id &study.day == 8))

plot(log(subset(train, user == id &study.day == 8)$steps.large + 5), type="o")
points(pred, col = 2, type = "o")


#steps vs no steps model
model2data = subset(train, select = c(steps.any, steps.yesterday.three, steps.5min.any, steps.10min.any, steps.15min.any, steps.20min.any, steps.25min.any, steps.30min.any, steps.35min.any, steps.40min.any, temp_mean, daily.precip_mean, steps.week.three, weekend))
model2data$temp_mean = scale(model2data$temp_mean)
model2data$daily.precip_mean = scale(model2data$daily.precip_mean)
model2 <- glm(steps.any ~ .^2,family=binomial(link='logit'),data=model2data)
summary(model2)
library(pscl)
pR2(model2)

#graph larger steps vs time 
lo <- loess(model1data$steps.large~model1data$time)
plot(model1data$steps.large ~ model1data$time)
lines(predict(lo), col='red', lwd=2)
