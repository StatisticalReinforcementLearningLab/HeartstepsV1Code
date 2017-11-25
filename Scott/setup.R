library(ggplot2)
source("functions.R")

gitloc <- switch(Sys.info()["sysname"],
                  "Windows" = list(gitloc = "C:/Users/wdem/Documents/GitHub/heartstepsdata/"),
                  "Darwin" = list(gitloc = "/Users/walterdempsey/Documents/github/heartstepsdata"))
setwd(gitloc$gitloc)

## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)
setwd("~/Documents/heartstepsdata-master")
source("init.R")

setwd(sys.var$mbox.data)
load("csv.RData")

#bring in a second daily that contains weather info
setwd("/Volumes/dav/HeartSteps/Walter/daily")
daily_v2 <- gzfile("dfdaily_v2.rds")
dailytwo <- readRDS(daily_v2)
close(daily_v2)

setwd("/Volumes/dav/HeartSteps/Scott")
## last data update
max.date <- as.Date("2016-02-19")

## target study days for each user
max.day <- 42

## hour threshold for which we can presume that the device is actively being used
max.device.since <- 1

##### User data #####

## slot of update time, last notification


## infer intake date-time from first selection of notification time slots
users <- with(subset(timeslot[with(timeslot, order(user, utime.updated)), ],
                     !duplicated(user)),
              data.frame(user,
                         intake.date = date.updated,
                         intake.utime = utime.updated,
                         intake.tz = tz,
                         intake.gmtoff = gmtoff,
                         intake.hour = time.updated.hour,
                         intake.min = time.updated.min,
                         intake.slot = slot.updated))

## Add travel, exit, and dropout dates
temp0 <- with(participants,
              data.frame(user,
                         travel.start,
                         travel.end,
                         exit.date = exit.interview.date,
                         dropout.date))

## infer exit date-time from last notification
## NOTE: This does not necessarily work; some participants still had
## app installed post-exit
temp <- rbind(with(decision,
                   data.frame(user,
                              last.date = date.stamp,
                              last.utime = utime.stamp,
                              last.tz = tz,
                              last.gmtoff = gmtoff,
                              last.hour = time.stamp.hour,
                              last.min = time.stamp.min,
                              last.slot = slot)),
              with(notify,
                   data.frame(user,
                              last.date = notified.date,
                              last.utime = notified.utime,
                              last.tz = tz,
                              last.gmtoff = gmtoff,
                              last.slot = length(slots) - 1,
                              last.hour = notified.time.hour,
                              last.min = notified.time.min)))
temp <- temp[with(temp, order(user, -as.numeric(last.utime))), ]
users <- merge(users, temp0, by = "user", all = TRUE)
users <- merge(users, subset(temp, !duplicated(user)), by = "user", all = TRUE)
rm(temp0)

## Update last.date to be the actual last day on study
# Compare last.date computed above using notification times to dropout date
users$last.date <-
  with(users,
       as.Date(sapply(1:length(unique(user)),
                      function(x) min(last.date[x],
                                      min(exit.date[x], dropout.date[x], na.rm = T),
                                      na.rm = T))))

## odd user id implies that HeartSteps is installed on own phone
# NB: NOT ALWAYS THE CASE
users$own.phone <- users$user %% 2 != 0

## device locale is (most likely) US English
users <- merge(users, aggregate(en.locale ~ user, all, data = timezone),
               by = "user", all.x = TRUE)

## add intake and exit interview data
users <- merge(users,
               merge(intake, exit, by = c("user", "userid"), all = TRUE,
                     suffixes = c(".intake", ".exit")),
               by = "user", all.x = TRUE)

## indicate users that just enrolled or dropped out,
## don't have their locale set to English
users$days <- with(users, as.numeric(difftime(last.date, intake.date, "days")))
users$exclude <- with(users, !en.locale | intake.date >= max.date | days < 13 |
                        (intake.date + 42 < max.date & days < 10))
users$user.index <- cumsum(!users$exclude)
users$user.index[users$exclude] <- NA

##### Daily data #####

## evaluate user-date combinations, by generating a sequence of dates
## from intake to the earliest of exit date and 'max.date'
temp <- do.call("rbind",
                with(subset(users, !exclude),
                     mapply(function(u, x, y, ...) data.frame(u, seq(x, y, ...)),
                            u = user, x = intake.date,
                            y = pmin(last.date, max.date),
                            by = "days", SIMPLIFY = FALSE)))

## expand user level data to user-date level
daily <- data.frame(users[match(temp[, 1], users$user), ],
                    study.date = temp[, 2])
daily <- subset(daily, select = c(user, user.index, intake.date:last.slot,
                                  study.date, own.phone))

## Add indicator for travel time
daily$travel <- with(daily, study.date >= travel.start & study.date <= travel.end)
daily$travel[is.na(daily$travel)] <- FALSE

## add index day on study, starting from zero
daily$study.day <-
  with(daily, as.numeric(difftime(study.date, intake.date, units = "days")))

## Modify index to exclude travel time
daily$study.day.nogap <- with(daily, ifelse(travel, NA, study.day))
daily$study.day.nogap[!daily$travel] <-
  unlist(sapply(unique(daily$user),
                function(x) {
                  r <- with(daily, rle(travel[user == x]))
                  if (length(r$lengths) > 1) {
                    with(daily[daily$user == x & !daily$travel, ],
                         c(study.day[1:r$lengths[1]], seq(sum(r$lengths[1:2]) + 1, sum(r$lengths))
                           - r$lengths[2] - 1))
                  }
                  else daily$study.day[daily$user == x]
                },
                simplify = T))




## add daily step counts,
## aligned with the days (00:00 - 23:59) in the *intake* time zone
## FIXME: align step counts with timing of EMA?
jawbone <- merge(jawbone, subset(users, select = user:last.min),
                 by = "user", suffixes = c("", ".user"))
jawbone$end.date <-
  do.call("c", with(jawbone, mapply(as.Date, x = end.utime, tz = intake.tz,
                                    SIMPLIFY = FALSE)))
jawbone <- jawbone[with(jawbone, order(user, end.utime)), ]

daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = jawbone, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "jbsteps"

googlefit <- merge(googlefit, subset(users, select = user:last.min),
                   by = "user", suffixes = c("", ".user"))
googlefit$end.date <-
  do.call("c", with(googlefit, mapply(as.Date, x = end.utime, tz = intake.tz,
                                      SIMPLIFY = FALSE)))
googlefit <- googlefit[with(googlefit, order(user, end.utime)), ]

daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = googlefit, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "gfsteps"

daily <- daily[with(daily, order(user, study.day)), ]

## Only use user-days with travel == FALSE and
## Step count is not zero

used.daily = daily[daily$travel== FALSE & !is.na(daily$jbsteps),]

## construct a sequence of times in a day for a user-day pair
## Will have min max hour, and
require(lubridate)

time.fn <- function(date) {
  #if end of month but not year
  if ((day(date) == 31 & month(date) %in% c(7, 8, 10, 1)) | (day(date) == 30 & month(date) %in% c(6, 9, 11))){
    return(c(make_datetime(year = year(date), month = month(date),
                           day = day(date),
                           hour = rep(seq(8,23),each = length(seq(0,55,5))), min = seq(0,55,5),
                           sec = 0, tz = "GMT"),
             make_datetime(year = year(date), month = month(date) + 1,
                           day = 1,
                           hour = rep(seq(0,4),each = length(seq(0,55,5))), min = seq(0,55,5),
                           sec = 0, tz = "GMT")))
  } 
  else if (day(date) == 31 & month(date) == 12){
    #if it's the end of the year
    return(c(make_datetime(year = year(date), month = month(date),
                           day = day(date),
                           hour = rep(seq(8,23),each = length(seq(0,55,5))), min = seq(0,55,5),
                           sec = 0, tz = "GMT"),
             make_datetime(year = year(date) + 1, month = 1,
                           day = 1,
                           hour = rep(seq(0,4),each = length(seq(0,55,5))), min = seq(0,55,5),
                           sec = 0, tz = "GMT")))
  } else{
    #if normal day
    return(c(make_datetime(year = year(date), month = month(date),
                            day = day(date),
                            hour = rep(seq(8,23),each = length(seq(0,55,5))), min = seq(0,55,5),
                            sec = 0, tz = "GMT"),
              make_datetime(year = year(date), month = month(date),
                            day = day(date)+1,
                            hour = rep(seq(0,4),each = length(seq(0,55,5))), min = seq(0,55,5),
                            sec = 0, tz = "GMT")))
  }
}

temp = lapply(used.daily$study.date,time.fn)

dates = as.POSIXct(unlist(temp), origin = "1970-01-01", tz = "GMT")    # in UTC

repeat.times = length(dates)/length(used.daily$user)

window.time = data.frame(rep(used.daily$user,each = repeat.times),
      dates,
      rep(used.daily$study.day, each = repeat.times),
      rep(used.daily$study.date, each = repeat.times))

names(window.time) = c("user", "window.utime", "study.day", "study.date")

window.time = window.time[!is.na(window.time$window.utime),]

## Merge.first

jbprefive <- merge.first(subset(jawbone,
                                end.utime >= intake.utime & end.utime <= last.utime),
                         subset(window.time,
                                select = c(user, window.utime, study.day)),
                         id = "user", var.x = "end.utime", var.y = "window.utime")

jbprefive <- jbprefive[with(jbprefive, order(user, end.utime)), ]

window.time <- merge(window.time,
               aggregate(steps ~ user + window.utime, data = jbprefive, sum),
               by.x = c("user", "window.utime"), by.y = c("user", "window.utime"),
               all.x = TRUE)

# Sedentary = 40 minutes of < 150 steps

temp = window.time$steps
temp[is.na(temp)] = 0.0

#from day start you will always need to set first 7 values after wake up to 0s (not sedentary)
chosen.width = 8 # 5*8 = 40 minutes
sedentary.width = rollapply(temp,FUN = sum, width = chosen.width)
window.time$sedentary.width = c(rep(1,chosen.width-1), sedentary.width < 150)

window.time <- merge(window.time, 
               subset(dailytwo, select = c(user, study.date, temp_mean, daily.precip_mean)),
               by.x=c("user", "study.date"), by.y =c("user", "study.date"), 
               all.x =TRUE)

# window.time$sedentary = is.na(window.time$steps)
# 
# chosen.width = 8 # 5*8 = 40 minutes
# sedentary.width = rollapply(window.time$sedentary,FUN = prod, width = chosen.width)
# 
# window.time$sedentary.width = c(sedentary.width,rep(0,chosen.width-1))

write.table(window.time, "/Volumes/dav/HeartSteps/Scott/minute_data.csv", sep = ",")


