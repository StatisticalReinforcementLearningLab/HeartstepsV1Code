library(ggplot2)
source("ema_functions.R")

gitloc <- switch(Sys.info()["sysname"],
                  "Windows" = list(gitloc = "C:/Users/wdem/Documents/GitHub/heartstepsdata/"),
                  "Darwin" = list(gitloc = "/Users/walterdempsey/Documents/github/heartstepsdata"))
setwd(gitloc$gitloc)

## Make sure to mount the HeartSteps Box Folder now.

## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)
source("init.R")
setwd(sys.var$mbox.data)
load("csv.RData")

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


## add *intended* EMA notification time
## nb: take latest time if user updated the EMA slot repeatedly in one day
temp <- subset(timeslot, time.updated.hour + time.updated.min / 60 < ema.hours)
temp <- subset(temp, !duplicated(cbind(user, date.updated, ema.hours)))
temp <- temp[with(temp, order(user, date.updated, -as.numeric(utime.updated))), ]
temp <- subset(temp, !duplicated(cbind(user, date.updated)),
               select = c(user, date.updated, ema.hours))
daily <- merge.last(daily, temp,
                    id = "user", var.x = "study.date", var.y = "date.updated")
daily$ema.utime <- with(daily, as.POSIXct(study.date) + ema.hours * 60^2)
daily <-
  merge.last(daily,
             with(timezone,
                  data.frame(user, ltime, ema.tz = tz, ema.gmtoff = gmtoff)),
             id = "user", var.x = "ema.utime", var.y = "ltime")
any(is.na(daily$ema.gmtoff))
## adjust EMA time for inferred time zone
daily$ema.utime <- with(daily, ema.utime - ema.gmtoff)

## add context at EMA notification or (if unavailable) at earliest engagement
any(with(notify, duplicated(cbind(user, ema.date))))
names(notify) <- gsub("^notified\\.(|time.)", "context.", names(notify))
names(engage) <- gsub("^engaged\\.(|time.)", "context.", names(engage))
notify$notify <- TRUE
temp <- merge(notify, engage, all = TRUE)
temp <- temp[with(temp, order(user, is.na(notify), ema.date, context.utime)), ]
temp <- subset(temp, !duplicated(cbind(user, ema.date)))
daily <- merge(daily,
               subset(temp,
                      select = c(user,
                                 tz, gmtoff, ema.date,
                                 notify, context.date, context.utime,
                                 context.year:context.sec,
                                 planning.today, recognized.activity,
                                 front.end.application, calendar,
                                 gps.coordinate, home, work, city,
                                 location.exact, location.category,
                                 weather.condition, temperature, windspeed,
                                 precipitation.chance, snow)),
               by.x = c("user", "study.date"), by.y = c("user", "ema.date"),
               all.x = TRUE)
## adjust EMA time to that of context, when available
temp <- !is.na(daily$context.utime)
daily$ema.utime[temp] <- daily$context.utime[temp]

## add EMA response
any(with(ema, duplicated(cbind(user, ema.date, order))))
daily <- merge(daily,
               aggregate(subset(ema, select = c(ema.set.length, hectic:urge)),
                         by = with(ema, list(user, ema.date)),
                         function(x) na.omit(x)[1]),
               by.x = c("user", "study.date"),
               by.y = paste("Group", 1:2, sep = "."), all.x = TRUE)

## any EMAs erroneously represented as missing?
setNames(sapply(with(users, user[!exclude]),
                function(u)
                  any(with(daily, study.date[user == u & is.na(ema.set.length)])
                      %in% with(ema, ema.date[user == u]))),
         with(users, user[!exclude]))

## add indicator of EMA engagement
daily <- merge(daily,
               aggregate(interaction.count ~ user + ema.date, sum, data = engage),
               by.x = c("user", "study.date"), by.y = c("user", "ema.date"),
               all.x = TRUE)

## add planning response
any(with(plan, duplicated(cbind(user, ema.date))))
daily <- merge(daily,
               with(plan, aggregate(cbind(planning, response),
                                    list(user = user, study.date = ema.date),
                                    function(x) x[1])),
               by.x = c("user", "study.date"), all.x = TRUE)

## add last time the device was used
daily <- merge.last(daily,
                    with(tracker, data.frame(user, start.utime,
                                             device.utime = start.utime)),
                    id = "user", var.x = "ema.utime", var.y = "start.utime")
daily$device.since <-
  with(daily, difftime(ema.utime, device.utime, units = "hours"))

## responded to planning or at least one EMA question?
daily$respond <- with(daily, !is.na(planning) | !is.na(ema.set.length))

## viewed the planning/EMA?
## FIXME: verify interpretation
daily$view <- with(daily, !is.na(interaction.count) | respond)

## had active connection at "time" of EMA?
daily$notify <- !is.na(daily$notify)
daily$connect <- with(daily, notify | view | respond)

## revise planning status
daily$planning.today[!daily$connect] <-
  daily$planning[!daily$connect] <- "disconnected"
daily$planning[with(daily, respond & is.na(planning))] <- "no_planning"

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
  return(c(make_datetime(year = year(date), month = month(date),
                          day = day(date),
                          hour = rep(seq(14,24),each = length(seq(0,55,5))), min = seq(0,55,5),
                          sec = 0, tz = "GMT"),
            make_datetime(year = year(date), month = month(date),
                          day = day(date)+1,
                          hour = rep(1,each = length(seq(0,55,5))), min = seq(0,55,5),
                          sec = 0, tz = "GMT")))
}

temp = lapply(used.daily$study.date,time.fn)

dates = as.POSIXct(unlist(temp), origin = "1970-01-01", tz = "GMT")    # in UTC

repeat.times = length(dates)/length(used.daily$user)

window.time = data.frame(rep(used.daily$user,each = repeat.times),
      dates,
      rep(used.daily$study.day, each = repeat.times))

names(window.time) = c("user", "window.utime", "study.day")

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

chosen.width = 8 # 5*8 = 40 minutes
sedentary.width = rollapply(temp,FUN = sum, width = chosen.width)
window.time$sedentary.width = c(sedentary.width < 150,rep(0,chosen.width-1))

# window.time$sedentary = is.na(window.time$steps)
# 
# chosen.width = 8 # 5*8 = 40 minutes
# sedentary.width = rollapply(window.time$sedentary,FUN = prod, width = chosen.width)
# 
# window.time$sedentary.width = c(sedentary.width,rep(0,chosen.width-1))

hour.toss = hour(window.time$window.utime)
minute.toss = minute(window.time$window.utime)

temp.persondays = data.frame(cbind(window.time$user, day(window.time$window.utime)))
names(temp.persondays) = c("user","day")

unique.persondays = aggregate(numeric(nrow(temp.persondays)), temp.persondays[c("user", "day")], length)

nrow(unique.persondays)

window.time = window.time[-which(hour.toss == 1 & minute.toss >= 20),]

## Buckets are defined by 14 - 17, 
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)

obs.bucket1 = (hour(window.time$window.utime) >= bucket1[1]) & (hour(window.time$window.utime) <= bucket1[2])
obs.bucket2 = (hour(window.time$window.utime) >= bucket2[1]) & (hour(window.time$window.utime) <= bucket2[2])
obs.bucket3 = (hour(window.time$window.utime) >= bucket3[1]) | (hour(window.time$window.utime) <= bucket3[2])

# Compute the number of risk times within each user-day in each bucket
k1 = aggregate(sedentary.width~user + study.day, data = window.time[obs.bucket1,], sum)
k2 = aggregate(sedentary.width~user + study.day, data = window.time[obs.bucket2,], sum)
k3 = aggregate(sedentary.width~user + study.day, data = window.time[obs.bucket3,], sum)

kbar = c( mean(k1$sedentary.width), mean(k2$sedentary.width), mean(k3$sedentary.width))

probzero.bar = c(mean(k1$sedentary.width>0), mean(k2$sedentary.width>0),mean(k3$sedentary.width>0))

Nbar = probzero.bar/sum(probzero.bar) * 1.5

# fracsed.bucket1 = mean(window.time$sedentary.width[obs.bucket1])
# fracsed.bucket2 = mean(window.time$sedentary.width[obs.bucket2])
# fracsed.bucket3 = mean(window.time$sedentary.width[obs.bucket3])

# prob.bucket1 = (1.5/3)/(12*4*fracsed.bucket1)
# prob.bucket2 = (1.5/3)/(12*4*fracsed.bucket2)
# prob.bucket3 = (1.5/3)/(12*4*fracsed.bucket3)
# 
# output = c(prob.bucket1,prob.bucket2,prob.bucket3)

output = Nbar/kbar

setwd("/Users/walterdempsey/Documents/github/heartstepsdata/Walter")
saveRDS(object = output, file = "ema_output")

## ANOVA DECOMPOSITION
which.bucket <- function(hour) {
  if ( (hour >= bucket1[1] & hour <= bucket1[2] ) ) {
    return(1)
  } else if ( (hour >= bucket2[1] & hour <= bucket2[2] ) ) {
    return(2)
  } else if ( (hour >= bucket3[1] | hour <= bucket3[2] ) ) {
    return(3)
  }
}
window.time$hour = hour(window.time$window.utime)
window.time$day = day(window.time$window.utime)
window.time$bucket = unlist(lapply(window.time$hour,FUN = which.bucket))

fulldata.anova = aggregate(sedentary.width ~ user + day + bucket, data = window.time, sum)

m1 <- aov(sedentary.width ~ as.factor(user) * as.factor(day) * as.factor(bucket), data = fulldata.anova)

summary(m1)

### CHECK LATER
removeuser.anova = aggregate(sedentary.width ~ day + bucket, data = window.time, mean)

removeday.anova = aggregate(sedentary.width ~ bucket, data = window.time, mean)

removehour.anova = aggregate(sedentary.width ~ 1, data = window.time, mean)

# ANOVA
num.obs = dim(fulldata.anova)[1]
total = 0; mean.total = 0
D = length(unique(window.time$day))
N = length(unique(window.time$user))
K = 3

for( i in 1:num.obs) {
  obs = which((removeuser.anova[,1] == fulldata.anova[i,2]) & (removeuser.anova[,2] == fulldata.anova[i,3]))
  total = total + (fulldata.anova[i,4] - removeuser.anova[obs,3])^2
  mean.total = mean.total + (fulldata.anova[i,4] - removehour.anova)^2
}

total
mean.total


num.obs.1 = dim(removeuser.anova)[1]
total.1 = 0
for( i in 1:num.obs.1) {
  # n.i = sum( (fulldata.anova[,2] == removeuser.anova[i,1]) & (fulldata.anova[,3] == removeuser.anova[i,2]) )
  n.i = N
  obs = which((removeday.anova[,1] == removeuser.anova[i,2]))
  total.1 = total.1 + n.i*(removeuser.anova[i,3] - removeday.anova[obs,2])^2
}

total.1

num.obs.2 = dim(removeday.anova)[1]
total.2 = 0
for( i in 1:num.obs.2) {
  # n.i = sum( (fulldata.anova[,3] == removeday.anova[i,1]))
  n.i = N*D
  total.2 = total.2 + n.i*(removeday.anova[i,2] - removehour.anova)^2
}

total.2

# D.F.
df.meantotal  = D*N*3 - 1
df.total = 3*D*(N-1)
df.total.1 = 3*D-1
df.total.2 = 3-1


SS = c(total, total.1, total.2, mean.total)
DF = c( df.total,df.total.1, df.total.2, df.meantotal)
MS = SS/DF

cbind(DF,SS, MS)
