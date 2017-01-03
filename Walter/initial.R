source("functions.R")

gitloc <- switch(Sys.info()["sysname"],
                  "Windows" = list(gitloc = "C:/Users/wdem/Documents/GitHub/heartstepsdata/"),
                  "Darwin" = list(gitloc = "/Users/walterdempsey/Documents/github/heartstepsdata"))
setwd(gitloc$gitloc)

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

window.time$sedentary = is.na(window.time$steps)

chosen.width = 8 # 5*8 = 40 minutes
sedentary.width = rollapply(window.time$sedentary,FUN = prod, width = chosen.width)

window.time$sedentary.width = c(sedentary.width,rep(0,chosen.width-1))

hour.toss = hour(window.time$window.utime)
minute.toss = minute(window.time$window.utime)

window.time = window.time[-which(hour.toss == 1 & minute.toss >= 20),]

temp.values = aggregate(sedentary.width~ user + study.day, data = window.time, FUN = function(x) c(rle(x)$values))
temp.length = aggregate(sedentary.width~ user + study.day, data = window.time, FUN = function(x) c(rle(x)$length))

Sedentary.values = as.logical(unlist(temp.values[,3]))
Sedentary.length = as.numeric(unlist(temp.length[,3]))

write.table(window.time, "/Users/walterdempsey/Dropbox/MRTs/WD & SAM work/Heartsteps - randomization probability/code/window_time.csv", sep = ",")
write.table(Sedentary.values, "/Users/walterdempsey/Dropbox/MRTs/WD & SAM work/Heartsteps - randomization probability/code/sed_values.csv", sep = ",")
write.table(Sedentary.length, "/Users/walterdempsey/Dropbox/MRTs/WD & SAM work/Heartsteps - randomization probability/code/sed_length.csv", sep = ",")
### Initial summary

#sedentary.runs = rle(window.time$sedentary.thirty)

par(mar = c(5,3,1,1)+0.1, mfrow = c(2,1))
hist(Sedentary.length[Sedentary.values == 0 & Sedentary.length < 100],
     breaks = 75, xlab = "Length of sequential classification to Not Sedentary", main = "")

hist(Sedentary.length[Sedentary.values == 1 & Sedentary.length < 100],
     breaks = 75, xlab = "Length of sequential classification to Sedentary", main = "")

### Check for difference between "Early Study" (Day < 21) and "Late Study" (Day >= 21)

temp.values.early = aggregate(sedentary.width~ user + study.day, 
                              data = window.time[window.time$study.day < 21,], 
                              FUN = function(x) c(rle(x)$values))
temp.length.early = aggregate(sedentary.width~ user + study.day, 
                              data = window.time[window.time$study.day < 21,], 
                              FUN = function(x) c(rle(x)$length))

Sedentary.values.early = as.logical(unlist(temp.values.early[,3]))
Sedentary.length.early = as.numeric(unlist(temp.length.early[,3]))

temp.values.late = aggregate(sedentary.width~ user + study.day, 
                              data = window.time[window.time$study.day >= 21,], 
                              FUN = function(x) c(rle(x)$values))
temp.length.late = aggregate(sedentary.width~ user + study.day, 
                              data = window.time[window.time$study.day >= 21,], 
                              FUN = function(x) c(rle(x)$length))

Sedentary.values.late = as.logical(unlist(temp.values.late[,3]))
Sedentary.length.late = as.numeric(unlist(temp.length.late[,3]))


# early.sedentary.runs = rle(window.time$sedentary.thirty[window.time$study.day< 21])
# late.sedentary.runs = rle(window.time$sedentary.thirty[window.time$study.day >= 21])

hist(Sedentary.length.early[Sedentary.values.early == 1 & Sedentary.length.early < 100], 
     breaks = 75, xlab = "Length of sequential classification to Sedentary for Early Study", main = "")

hist(Sedentary.length.late[Sedentary.values.late == 1 & Sedentary.length.late < 100], 
     breaks = 75, xlab = "Length of sequential classification to Sedentary for Late Study", main = "")


### Check for difference between "First Half of Day" (14 < Hour < 20) 
### and "Second Half of Day" (20 <= Hour <= 24 or 0 <= Hour < 2)
temp.values.firsthalf = aggregate(sedentary.width~ user + study.day, 
                                  data = window.time[14 < hour(window.time$window.utime) 
                                                     & hour(window.time$window.utime) < 20,], 
                                  FUN = function(x) c(rle(x)$values))
temp.length.firsthalf = aggregate(sedentary.width ~ user + study.day, 
                                  data = window.time[14 < hour(window.time$window.utime) 
                                                     & hour(window.time$window.utime) < 20,], 
                                  FUN = function(x) c(rle(x)$length))

Sedentary.values.firsthalf = as.logical(unlist(temp.values.firsthalf[,3]))
Sedentary.length.firsthalf = as.numeric(unlist(temp.length.firsthalf[,3]))

temp.values.secondhalf = aggregate(sedentary.width~ user + study.day, 
                                   data = window.time[20 <= hour(window.time$window.utime) 
                                                      | hour(window.time$window.utime) <= 2,], 
                                   FUN = function(x) c(rle(x)$values))
temp.length.secondhalf = aggregate(sedentary.width~ user + study.day, 
                                   data = window.time[20 <= hour(window.time$window.utime) 
                                                      | hour(window.time$window.utime) <= 2,], 
                                   FUN = function(x) c(rle(x)$length))

Sedentary.values.secondhalf = as.logical(unlist(temp.values.secondhalf[,3]))
Sedentary.length.secondhalf = as.numeric(unlist(temp.length.secondhalf[,3]))


# firsthalf.sedentary.runs = rle(window.time$sedentary.thirty[14 < hour(window.time$window.utime) 
#                                                             & hour(window.time$window.utime) < 20])
# secondhalf.sedentary.runs = rle(window.time$sedentary.thirty[!(14 < hour(window.time$window.utime) 
#                                                                & hour(window.time$window.utime) < 20)])

hist(Sedentary.length.firsthalf[Sedentary.values.firsthalf == 1 & Sedentary.length.firsthalf < 100], 
     breaks = 75, xlab = "Length of sequential classification to Sedentary for first half of day", main = "")

hist(Sedentary.length.secondhalf[Sedentary.values.secondhalf == 1 & Sedentary.length.secondhalf < 100], 
     breaks = 75, xlab = "Length of sequential classification to Sedentary for second half of day", main = "")

## There appears to be no major departures!

## We now check the google.fit data to see if 
## sedentary and non-sedentary plots are similar

gf.window.time = data.frame(rep(used.daily$user,each = repeat.times),
                         dates,
                         rep(used.daily$study.day, each = repeat.times))

names(gf.window.time) = c("user", "window.utime", "study.day")

gf.window.time = gf.window.time[!is.na(gf.window.time$window.utime),]

## Merge.first 

gfprefive <- merge.first(subset(googlefit,
                                end.utime >= intake.utime & end.utime <= last.utime),
                         subset(window.time,
                                select = c(user, window.utime, study.day)),
                         id = "user", var.x = "end.utime", var.y = "window.utime")

gfprefive <- gfprefive[with(gfprefive, order(user, end.utime)), ]

gf.window.time <- merge(gf.window.time,
                     aggregate(steps ~ user + window.utime, data = gfprefive, sum),
                     by.x = c("user", "window.utime"), by.y = c("user", "window.utime"),
                     all.x = TRUE)

gf.window.time$sedentary = is.na(gf.window.time$steps)

gf.sedentary.width = rollapply(gf.window.time$sedentary,FUN = prod, width = chosen.width)

gf.window.time$sedentary.width = c(gf.sedentary.width,rep(0,chosen.width-1))

gf.hour.toss = hour(gf.window.time$window.utime)
gf.minute.toss = minute(gf.window.time$window.utime)

gf.window.time = gf.window.time[-which(gf.hour.toss == 1 & gf.minute.toss >= 30),]

#### Summary of the Google Fit Data

### Initial summary

temp.values.gf = aggregate(sedentary.width ~ user + study.day, 
                                   data = gf.window.time, 
                                   FUN = function(x) c(rle(x)$values))
temp.length.gf = aggregate(sedentary.width~ user + study.day, 
                                   data = gf.window.time, 
                                   FUN = function(x) c(rle(x)$length))

Sedentary.values.gf = as.logical(unlist(temp.values.gf[,3]))
Sedentary.length.gf = as.numeric(unlist(temp.length.gf[,3]))


# gf.sedentary.runs = rle(gf.window.time$sedentary.thirty)

par(mar = c(5,3,1,1)+0.1, mfrow = c(2,1))
hist(Sedentary.length.gf[Sedentary.values.gf == 0 & Sedentary.length.gf < 100],
     breaks = 75, xlab = "Length of sequential classification to Not Sedentary", main = "")

hist(Sedentary.length.gf[Sedentary.values.gf == 1 & Sedentary.length.gf < 100],
     breaks = 75, xlab = "Length of sequential classification to Sedentary", main = "")


#### Situation 3:  Sedentary "only" if both gf and jb say so 
#### Otherwise they are Not Sedentary

### Initial summary
combo.sedentary.width = gf.window.time$sedentary.width == 1 & window.time$sedentary.width == 1

temp.values.combo = aggregate(combo.sedentary.width ~ gf.window.time$user + gf.window.time$study.day, 
                           FUN = function(x) c(rle(x)$values))
temp.length.combo = aggregate(combo.sedentary.width ~ gf.window.time$user + gf.window.time$study.day, 
                           FUN = function(x) c(rle(x)$length))

Sedentary.values.combo = as.logical(unlist(temp.values.combo[,3]))
Sedentary.length.combo = as.numeric(unlist(temp.length.combo[,3]))

par(mar = c(5,3,1,1)+0.1, mfrow = c(2,1))
hist(Sedentary.length.combo[Sedentary.values.combo==0 & Sedentary.length.combo < 100],
     breaks = 75, xlab = "Length of sequential classification to Not Sedentary", main = " ")

hist(Sedentary.length.combo[Sedentary.values.combo==1 & Sedentary.length.combo < 100],
     breaks = 75, xlab = "Length of sequential classification to Sedentary", main = "")

## Check the remainder function 
## for two choices of eta

seq.t = seq(1,130,12)
seq.hour = (14+floor(seq.t/12))%%24
seq.remaining.time = 144- seq.t
current.run.length = 15

False.res = False.res.etaone = seq.t * 0
True.res = True.res.etaone = seq.t * 0

for(i in 1:length(seq.t)) {
  False.res[i] = full.remainder.fn(seq.remaining.time[i], FALSE, current.run.length, seq.hour[i], eta = 0)
  False.res.etaone[i] = full.remainder.fn(seq.remaining.time[i], FALSE, current.run.length, seq.hour[i], eta = 1)
  True.res[i] = full.remainder.fn(seq.remaining.time[i], TRUE, current.run.length, seq.hour[i], eta = 0)
  True.res.etaone[i] = full.remainder.fn(seq.remaining.time[i], TRUE, current.run.length, seq.hour[i], eta = 1)
}


par(mar = c(4,4,1,1)+0.1, mfrow = c(2,1))
plot(seq.t, False.res, ylim = c(0, 60), type = "l", axes = FALSE, xlab = "Time Steps (t)", ylab = "g(Not Sedentary,15,T-t)")
lines(seq.t, False.res.etaone, lty = 2)
axis(side = 1); axis(side = 2, at = seq(0,60,20))


plot(seq.t, True.res, ylim = c(0, 60), type = "l", axes = FALSE, xlab = "Time Steps (t)", ylab = "g(Sedentary,15, T-t)")
lines(seq.t, True.res.etaone, lty = 2)
axis(side = 1); axis(side = 2, at = seq(0,60,20))

