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

## add daily step counts that are from jawbone website.
## this data included the daily step count variable with Google fit imputed for User 35.
daily.jb = read.csv("daily.jbsteps.csv")

daily.jb$DATE=as.character(daily.jb$DATE)
daily.jb$DATE=as.Date(daily.jb$DATE, "%Y%m%d")

daily=merge(daily, subset(daily.jb,select=c(user, DATE, m_steps)),
            by.x=c("user","study.date"), by.y = c("user","DATE"), 
            all.x=TRUE)

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

##### Suggestion data #####

## number of suggestion decision points in a given day
k <- length(slots) - 1

## expand daily data to level of the decision points
suggest <- data.frame(subset(daily, select = user:study.day.nogap),
                      slot = rep(1:k, nrow(daily)), row.names = NULL)

## discard decision points that take place before intake or after exit
suggest <- subset(suggest, !(study.date == intake.date & slot <= intake.slot)
                  & !(study.date == last.date & slot > last.slot))

## add decision result and context by the *intended* time slot
any(with(decision, duplicated(cbind(user, date.stamp, slot))))
any(with(decision, duplicated(cbind(user, date.stamp, slot, tz, gmtoff))))
suggest <- merge(suggest,
                 subset(decision,
                        select = c(user, tz, gmtoff, date.stamp,
                                   utime.stamp, time.stamp.year:time.stamp.sec,
                                   is.prefetch, slot, time.slot, time.stamp.slot,
                                   link, notify, is.randomized, snooze.status,
                                   recognized.activity, front.end.application,
                                   returned.message, calendar, gps.coordinate,
                                   home, work, city, location.exact, 
                                   location.category, weather.condition,
                                   temperature, windspeed, precipitation.chance,
                                   snow, tag.active)),
                 by.x = c("user", "study.date", "slot"),
                 by.y = c("user", "date.stamp", "slot"), all.x = TRUE)

## add suggestion response and its context
any(with(response, duplicated(cbind(user, notified.date, slot))))
suggest <- merge(suggest,
                 subset(response,
                        select = c(user, date.stamp, slot,
                                   notified.utime, responded.utime,
                                   notified.time.year:notified.time.sec,
                                   responded.time.year:responded.time.sec,
                                   recognized.activity, interaction.count,
                                   calendar, gps.coordinate, home, work, city,
                                   location.exact, location.category,
                                   weather.condition, temperature, windspeed,
                                   precipitation.chance, snow,
                                   notification.message, response)),
                 by.x = c("user", "study.date", "slot"),
                 by.y = c("user", "date.stamp", "slot"),
                 all.x = TRUE, suffixes = c("", ".response"))

## index momentary decision, starting from zero
suggest$decision.index <-
  do.call("c", sapply(table(suggest$user) - 1, seq, from = 0, by = 1,
                      simplify = FALSE))

suggest$decision.index.nogap <- NA
suggest$decision.index.nogap[!is.na(suggest$study.day.nogap)] <- 
  do.call("c", sapply(table(suggest$user[!is.na(suggest$study.day.nogap)]) - 1,
                      seq, from = 0, by = 1, simplify = FALSE))

suggest$last.date <- with(suggest,
                        as.Date(sapply(1:dim(suggest)[1], 
                                       function(x) min(last.date[x], 
                                                       dropout.date[x],
                                                       na.rm = T))))

## expand user-designated times into day-slot level...
temp <-
  data.frame(subset(timeslot,
                    select = c(user, date.updated, utime.updated, gmtoff)),
             slot = rep(1:k, each = nrow(timeslot)),
             hrsmin = unlist(timeslot[, match(slots[1:k], names(timeslot))]),
             row.names = NULL)

## ... omit redundant slots
temp <- temp[with(temp, order(user, slot, utime.updated)), ]
temp <- subset(temp, !duplicated(cbind(user, slot, date.updated, hrsmin)))

## ... adjust date updated to date from which the slot designation is relevant
temp$slot.utime <-
  with(temp, char2utime(paste0(date.updated, " ", hrsmin, ":00"), gmtoff))
temp$date.updated <- with(temp, date.updated + (utime.updated > slot.utime))

## ... for slot times in the same relevant day, omit the earlier times
temp <- temp[with(temp, order(user, date.updated, -as.numeric(utime.updated))), ]
temp <- subset(temp, !duplicated(cbind(user, slot, date.updated)),
               select = -c(slot.utime, utime.updated, gmtoff))

## ... add slot times, evaluated under the last available time zone
suggest <- merge.last(suggest, temp, id = c("user", "slot"),
                      var.x = "study.date", var.y = "date.updated")
suggest <- suggest[with(suggest, order(user, study.date, slot)), ]
suggest$gmtoff <- with(suggest, impute(user, decision.index, gmtoff, na.locf))
suggest$slot.utime <-
  with(suggest, char2utime(paste0(study.date, " ", hrsmin, ":00"), gmtoff))

## had active connection at decision slot?
suggest$connect <- with(suggest, !is.na(notify))

## decision time or (in the absence of a decision) the user-designated
## notification time, adjusting for time lag with prefetch/activity recognition
suggest$decision.utime <- suggest$slot.utime + 90
suggest$decision.utime[suggest$connect] <-
  with(subset(suggest, connect), utime.stamp + 30 * 60 * is.prefetch)
any(is.na(suggest$decision.utime))
any(with(suggest, duplicated(cbind(user, decision.utime))))

## add last time the device was used
suggest <-
  merge.last(suggest,
             with(tracker, data.frame(user, start.utime,
                                      device.utime = start.utime)),
             id = "user", var.x = "decision.utime", var.y = "start.utime")
suggest$device.since <-
  with(suggest, difftime(decision.utime, device.utime, units = "hours"))

## walking or in a vehicle at decision slot?
suggest$intransit <-
  with(suggest, !(recognized.activity %in% c("STILL", "UNKNOWN")))
suggest$intransit[!suggest$connect] <- NA

## first-pass availability, as defined plus active connection
suggest$avail <- with(suggest, connect & !snooze.status & !intransit)

## send status; like 'notify', but corrected for prefetch issues
suggest$send <- with(suggest, (avail & is.randomized) | !is.na(response))

## Suggestion type: Active vs. Sedentary vs. None
suggest$send.active    <- (suggest$send & suggest$tag.active)
suggest$send.sedentary <- (suggest$send & !suggest$tag.active)

suggest <- suggest[with(suggest, order(user, decision.index)), ]

## --- step counts by slots, useful for constructing the proximal outcome

jbslot <- merge.last(subset(jawbone,
                            end.utime >= intake.utime & end.utime <= last.utime),
                     subset(suggest,
                            select = c(user, user.index, slot, decision.utime,
                                       study.date, study.day, study.day.nogap,
                                       decision.index, decision.index.nogap,
                                       connect, avail, send)),
                     id = "user", var.x = "end.utime", var.y = "decision.utime")
jbslot <- subset(jbslot, !is.na(slot))
jbslot <- jbslot[with(jbslot, order(user, end.utime)), ]

jbslotpre <- merge.first(subset(jawbone,
                               end.utime >= intake.utime & end.utime <= last.utime),
                        subset(suggest,
                               select = c(user, user.index, slot, decision.utime,
                                          study.date, study.day, study.day.nogap,
                                          decision.index, decision.index.nogap,
                                          connect, avail, send)),
                        id = "user", var.x = "end.utime", var.y = "decision.utime")
jbslotpre <- subset(jbslotpre, !is.na(slot))
jbslotpre <- jbslotpre[with(jbslotpre, order(user, end.utime)), ]

gfslot <- merge.last(subset(googlefit,
                            end.utime >= intake.utime & end.utime <= last.utime),
                     subset(suggest,
                            select = c(user, user.index, slot, decision.utime,
                                       study.date, study.day, decision.index,
                                       connect, avail, send)),
                     id = "user", var.x = "end.utime", var.y = "decision.utime")
gfslot <- subset(gfslot, !is.na(slot))
gfslot <- gfslot[with(gfslot, order(user, end.utime)), ]

gfslotpre <- merge.first(subset(googlefit,
                            end.utime >= intake.utime & end.utime <= last.utime),
                     subset(suggest,
                            select = c(user, user.index, slot, decision.utime,
                                       study.date, study.day, decision.index,
                                       connect, avail, send)),
                     id = "user", var.x = "end.utime", var.y = "decision.utime")
gfslotpre <- subset(gfslotpre, !is.na(slot))
gfslotpre <- gfslotpre[with(gfslotpre, order(user, end.utime)), ]

## --- add step counts 30 and 60 minute FOLLOWING each decision point
## NB: We impute 0 steps when there is no record in the Jawbone data

temp <- with(jbslot, end.utime - decision.utime)
suggest <- merge(suggest,
                 aggregate(cbind(jbmins10  = temp <= 10 * 60,
                                 jbsteps10 = steps * (temp <= 10 * 60),
                                 jbmins30  = temp <= 30 * 60,
                                 jbsteps30 = steps * (temp <= 30 * 60),
                                 jbmins60  = temp <= 60 * 60,
                                 jbsteps60 = steps * (temp <= 60 * 60))
                           ~ decision.index + user, data = jbslot, FUN = sum),
                 by = c("user", "decision.index"), all.x = TRUE)

temp <- with(gfslot, end.utime - decision.utime)
suggest <- merge(suggest,
                 aggregate(cbind(gfmins10  = temp <= 10 * 60,
                                 gfsteps10 = steps * (temp <= 10 * 60),
                                 gfmins30  = temp <= 30 * 60,
                                 gfsteps30 = steps * (temp <= 30 * 60),
                                 gfmins60  = temp <= 60 * 60,
                                 gfsteps60 = steps * (temp <= 60 * 60))
                           ~ decision.index + user, data = gfslot, FUN = sum),
                 by = c("user", "decision.index"), all.x = TRUE)

# Aggregate returns NA when study.date is not in the user's Jawbone data (e.g., if 
# the user just didn't wear the Jawbone for a whole day, or if she was traveling 
# internationally and left the tracker at home). We impute zeros.
suggest$jbsteps10.zero <- with(suggest, ifelse(is.na(jbsteps10), 0, jbsteps10))
suggest$jbsteps30.zero <- with(suggest, ifelse(is.na(jbsteps30), 0, jbsteps30))
suggest$jbsteps60.zero <- with(suggest, ifelse(is.na(jbsteps60), 0, jbsteps60))

## Log transform step count
suggest$jbsteps10.log <- log(suggest$jbsteps10.zero + 0.5)
suggest$jbsteps30.log <- log(suggest$jbsteps30.zero + 0.5)
suggest$jbsteps60.log <- log(suggest$jbsteps60.zero + 0.5)


## --- add steps counts 30 and 60 minutes PRIOR TO each decision point
temp <- with(jbslotpre, decision.utime - end.utime)
suggest <- merge(suggest,
                 aggregate(cbind(jbmins30pre = (temp <= 30 * 60),
                                 jbsteps30pre = steps * (temp <= 30 * 60),
                                 jbmins60pre = (temp <= 60 * 60),
                                 jbsteps60pre = steps * (temp <= 60 * 60))
                           ~ decision.index + user, data = jbslotpre, FUN = sum,
                           na.rm = TRUE),
                 by = c("user", "decision.index"), all.x = TRUE)

temp <- with(gfslotpre, decision.utime - end.utime)
suggest <- merge(suggest,
                 aggregate(cbind(gfmins30pre = temp <= 30 * 60,
                                 gfsteps30pre = steps * (temp <= 30 * 60),
                                 gfmins60pre = temp <= 60 * 60,
                                 gfsteps60pre = steps * (temp <= 60 * 60))
                           ~ decision.index + user, data = gfslotpre, FUN = sum,
                           na.rm = TRUE),
                 by = c("user", "decision.index"), all.x = TRUE)

## As above, impute zeros whenever missing
suggest$jbsteps30pre.zero <- suggest$jbsteps30pre
suggest$jbsteps60pre.zero <- suggest$jbsteps60pre
suggest$jbsteps30pre.zero[is.na(suggest$jbsteps30pre)] <- 0
suggest$jbsteps60pre.zero[is.na(suggest$jbsteps60pre)] <- 0

## Spline imputation
suggest$steps30.spl <- with(suggest, impute(user, decision.index, jbsteps30,
                                            fun = na.spline))
suggest$steps60.spl <- with(suggest, impute(user, decision.index, jbsteps60,
                                            fun = na.spline))

## Log-transform step count
suggest$jbsteps30pre.log <- log(suggest$jbsteps30pre.zero + 0.5)
suggest$jbsteps60pre.log <- log(suggest$jbsteps60pre.zero + 0.5)


save(max.day, max.date, users, daily, suggest, jbslot, gfslot, jbslotpre, gfslotpre,
     file = "analysis.RData")
save(suggest, users, daily, jawbone, timezone, file = "analysis-small.RData")
setwd(sys.var$repo)