## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")

max.date <- as.Date("2015-11-27")

## --- user data

## slot of update time, last notification

## infer intake date-time from first selection of notification time slots
timeslot$slot <- ltime2slot(time.updated.hour, time.updated.min, timeslot)
users <- with(subset(timeslot[with(timeslot, order(user, utime.updated)), ],
                     !duplicated(user)),
              data.frame(user, intake.date = date.updated,
                         intake.utime = utime.updated, intake.tz = tz,
                         intake.gmtoff = gmtoff, intake.min = time.updated.min,
                         intake.hour = time.updated.hour, first.slot = slot + 1))

## infer exit date-time from last notification
temp <-
  rbind(with(decision,
             data.frame(user, last.date = date.stamp, last.utime = utime.stamp,
                        last.tz = tz, last.gmtoff = gmtoff, last.slot = slot,
                        last.hour = time.stamp.hour, last.min = time.stamp.min)),
        with(notify,
             data.frame(user, last.date = notified.date,
                        last.utime = notified.utime, last.tz = tz,
                        last.gmtoff = gmtoff, last.slot = length(slots),
                        last.hour = notified.time.hour,
                        last.min = notified.time.min)))
temp <- temp[with(temp, order(user, -as.numeric(last.utime))), ]
users <- merge(users, subset(temp, !duplicated(user)), by = "user", all = TRUE)

## indicate users that just enrolled or dropped out
users$days <- with(users, as.numeric(difftime(last.date, intake.date, "days")))
users$exclude <- with(users, intake.date >= max.date | days < 7 |
                             (intake.date + 42 < max.date & days < 10))
users$user.index <- cumsum(!users$exclude)
users$user.index[users$exclude] <- NA

## odd user id implies that HeartSteps is installed on own phone
users$own.phone <- users$user %% 2 != 0

## device locale is (most likely) US English
users <- merge(users,
               aggregate(cbind(en.locale = !grepl("^\\?+$", timezone)) ~ user,
                         all, data = read.csv("checks/user_timezones.csv")),
               by = "user", all.x = TRUE)

## add intake and exit interview data
users <- merge(users,
               merge(intake, exit, by = c("user", "userid"), all = TRUE,
                     suffixes = c(".intake", ".exit")),
               by = "user", all.x = TRUE)

## --- daily data

## expand to user data to user-day level
daily <-
  do.call("rbind",
          sapply(which(!users$exclude),
                 function(r)
                   with(users[r, , drop = FALSE],
                        data.frame(user, user.index, intake.date, intake.utime,
                                   intake.tz, intake.gmtoff, intake.hour,
                                   intake.min, first.slot, last.date,
                                   last.utime, last.tz, last.gmtoff, last.hour,
                                   last.min, last.slot,
                                   study.date = seq(intake.date,
                                                    pmin(last.date, max.date),
                                                    by = "days"))),
                 simplify = FALSE))

## index day on study, starting from zero
daily$study.day <-
  with(daily, as.numeric(difftime(study.date, intake.date, units = "days")))

## context at EMA notification or (if unavailable) at earliest engagement
any(with(notify, duplicated(cbind(user, ema.date))))
names(notify) <- gsub("^notified\\.(|time.)", "context.", names(notify))
names(engage) <- gsub("^engaged\\.(|time.)", "context.", names(engage))
notify$notify <- 1
temp <- merge(notify, engage, all = TRUE)
temp <- temp[with(temp, order(user, is.na(notify), ema.date, context.utime)), ]
temp <- subset(temp, !duplicated(cbind(user, ema.date)))
daily <-
  merge(daily,
        subset(temp,
               select = c(user, tz, gmtoff, ema.date, notify, context.date,
                          context.utime, context.year:context.sec,
                          planning.today, home, work, calendar,
                          recognized.activity, front.end.application,
                          gps.coordinate, city, location.exact,
                          location.category, weather.condition, temperature,
                          windspeed, precipitation.chance, snow)),
        by.x = c("user", "study.date"), by.y = c("user", "ema.date"),
        all.x = TRUE)

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
                function(u) any(subset(daily, user == u
                                       & is.na(ema.set.length))$study.date
                                %in% subset(ema, user == u)$ema.date)),
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
jawbone <- merge(jawbone, users, by = "user", suffixes = c("", ".user"))
jawbone$end.date <- do.call("c",
                            with(jawbone,
                                 mapply(as.Date, x = end.utime, tz = intake.tz,
                                        SIMPLIFY = FALSE)))
daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = jawbone, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "jbsteps"
daily$lag1.jbsteps <- with(daily, delay(user, study.day, jbsteps))

googlefit <- merge(googlefit, users, by = "user", suffixes = c("", ".user"))
googlefit$end.date <- do.call("c",
                            with(googlefit,
                                 mapply(as.Date, x = end.utime, tz = intake.tz,
                                        SIMPLIFY = FALSE)))
daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = googlefit, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "gfsteps"
daily$lag1.gfsteps <- with(daily, delay(user, study.day, gfsteps))

daily <- daily[with(daily, order(user, study.day)), ]

## --- suggestion data

## expand daily data to level of the suggestion/treatment occasions
suggest <-
  do.call("rbind",
          sapply(1:nrow(daily),
                 function(r)
                   with(daily[r, , drop = FALSE],
                        data.frame(user, user.index, intake.date, intake.utime,
                                   intake.tz, intake.gmtoff, intake.hour,
                                   intake.min, last.date, last.utime, last.tz,
                                   last.gmtoff, last.hour, last.min, first.slot,
                                   last.slot, study.date, study.day,
                                   slot = 1:5)),
                 simplify = FALSE))
suggest <- subset(suggest, !(study.date == intake.date & slot < first.slot)
                  & !(study.date == last.date & slot > last.slot))

## add decision result by the *intended* time slot
any(with(decision, duplicated(cbind(user, date.stamp, slot))))
suggest <-
  merge(suggest,
        subset(decision,
               select = c(user, tz, gmtoff, date.stamp, utime.stamp,
                          time.stamp.year:time.stamp.sec, is.prefetch, slot,
                          time.slot, time.stamp.slot, returned.message, link,
                          tag.active, snooze.status, notify, is.randomized,
                          recognized.activity, front.end.application)),
        by.x = c("user", "study.date", "slot"),
        by.y = c("user", "date.stamp", "slot"),
        all.x = TRUE)

## index momentary decision, starting from zero
suggest$index <- do.call("c", sapply(table(suggest$user) - 1, seq, from = 0,
                                     by = 1, simplify = FALSE))

## add suggestion response and its context
any(with(response, duplicated(cbind(user, notified.date, slot))))
suggest <-
  merge(suggest,
        subset(response,
               select = c(user, date.stamp, slot, notified.utime,
                          responded.utime, notified.time.year:notified.time.sec,
                          responded.time.year:responded.time.sec,
                          interaction.count, notification.message, response,
                          home, work, calendar, recognized.activity,
                          gps.coordinate, city, location.exact,
                          location.category, weather.condition, temperature,
                          windspeed, precipitation.chance, snow)),
        by.x = c("user", "study.date", "slot"),
        by.y = c("user", "date.stamp", "slot"),
        all.x = TRUE, suffixes = c("", ".response"))

## had active connection at decision slot?
suggest$connect <- with(suggest, !is.na(notify))

## suggestion pushed to user?
## nb: this is inferred and combines the randomization and availability
suggest$suggest <- with(suggest, connect & link)

save(max.date, users, daily, suggest, file = "analysis.RData")
