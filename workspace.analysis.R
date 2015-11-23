## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")

max.date <- as.Date("2015-11-21")

## --- daily data
## FIXME: sort files by user, day

## users
## infer intake from first selection of notification time slots
users <- merge(subset(timeslot, !duplicated(user),
                      select = c(user, date.updated, tz, gmtoff)),
               aggregate(date.stamp ~ user, data = decision, max),
               by = "user", all = TRUE)
## infer last "contact" from last momentry decision date
users <- merge(users, subset(decision, !duplicated(cbind(user, date.stamp)),
                             select = c(user, date.stamp, tz, gmtoff)),
               by = c("user", "date.stamp"), all.x = TRUE,
               suffixes = c(".intake", ".last"))
names(users)[2:3] <- c("last.date", "intake.date")
## days from intake to last momentary decision date
users$days <- with(users, as.numeric(difftime(last.date, intake.date, "days")))
## indicate users that just enrolled or dropped out
users$exclude <- with(users, intake.date >= max.date | days < 7 |
                             (intake.date + 42 < max.date & days < 10))
## odd user id implies that HeartSteps is installed on own phone
users$own.phone <- users$user %% 2 != 0
## add intake and exit interview data
users <- merge(users, merge(intake, exit, by = c("user", "userid"), all = TRUE,
                            suffixes = c(".intake", ".exit")),
               by = "user", all.x = TRUE)

## expand to user data to user-day level
daily <-
  do.call("rbind",
          sapply(which(!users$exclude),
                 function(r)
                   with(users[r, , drop = FALSE],
                        data.frame(user = user, intake.date = intake.date,
                                   last.date = last.date, tz.intake = tz.intake,
                                   gmtoff.intake = gmtoff.intake,
                                   study.date = seq(intake.date,
                                                    pmin(last.date, max.date),
                                                    by = "days"))),
                 simplify = FALSE))
## index day on study, starting from zero
daily$study.day <- with(daily, as.numeric(difftime(study.date, intake.date,
                                                   units = "days")))

## context at EMA notification or (if unavailable) at earliest engagement context
any(with(notify, duplicated(cbind(user, notified.date))))
names(notify) <- gsub("^notified\\.", "context.", names(notify))
names(engage) <- gsub("^engaged\\.", "context.", names(engage))
notify$notify <- 1
temp <- merge(notify, engage, all = TRUE)
temp <- temp[with(temp,
                  order(user, is.na(notify), context.date, context.utime)), ]
temp <- subset(temp, !duplicated(cbind(user, context.date)))
daily <- merge(daily,
               subset(temp,
                      select = c(user, tz, gmtoff, context.date, context.utime,
                                 context.time.year:context.time.sec,
                                 planning.today, home, work, calendar,
                                 recognized.activity, front.end.application,
                                 gps.coordinate, city, location.exact,
                                 location.category, weather.condition,
                                 temperature, windspeed, precipitation.chance,
                                 snow)),
               by.x = c("user", "study.date"), by.y = c("user", "context.date"),
               all.x = TRUE)

## add planning status
any(with(plan, duplicated(cbind(user, date.started))))
daily <- merge(daily,
               with(plan, aggregate(planning, list(user, date.started),
                                    function(x) x[1])),
               by.x = c("user", "study.date"), by.y = c("Group.1", "Group.2"),
               all.x = TRUE)

## add EMA response
any(with(ema, duplicated(cbind(user, message.date, order))))
daily <- merge(daily,
               aggregate(subset(ema, select = c(ema.set.length, hectic:urge)),
                         by = with(ema, list(user, message.date)),
                         function(x) na.omit(x)[1]),
               by.x = c("user", "study.date"),
               by.y = paste("Group", 1:2, sep = "."), all.x = TRUE)
## any EMAs erroneously represented as missing?
setNames(sapply(with(users, user[!exclude]),
                function(u) any(subset(daily, user == u
                                       & is.na(ema.set.length))$study.date
                                %in% subset(ema, user == u)$message.date)),
         with(users, user[!exclude]))

## administered (versus intended) planning status
daily$planning <- with(daily, ifelse(is.na(ema.set.length),
                                     NA, ifelse(is.na(x), "no_planning", x)))
daily$x <- NULL

## add step counts
## FIXME: align step counts with timing of EMA?
jawbone <- merge(jawbone, users, by = "user", suffixes = c("", ".user"))
jawbone$end.date <- do.call("c",
                            with(jawbone,
                                 mapply(as.Date, x = end.utime, tz = tz.intake,
                                        SIMPLIFY = FALSE)))
daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = jawbone, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "jbsteps"
daily$na.jbsteps <- is.na(daily$jbsteps)
daily$lag1.na.jbsteps <- with(daily, delay(user, study.day, na.jbsteps))

googlefit <- merge(googlefit, users, by = "user", suffixes = c("", ".user"))
googlefit$end.date <- do.call("c",
                            with(googlefit,
                                 mapply(as.Date, x = end.utime, tz = tz.intake,
                                        SIMPLIFY = FALSE)))
daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = googlefit, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "gfsteps"
daily$na.gfsteps <- is.na(daily$gfsteps)
daily$lag1.na.gfsteps <- with(daily, delay(user, study.day, na.gfsteps))

daily <- daily[with(daily, order(user, study.day)), ]

## expand daily data to level of the momentary decision to provide a suggestion
suggest <- do.call("rbind",
                   sapply(1:nrow(daily),
                          function(r)
                            with(daily[r, , drop = FALSE],
                                 data.frame(user = user,
                                            intake.date = intake.date,
                                            last.date = last.date,
                                            tz.intake = tz.intake,
                                            gmtoff.intake = gmtoff.intake,
                                            study.date = study.date,
                                            study.day = study.day, slot = 1:5)),
                          simplify = FALSE))
## index momentary decision, starting from zero
suggest$decision <- do.call("c", sapply(table(suggest$user) - 1, seq, from = 0,
                                        by = 1, simplify = FALSE))

## add decision result by the *intended* time slot
any(with(decision, duplicated(cbind(user, date.stamp, slot))))
suggest <- merge(suggest,
                 subset(decision,
                        select = c(user, tz, gmtoff, date.stamp, utime.stamp,
                                   time.stamp.year:time.stamp.sec, is.prefetch,
                                   slot, time.slot, time.stamp.slot, notify,
                                   returned.message, tag.active, snooze.status,
                                   is.randomized, recognized.activity,
                                   front.end.application)),
                 by.x = c("user", "study.date", "slot"),
                 by.y = c("user", "date.stamp", "slot"),
                 all.x = TRUE)

## add suggestion response and its context
any(with(response, duplicated(cbind(user, notified.date, slot))))
suggest <- merge(suggest,
                 subset(response,
                        select = c(user, notified.date, slot,
                                   notified.utime, responded.utime,
                                   notified.time.year:notified.time.sec,
                                   responded.time.year:responded.time.sec,
                                   interaction.count, notification.message,
                                   tag.active, response, home, work, calendar,
                                   recognized.activity, gps.coordinate, city,
                                   location.exact, location.category,
                                   weather.condition, temperature, windspeed,
                                   precipitation.chance, snow)),
                 by.x = c("user", "study.date", "slot"),
                 by.y = c("user", "notified.date", "slot"),
                 all.x = TRUE, suffixes = c("", ".response"))

save(max.date, users, daily, suggest, file = "analysis.RData")
