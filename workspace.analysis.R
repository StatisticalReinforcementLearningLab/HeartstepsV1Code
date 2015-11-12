## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")

max.date <- as.Date("2015-11-11")

## --- daily data

## users
## intake from first point at which the user selected time slots
## last "contact" from last momentry decision date
users <- merge(subset(timeslot, !duplicated(user),
                      select = c(user, date.updated, tz, gmtoff)),
               aggregate(date.stamp ~ user, data = decision, max),
               by = "user", all = TRUE)
users <- merge(users,
               subset(decision, !duplicated(cbind(user, date.stamp)),
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

## expand to user data to user-day level
daily <- do.call("rbind",
                 sapply(which(!users$exclude),
                        function(r)
                          with(users[r, , drop = FALSE],
                               data.frame(user = user, intake.date = intake.date,
                                          tz.intake = tz.intake,
                                          gmtoff.intake = gmtoff.intake,
                                          study.date = seq(intake.date, last.date,
                                                           by = "days"))),
                        simplify = FALSE))
daily$study.day <- with(daily, as.numeric(difftime(study.date, intake.date,
                                                   units = "days")))

## planning/EMA notification context
any(with(notify, duplicated(cbind(user, notified.date))))
daily <- merge(daily,
               subset(notify,
                      select = c(user, tz, gmtoff, notified.date, notified.utime,
                                 notified.time.year:notified.time.sec,
                                 planning.today, ema.set.today, ema.set.length,
                                 home, work, calendar, recognized.activity,
                                 front.end.application, gps.coordinate,
                                 city, location.exact, location.category,
                                 weather.condition, temperature, windspeed,
                                 precipitation.chance, snow)),
               by.x = c("user", "study.date"), by.y = c("user", "notified.date"),
               all.x = TRUE)

## planning status
## nb: if status is updated and read from device in reverse order, the previous
##     planning status is administered (on day 1, this is 'no planning');
##     the intended distribution was 50% no planning, 25% structured/unstructured
any(with(plan, duplicated(cbind(user, date.started))))
daily <- merge(daily,
               with(plan, aggregate(planning, list(user, date.started),
                                    function(x) x[1])),
               by.x = c("user", "study.date"), by.y = c("Group.1", "Group.2"),
               all.x = TRUE)
daily$planning <- with(daily, ifelse(is.na(ema.set.length),
                                     NA, ifelse(is.na(x), "none", x)))
daily$x <- NULL

## EMA response
any(with(ema, duplicated(cbind(user, message.date, order))))
daily <- merge(daily,
               aggregate(subset(ema, select = hectic:msg.up),
                         by = with(ema, list(user, message.date)), na.omit),
               by.x = c("user", "study.date"),
               by.y = paste("Group", 1:2, sep = "."), all.x = TRUE)

## step counts
## FIXME: align days with EMA slot in home time zone
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

save(intake, users, daily, file = "analysis.RData")
