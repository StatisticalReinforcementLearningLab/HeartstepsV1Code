## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")

max.date <- as.Date("2015-11-08")

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

## expand to user-day level
daily <- do.call("rbind", sapply(1:nrow(users), function(r)
  with(users[r, , drop = FALSE],
       data.frame(user = user, intake.date = intake.date, tz.intake = tz.intake,
                  gmtoff.intake = gmtoff.intake,
                  study.date = seq(intake.date, last.date, by = "days"))),
  simplify = FALSE))
daily$study.day <- with(daily, as.numeric(difftime(study.date, intake.date,
                                                   "days")))

## EMA (partial) completion status
daily <- merge(daily,
               aggregate(ema.set.length ~ user + message.date, data = ema, max),
               by.x = c("user", "study.date"), by.y = c("user", "message.date"),
               all.x = TRUE)

## planning status
## nb: if status is written and read from device in reverse order,
##     the previous planning decision is read (on day 1, this is 'no planning')
## FIXME: check actual distribution - intended is 50% no planning, 25% structured
daily <- merge(daily,
               with(plan, aggregate(planning, list(user, date.started),
                                    function(x) x[1])),
               by.x = c("user", "study.date"), by.y = c("Group.1", "Group.2"),
               all.x = TRUE)
daily$plan <- with(daily, ifelse(is.na(ema.set.length),
                                 NA, ifelse(is.na(x), "none", x)))
daily$x <- NULL

## step counts
## FIXME: align days with EMA slot in home time zone
jawbone <- merge(jawbone, users, by = "user", suffixes = c("", ".user"))
jawbone$end.date <- do.call("c",
                            with(jawbone,
                                 mapply(as.Date, x = end.utime, tz = tz.user,
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
                                 mapply(as.Date, x = end.utime, tz = tz.user,
                                        SIMPLIFY = FALSE)))
daily <- merge(daily,
               aggregate(steps ~ user + end.date, data = googlefit, sum),
               by.x = c("user", "study.date"), by.y = c("user", "end.date"),
               all.x = TRUE)
names(daily)[ncol(daily)] <- "gfsteps"
daily$na.gfsteps <- is.na(daily$gfsteps)
daily$lag1.na.gfsteps <- with(daily, delay(user, study.day, na.gfsteps))

## plot a given user's daily step counts
daily.plot <- function(u) {
  d <- subset(daily, user == u)
  maxs <- max(1, d$jbsteps, d$gfsteps, na.rm = TRUE)
  maxd <- max(d$study.day)
  plot(NULL, xlim = c(0, maxd), ylim = c(0, maxs),
       xlab = "", ylab = "", main = "", axes = FALSE, frame.plot = FALSE)
  mtext(paste(u), 2, line = 3, cex = 0.75)
  sapply(subset(d, is.na(ema.set.length))$study.day,
         function(j) abline(v = j, col = grey(0, 0.3)))
  meanjb <- mean(d$jbsteps, na.rm = TRUE)
  abline(h = meanjb, col = grey(0, 0.3))
  with(d, points(study.day, jbsteps, type = "l"))
  with(d, points(study.day, gfsteps, type = "l", lty = "dotted"))
  at <- c(0, with(d, study.day[(na.jbsteps & !lag1.na.jbsteps)
                               | (!na.jbsteps & lag1.na.jbsteps)][-1]), maxd)
  axis(1, at = at)
  axis(2, at = sort(round(c(0, meanjb, maxs))))
}

save(intake, users, daily, daily.plot, file = "analysis.RData")
