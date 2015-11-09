## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")

max.date <- as.Date("2015-10-31")

## --- daily data

## users
users <- merge(subset(timeslot, !duplicated(user),
                      select = c(user, date.updated, ema, tz, gmtoff)),
               aggregate(message.date ~ user, data = ema,
                         function(x) min(max(x), max.date)),
               by = "user", all = TRUE)
names(users)[c(2, 3, ncol(users))] <- c("intake.date", "ema.slot", "last.date")
users <- subset(users, user != 12 & intake.date < last.date)

## expand to user-day level
daily <- do.call("rbind", sapply(1:nrow(users), function(r)
  with(users[r, , drop = FALSE],
       data.frame(user = user, tz = tz, gmtoff = gmtoff, ema.slot = ema.slot,
                  intake.date = intake.date,
                  study.date = seq(intake.date, last.date, by = "days"))),
  simplify = FALSE))
daily$study.day <- with(daily,
                        as.numeric(difftime(study.date, intake.date,
                                            units = "days")))

## EMA (partial) completion status
daily <- merge(daily,
               aggregate(ema.set.length ~ user + message.date, data = ema, max),
               by.x = c("user", "study.date"), by.y = c("user", "message.date"),
               all.x = TRUE)

## planning status
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
