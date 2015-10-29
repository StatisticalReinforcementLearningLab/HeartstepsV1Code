## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")
file <- "analysis.RData"

users <- sort(user$user)

## --- daily data

## taking intake and exit interview dates, generate sequence of study dates
## (which we can safely consider in UTC, since EST is 4-5 hours behind UTC)
daily <- do.call("rbind", sapply(1:nrow(user), function(x)
  with(user[x, , drop = FALSE],
       data.frame(user = user,
                  intake.date = intake.date,
                  study.udate = seq(intake.date, exit.date, by = "days"))),
  simplify = FALSE))
daily$study.day <- with(daily, as.numeric(difftime(study.udate, intake.date,
                                                   units = "days")))

## EMA completion status
## FIXME: base this on EMA response instead?

## EMA is based on local time, so match on that
## FIXME: does this make sense?
## FIXME: we have recurrent completion time stamps!
complete$date.stamp <- char2date(complete$time.stamp)
daily <- merge(daily,
               subset(complete, select = c(user, date.stamp, completed)),
               by.x = c("user", "study.udate"), by.y = c("user", "date.stamp"),
               all.x = TRUE, sort = TRUE)

daily <- subset(daily, !duplicated(cbind(user, study.day)))

## step counts
daily <- merge(daily,
               aggregate(steps ~ user + end.udate, data = jawbone, sum),
               by.x = c("user", "study.udate"), by.y = c("user", "end.udate"),
               all.x = TRUE, sort = TRUE)
daily$na.steps <- is.na(daily$steps)
daily$lag1.na.steps <- with(daily, delay(user, study.day, na.steps))

max.day <- max(daily$study.day)

daily.plot <- function(u) {
  d <- subset(daily, user == u)
  m <- max(1, d$steps, na.rm = TRUE)
  n <- max(d$study.day)
  plot(NULL, xlim = c(0, n), ylim = c(0, m),
       xlab = "", ylab = "", main = "", axes = FALSE, frame.plot = FALSE)
  mtext(paste("User", u), 2, line = 2, cex = 0.75)
  sapply(subset(d, !completed)$study.day,
         function(j) rect(j - 1, 0, j, m, col = grey(0, 0.2), border = NA))
  sapply(subset(d, study.udate > last.date)$study.day,
         function(j) rect(j - 1, 0, j, m, col = grey(0, 0.05), border = NA))
  with(d, points(study.day, steps, type = "l"))
  at <- c(0, with(d, study.day[(na.steps & !lag1.na.steps)
                               | (!na.steps & lag1.na.steps)][-1]), n)
  axis(1, at = at)
  axis(2, at = c(0, m))
}

## minimum and maximum message and stamp times
temp <- subset(ema, select = c(user, contextid, message.utime, utime.stamp))
temp.min <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[1])
temp.max <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[length(x)])
names(temp.min)[3:4] <- paste("min", names(temp.min)[3:4], sep = ".")
names(temp.max)[3:4] <- paste("max", names(temp.max)[3:4], sep = ".")
temp <- merge(temp.min, temp.max, by = c("user", "contextid"))

## response strings to indicators (where possible)
ema$hectic <- with(ema, as.numeric(ifelse(question == "1", response, NA)))
ema$stress <- with(ema, as.numeric(ifelse(question == "2", response, NA)))
ema$typical <- with(ema, as.numeric(ifelse(question == "3", response, NA)))
ema$energy <- with(ema, as.numeric(ifelse(question == "research3",
                                          response, NA)))
ema$urge <- with(ema, as.numeric(ifelse(question == "research4", response, NA)))
ema$follow <- with(ema, ifelse(question == "5", response, NA))
ema$msg.down <- with(ema, ifelse(question == "6", message, NA))
ema$msg.up <- with(ema, ifelse(question == "7", message, NA))

ema <- cbind(ema,
             match.option(ema4, ema$response,
                          ema$question == "4", "active", FALSE),
             match.option(ema6, ema$response, ema$question == "6", "down"),
             match.option(ema7, ema$response, ema$question == "7", "up"),
             match.option(research1, ema$response,
                          ema$question == "research1", "barrier"),
             match.option(research2, ema$response,
                          ema$question == "research2", "enabler"))

## order in which each question was asked
ema$order.hectic <- with(ema, ifelse(question == "1", order, NA))
ema$order.stress <- with(ema, ifelse(question == "2", order, NA))
ema$order.typical <- with(ema, ifelse(question == "3", order, NA))
ema$order.active <- with(ema, ifelse(question == "4", order, NA))
ema$order.follow <- with(ema, ifelse(question == "5", order, NA))
ema$order.down <- with(ema, ifelse(question == "6", order, NA))
ema$order.up <- with(ema, ifelse(question == "7", order, NA))
ema$order.barrier <- with(ema, ifelse(question == "research1", order, NA))
ema$order.enabler <- with(ema, ifelse(question == "research2", order, NA))
ema$order.energy <- with(ema, ifelse(question == "research3", order, NA))
ema$order.urge <- with(ema, ifelse(question == "research4", order, NA))

dim(temp)
ema <- aggregate(subset(ema, select = c(hectic:order.urge)),
                 subset(ema, select = c(user, contextid)),
                 function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)][1]))
dim(ema)
temp <- merge(temp, ema, by = c("user", "contextid"), sort = TRUE)
dim(temp)

save(user, users, max.day, daily.plot, intake, daily, ema, file = file)
