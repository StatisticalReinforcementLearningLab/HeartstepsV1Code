## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")
file <- "analysis.RData"

## drop Unix time, POSIX elements when locale is unknown
## time zone fix 10-28-2015

## --- infer EMA response time zone from other EMA data frames
temp <-
  Reduce(function(x, y) merge(x, y, all = TRUE, by = "contextid"),
         list(with(notify,
                   data.frame(contextid, notified.timezone = timezone,
                              notified.tz = tz, notified.gmtoff = gmtoff)),
              with(subset(engage, !duplicated(contextid)),
                   data.frame(contextid, engaged.timezone = timezone,
                              engaged.tz = tz, engaged.gmtoff = gmtoff)),
              with(plan,
                   data.frame(contextid, plan.timezone = timezone,
                              plan.tz = tz, plan.gmtoff = gmtoff)),
              with(complete,
                   data.frame(contextid, complete.timezone = timezone,
                              complete.tz = tz, complete.gmtoff = gmtoff))))
temp$min.gmtoff <- with(temp, pmin(notified.gmtoff, engaged.gmtoff, plan.gmtoff,
                                   complete.gmtoff, na.rm = TRUE))
temp$max.gmtoff <- with(temp, pmax(notified.gmtoff, engaged.gmtoff, plan.gmtoff,
                                   complete.gmtoff, na.rm = TRUE))

## no EMA context
write.data(subset(temp, is.na(notified.tz) & is.na(engaged.tz)),
           file = "checks/missing_ema_context.csv")

## EMA saddling different time zones or DST settings
write.data(subset(temp, min.gmtoff != max.gmtoff),
           file = "checks/ema_multiple_tzdst.csv")
## if no instances of this, just take the time zone of completion
ema <- merge(ema, subset(complete, select = c(contextid, timezone, tz, gmtoff)),
             by = "contextid", all.x = TRUE)

ema$message.utime <- with(ema, char2utime(message.time, gmtoff))
ema$utime.stamp <- with(ema, char2utime(time.stamp, gmtoff))

ema <- cbind(ema,
             with(ema, char2calendar(message.time, tz, prefix = "message.time")),
             with(ema, char2calendar(time.stamp, tz, prefix = "time.stamp")))

## --- aggregate EMA data frame from EMA-question to EMA level

## minimum and maximum message and stamp times
temp <- subset(ema, select = c(user, contextid, message.time, time.stamp))
temp.min <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[1])
temp.max <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[length(x)])
names(temp.min)[3:4] <- paste("min", names(temp.min)[3:4], sep = ".")
names(temp.max)[3:4] <- paste("max", names(temp.max)[3:4], sep = ".")
temp <- merge(temp.min, temp.max, by = c("user", "contextid"))

## response strings to indicators (where possible)
ema$hectic <- with(ema, as.numeric(ifelse(question == "1", response, NA)))
ema$stress <- with(ema, as.numeric(ifelse(question == "2", response, NA)))
ema$typical <- with(ema, as.numeric(ifelse(question == "3", response, NA)))
ema$energy <-
  with(ema, as.numeric(ifelse(question == "research3", response, NA)))
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
ema <- merge(temp, ema, by = c("user", "contextid"), sort = TRUE)
dim(ema)


save(ema, file = file)
