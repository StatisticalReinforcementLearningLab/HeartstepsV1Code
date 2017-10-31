### Descriptive Analyses for HeartSteps User Experience and Engagement Paper ###

library(lme4)
library(grDevices)
source("init.R")
setwd(sys.var$mbox.data)
load("analysis.RData")
load("csv.RData")
setwd(sys.var$repo)

## Formatting choices
par(mar = c(3, 4, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), las = 1, tcl = 0.25)
color <- "royalblue1"

## Decide minimum time on study for inclusion in analysis
## and subset data to exclude participants who don't meet the threshold. 
analysis.data <- function(days = 0:35, max.day = 41) {
  ids  <- unique(suggest$user[suggest$study.day.nogap == rev(days)[1] &
                                !is.na(suggest$study.day.nogap)])
  d <- subset(suggest, !is.na(study.day.nogap) & user %in% ids & 
                !(avail == F & send == T) & study.day.nogap <= max.day &
                !is.na(send.active),
              select = c(user, study.day.nogap, decision.index.nogap, decision.utime,
                         slot, study.date, intake.date, intake.utime, intake.slot,
                         travel.start, travel.end, exit.date, dropout.date,
                         last.date, last.utime, last.slot, recognized.activity,
                         avail, send, send.active, send.sedentary, jbsteps10, 
                         jbsteps10.zero, jbsteps10.log, jbsteps30pre,
                         jbsteps30, jbsteps30pre.zero, jbsteps30.zero, 
                         jbsteps30pre.log, jbsteps30.log, jbsteps60pre,
                         jbsteps60, jbsteps60pre.zero, jbsteps60.zero,
                         jbsteps60pre.log, jbsteps60.log, response, location.category))
  return(list(data = d, ids = ids))
}
days <- 0:35
primary <- analysis.data(days = days)
ids     <- primary$ids
primary <- primary$data

##### Descriptives #####

# Participant demographics
mean(users$age[users$user %in% ids])
sum(users$age[users$user %in% ids] <= 25)
table(users$gender[users$user %in% ids]) / length(ids)
table(users$ethnicity[users$user %in% ids]) / length(ids)
table(users$education[users$user %in% ids]) / length(ids)
table(users$marital[users$user %in% ids]) / length(ids)

# Number of participants who've used a fitness tracker or fitness app previously
table(users$fittracker[users$user %in% ids])
table(users$fitapp[users$user %in% ids])

# Summarize activity at the day level
mean(users$vigact.days.intake[users$user %in% ids])
sd(users$vigact.days.intake[users$user %in% ids])
mean(users$modact.days.intake[users$user %in% ids])
sd(users$modact.days.intake[users$user %in% ids])

# Tabulate IPAQ activity levelsl
sum(users$ipaq.minimal.intake[users$user %in% ids & !is.na(users$ipaq.minimal.intake)])
sum(users$ipaq.hepa.intake[users$user %in% ids & !is.na(users$ipaq.hepa.intake)])
with(subset(users, user %in% ids), table(ipaq.hepa.intake, ipaq.minimal.intake))

# Number of participants using their own phones
sum(users$own.phone[users$user %in% ids])

# Get median step count 

dailyValid <- quote(subset(daily, !is.na(daily$jbsteps.direct)))
# Find users' last days on study
x <- aggregate(study.day.nogap ~ user, data = eval(dailyValid), max)
# Roll back 7 days and take the median
medSteps.last7 <- do.call("rbind", lapply(split.data.frame(eval(dailyValid), list(eval(dailyValid)$user)),
                                          function(d) {
                                            lastDay <- x$study.day.nogap[x$user == unique(d$user)]
                                            out <- data.frame("user" = unique(d$user),
                                                              "medSteps" = median(d$jbsteps.direct[d$study.day.nogap %in% seq(lastDay - 7, lastDay - 1)]))
                                            names(out) <- c("user", "medSteps")
                                            out
                                          }))

# Find median step count across first 7 days on-study
medSteps.first7 <- do.call("rbind", lapply(split.data.frame(eval(dailyValid), list(eval(dailyValid)$user)),
                                          function(d) {
                                            lastDay <- x$study.day.nogap[x$user == unique(d$user)]
                                            out <- data.frame("user" = unique(d$user),
                                                              "medSteps" = median(d$jbsteps.direct[d$study.day.nogap %in% seq(0:6)]))
                                          }))

# Compute difference in median step counts across first and last 7 days on-study
medSteps <- merge(medSteps.first7, medSteps.last7, by = "user", suffixes = c(".first", ".last"))
medSteps$diff <- with(medSteps, medSteps.last - medSteps.first)
summary(medSteps$diff)

# Compute difference between daily step count on first full day of study and last full day of study
y <- sapply(unique(daily$user[daily$user %in% ids]), function(u) {
  daily$jbsteps.direct[daily$study.day.nogap == x$study.day.nogap[x$user == u] - 1 &
                         !is.na(daily$study.day.nogap) & daily$user == u] -
    daily$jbsteps.direct[daily$study.day.nogap == 1 &
                           !is.na(daily$study.day.nogap) & daily$user == u]
}, simplify = TRUE)
y <- unlist(y)
barplot(y)
median(y, na.rm = T)

# Compute daily step count on last full day of study
z <- sapply(unique(daily$user[daily$user %in% ids]), function(u) {
  daily$jbsteps.direct[daily$study.day.nogap == x$study.day.nogap[x$user == u] - 1 &
                         !is.na(daily$study.day.nogap) & daily$user == u]
}, simplify = TRUE)

# Number of days for which we have Jawbone data
stepdays <- sapply(ids, function(i) sum(!is.na(daily$jbsteps.direct[daily$user == i & 
                                                                      daily$study.day.nogap <= x$study.day.nogap[x$user == i]])))
# Merge daily and user data frames to link baseline data with daily step counts
daily2 <- merge(daily, users, by = "user", all = T)

##### Modeling #####
summary(lm(I(log(jbsteps.direct)) ~ ipaq.hepa.intake + ipaq.minimal.intake, data = subset(daily2, !is.na(study.day.nogap) & study.day.nogap == 1 & user %in% ids)))
summary(lm(I(log(jbsteps.direct)) ~ selfeff.intake, data = subset(daily2, !is.na(study.day.nogap) & study.day.nogap == 1 & user %in% ids)))

# Mixed effects models for IPAQ moderation
d <- subset(daily2, user %in% ids & study.day.nogap == 1, select = c('user', 'jbsteps.direct', 'ipaq.hepa.intake', 'ipaq.minimal.intake', 'selfeff.intake'))
d <- cbind(d, "time" = 0)

d1 <- with(subset(users, user %in% ids), data.frame("user" = ids, "jbsteps.direct" = z,
                                                    ipaq.hepa.intake, ipaq.minimal.intake, selfeff.intake, "time" = 1))
d <- rbind(d, d1)

users$stepchange <- NA
users$stepchange[users$user %in% ids] <- y
mod1 <- lmer(log(jbsteps.direct + .5) ~ (1|user) + ipaq.hepa.intake + ipaq.minimal.intake + time + 
               time * ipaq.hepa.intake + time * ipaq.minimal.intake, data = d)
mod2 <- lmer(log(jbsteps.direct + .5) ~ (1|user) + ipaq.hepa.intake + 
               ipaq.minimal.intake + study.day.nogap + study.day.nogap * ipaq.hepa.intake + 
               study.day.nogap * ipaq.minimal.intake, 
             data = subset(daily2, user %in% ids & !is.na(study.day.nogap) & study.day.nogap %in% 1:41))
mod3 <- lmer(log(jbsteps.direct + .5) ~ (1|user) + selfeff.intake + study.day.nogap + 
               study.day.nogap * selfeff.intake, 
             data = subset(daily2, user %in% ids & !is.na(study.day.nogap) & study.day.nogap %in% 1:41))

##### App Usage by Session #####
# Create a session-level data.frame for app usage, rather than screen view-level
# session must be at least 2 seconds long; screen visit must be 2 seconds as well
appuse <- aggregate(start.time ~ user + session.index, data = usage, function(x) x[1])
appuse <- merge(appuse, aggregate(end.time ~ user + session.index, data = usage, FUN = function(x) x[length(x)]), by = c('user', 'session.index'))
appuse <- merge(appuse, aggregate(tz ~ user + session.index, data = usage, function(x) x[1]), by = c('user', 'session.index'))
appuse <- merge(appuse, aggregate(gmtoff ~ user + session.index, data = usage, function(x) x[1]), by = c('user', 'session.index'))
appuse <- subset(appuse, user %in% ids)
appuse <- appuse[order(appuse$user, appuse$session.index), ]
appuse$start.date <- char2date(appuse$start.time)
appuse$end.date <- char2date(appuse$end.time)
appuse <- cbind(appuse,
                "start.utime" = char2utime(appuse$start.time, offset = appuse$gmtoff),
                "end.utime" = char2utime(appuse$end.time, offset = appuse$gmtoff),
                char2calendar(appuse$start.time, tz = appuse$tz, prefix = "start"),
                char2calendar(appuse$end.time, tz = appuse$tz, prefix = "end"))
appuse$duration <- with(appuse, difftime(end.utime, start.utime, units = "secs"))

appdays <- aggregate(rep(1, dim(appuse)[1]) ~ user + start.date, data = appuse, length)
names(appdays)[3] <- "sessions"

# Usage by screen
with(subset(usage, user %in% ids & screen.duration >= 2), table(activity.fragment) / length(activity.fragment))

hist(as.numeric(appuse$duration)[as.numeric(appuse$duration) <= 20],
     main = "Duration of 'Sessions' of HeartSteps App Usage",
     xlab = "Duration (sec)")

##### EMA and Planning #####
# Mean percent of EMAs missed by each person
x <- aggregate((!respond & connect) ~ user, data = subset(daily, user %in% ids & !is.na(study.day.nogap) & study.day.nogap %in% 0:41), sum)
names(x)[2] <- "missed"
summary(x$missed)

with(subset(daily, connect & study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids), table(planning, useNA = 'ifany'))
with(subset(daily, connect & study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids), sum(view[is.na(planning)]))

# Time spent planning
daily.plan <- do.call(rbind,
                      lapply(ids, function(i) {
                        subset(plan, user == i & date.finished %in% unique(daily$study.date[daily$user == i & !is.na(daily$study.day.nogap) & daily$study.day.nogap %in% 0:41]))
                      }))



##### Suggestions #####
sum(primary$avail)
sum(primary$send[primary$avail])
sum(primary$send.active[primary$avail])
sum(primary$send.sedentary[primary$avail])
x <- aggregate(send ~ user + study.day.nogap, data = subset(primary, !is.na(study.day.nogap) & avail), sum)
mean(x$send)
sd(x$send)

##### Plotting #####
### Figure 1: For each day, plot median step count with IQRs as error bars
lastDay <- aggregate(study.day.nogap ~ user, data = subset(daily, !is.na(daily$jbsteps.direct)), max)
lastDay$study.day.nogap[lastDay$study.day.nogap >= 41] <- 41
medsteps <- list("median" = sapply(0:41, function(i) {
  with(subset(daily, study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids), 
       median(jbsteps.direct[study.day.nogap == i], na.rm = T))
}),
"q1" = sapply(0:41, function(i) {
  with(subset(daily, study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids), 
       quantile(jbsteps.direct[study.day.nogap == i], probs = .25, na.rm = T))
}),
"q3" = sapply(0:41, function(i) {
  with(subset(daily, study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids), 
       quantile(jbsteps.direct[study.day.nogap == i], probs = .75, na.rm = T))
})
)

propavail <- sapply(0:41, function(i) {
  with(subset(daily, study.day.nogap %in% 0:41 & !is.na(study.day.nogap) & user %in% ids),
       (sum(planning[!is.na(planning) & study.day.nogap == i] != "disconnected") +  /
         length(unique(user[study.day.nogap == i])))
})

png(filename="C:/Users/nseew/Dropbox/Mobile mHealth/HeartSteps/papers we write/HeartSteps Pilot 1 study/User experience paper/Median-Daily-Stepcount.png",
    width=3.3, height=4, units="in", res=300)
par(mar = c(3, 2.5, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), 
    las = 1, tcl = 0.25, family = "serif", ps = 9)
plot(medsteps$median / 1000 ~ seq(0,41), type = "l", xlab = "", ylab = "", ylim = c(2.9, 14), lwd = 3)
arrows(0:41, medsteps$q1 / 1000, y1 = medsteps$q3 / 1000, angle = 90, code = 3, length = .05)
title(xlab = "Day on study")
title(ylab = "Median daily step count (thousands)")
dev.off()
plot.new()

# Create plot of percent unavailability at each decision point
## Count number of people available at each decision index
y <- aggregate(avail ~ decision.index.nogap, data = primary, FUN = sum)
## Count number of people currently walking at each decision index and merge with above
y <- merge(y, aggregate(recognized.activity ~ decision.index.nogap,
                        data = primary, FUN = function(x) sum(x == "ON_FOOT")),
           by = "decision.index.nogap")
## Merge above with total number of people on-study at each decision point
y <- merge(y, as.data.frame(table(primary$decision.index.nogap)),
           by.x = "decision.index.nogap", by.y = "Var1")
## Compute percent unavailable and percent walking (latter is only among unavailable people)
y <- cbind(y, "percent.unavail" = 1 - y$avail / y$Freq, 
           "percent.intransit" = y$recognized.activity / y$Freq)

png(filename="C:/Users/nseew/Dropbox/Mobile mHealth/HeartSteps/papers we write/HeartSteps Pilot 1 study/User experience paper/Availability-Plot.png",
    width=3.3, height=4, units="in", res=300)
par(mar = c(3, 2.5, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), las = 1, tcl = 0.25, family = "serif", ps = 9)
plot(y$percent.unavail ~ y$decision.index.nogap, type = "l",
     xlim = c(0, 209), ylim = c(0, .5), lwd = 1,
     xlab = "Decision Point", ylab = "Proportion of Participants Unavailable")
lines(y$percent.intransit ~ y$decision.index.nogap, type = "l", xlim = c(0,209), lty = 2, lwd = 1)
legend("topright", legend = c("All-cause", "Walking-specific"), 
       lwd = 1, col = "black", lty = c(1, 2))
dev.off()

## Thumbs up/down plot (code from Tim)
tim_data <- subset(suggest, !is.na(study.day.nogap) & user %in% ids & 
                     !(avail == F & send == T) & avail == T & study.day.nogap %in% 0:41 &
                     !is.na(send.active))
tim_data_sent = subset(tim_data, send == T)

study_day_table = table(tim_data_sent$study.day.nogap, tim_data_sent$response, useNA = "ifany")
denom = rowSums(study_day_table)
print(cbind((study_day_table/denom), denom), digits = 2)
study_day_table2 = study_day_table/denom

x = 0:41
z = loess(x~study_day_table2[,1])

cur_span = .25
png(filename="C:/Users/nseew/Dropbox/Mobile mHealth/HeartSteps/papers we write/HeartSteps Pilot 1 study/User experience paper/Thumbs-plot.png",
    width=3.3, height=4, units="in", res=300)
par(mar = c(3, 2.5, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), las = 1, tcl = 0.25, family = "serif", ps = 9)
plot(predict(loess(study_day_table2[,1]~x, weights = denom, span = cur_span)), 
     ylim = c(0,.6), col = 1, cex = .8, type = 'l', 
     xlab = "Day on Study", ylab = "Percentage of Responses")
points(predict(loess(study_day_table2[,2]~x, weights = denom, span = cur_span)),
       col = "black", type = 'l', lty = 2)
points(predict(loess(study_day_table2[,3]~x, weights = denom, span = cur_span)),
       col = "black", type = 'l', lty = 3)
legend(x = "topright", col = "black", lty = c(1, 2, 3), legend =c("Thumbs-Down", "Thumbs-Up", "Non-Response"))
dev.off()

