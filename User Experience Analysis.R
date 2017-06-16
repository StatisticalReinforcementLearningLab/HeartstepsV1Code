### Descriptive Analyses for HeartSteps User Experience and Engagement Paper ###

library(lme4)
source("init.R")
setwd(sys.var$mbox.data)
load("analysis.RData")
setwd(sys.var$repo)

## Formatting choices
par(mar = c(3, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), las = 1, tcl = 0.25)
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
mean(users$age[users$user %in% ids])
sum(users$age[users$user %in% ids] <= 25)
table(users$gender[users$user %in% ids]) / length(ids)
table(users$ethnicity[users$user %in% ids]) / length(ids)
table(users$education[users$user %in% ids]) / length(ids)
table(users$marital[users$user %in% ids]) / length(ids)

table(users$fittracker[users$user %in% ids])
table(users$fitapp[users$user %in% ids])

mean(users$vigact.days.intake[users$user %in% ids])
sd(users$vigact.days.intake[users$user %in% ids])
mean(users$modact.days.intake[users$user %in% ids])
sd(users$modact.days.intake[users$user %in% ids])

sum(users$ipaq.minimal.intake[users$user %in% ids & !is.na(users$ipaq.minimal.intake)])
sum(users$ipaq.hepa.intake[users$user %in% ids & !is.na(users$ipaq.hepa.intake)])
with(subset(users, user %in% ids), table(ipaq.hepa.intake, ipaq.minimal.intake))


sum(users$own.phone[users$user %in% ids])

# Find users' last days on study
x <- aggregate(study.day.nogap ~ user, data = subset(daily, !is.na(daily$jbsteps.direct)), max)

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
  daily$jbsteps.direct[daily$study.day.nogap == x$study.day.nogap[x$user == u] - 1 & !is.na(daily$study.day.nogap) & daily$user == u]
  }, simplify = TRUE)

# Number of days for which we have Jawbone data
stepdays <- sapply(ids, function(i) sum(!is.na(daily$jbsteps.direct[daily$user == i & daily$study.day.nogap <= x$study.day.nogap[x$user == i]])))
# Merge daily and user data frames to link baseline data with daily step counts
daily2 <- merge(daily, users, by = "user", all = T)

# Number of total EMAs delivered
emacount <- aggregate(ema.set.length ~ user, data = subset(daily, user %in% ids & !is.na(study.day.nogap) & study.day.nogap <= 41), function(x) sum(!is.na(x)))

##### Mixed Effects Modeling #####
summary(aov(I(log(jbsteps.direct)) ~ ipaq.hepa.intake + ipaq.minimal.intake, data = subset(daily2, !is.na(study.day.nogap) & study.day.nogap == 1 & user %in% ids)))

