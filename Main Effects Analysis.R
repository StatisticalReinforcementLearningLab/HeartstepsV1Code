### MAIN EFFECTS ANALYSIS FOR HEARTSTEPS ###

##### Setup #####

## Load helper functions and data frames
source("init.R")
setwd(sys.var$mbox.data)
# load("csv.RData")
load("analysis-small.RData")
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
                    jbsteps60pre.log, jbsteps60.log))
  return(list(data = d, ids = ids))
}
days <- 0:35
primary <- analysis.data(days = days)
ids     <- primary$ids
primary <- primary$data

#### Describe missingness of Jawbone data #####

## Get list of dates for which each user has Jawbone data. Returns a data frame
## in which the "start.udate" variable is a list of dates for each user (NOTE 
## that the names of the list entries are NOT user IDs!)
user.days <- aggregate(start.udate ~ user, data = jawbone, FUN = unique)

## Find number of days (excluding travel) for which each user has no Jawbone data.
## Note that the names of objects in user.days$start.udate have a leading zero.
user.nojb <- sapply(ids, function(x) {
  index <- which(user.days$user == x)
  sum(!(unique(suggest$study.date[suggest$user == x & suggest$travel == F]) %in% 
          user.days$start.udate[[ifelse(index < 10, paste0('0', index), paste(index))]]))
})

# use primary data.frame instead of suggest (above)
user.nojb2 <- sapply(ids, function(x) {
  index <- which(user.days$user == x)
  sum(!(unique(primary$study.date[primary$user == x]) %in% 
          user.days$start.udate[[ifelse(index < 10, paste0('0', index), paste(index))]]))
})

sapply(ids, function(x) {
  index <- which(user.days$user == x)
  length(unique(primary$study.date[primary$user == x]))
})

##### Descriptive Plots #####

## Bar chart for number of days on which step count is completely missing/zero for
## each participant. Height of bar is number of days with no step count data,
## bar labels are percent of total days on study.
text(barplot(height = user.nojb, names.arg = ids, ylim = c(0, 27),
             xlab = "User", ylab = "Days Missing Step Count"),
     user.nojb + 1,
     labels = sapply(as.character(round(user.nojb / 
                                          aggregate(study.day.nogap ~ user, data = primary,
                                                    FUN = max)$study.day.nogap * 100, 1)),
                     function(x) ifelse(x != "0", paste0(x, "\\%"), "")),
     cex = .6)
missing.step.plot <- recordPlot()

## Average proximal step counts by day, user, and whether or not a suggestion
## was delivered (only for six weeks, and only among available times)
x <- aggregate(jbsteps30.zero ~ send + study.day.nogap + user,
               data = subset(suggest, !is.na(study.day.nogap) & study.day.nogap <= 41 &
                               user %in% ids & avail == T),
               FUN = mean)
x <- x[with(x, order(user, study.day.nogap, send)), ]
x$jbsteps30.log <- log(x$jbsteps30.zero + .5)
y <- aggregate(jbsteps30.zero ~ study.day.nogap + user, data = x, FUN = diff)
y1 <- aggregate(jbsteps30.log ~ study.day.nogap + user, data = x, FUN = diff)

## Spaghetti plot of mean difference in proximal step count 
## (send vs. no send) by user
with(y[which(sapply(y$jbsteps30.zero, length) == 1), ],
     interaction.plot(x.factor = study.day.nogap, trace.factor = user,
                      response = as.numeric(jbsteps30.zero),
                      ylim = c(-3000, 3000), legend = F, cex.axis = .6, 
                      xlab = "Study Day (excluding travel)",
                      ylab = "Mean Difference in Proximal Step Count"))
spaghetti.plot <- recordPlot()

## Plot of mean difference in proximal step count (send vs. no send) averaged
## over all users
y1 <- aggregate(as.numeric(jbsteps30.log) ~ study.day.nogap, data = y1, FUN = mean)
names(y1) <- c("day", "step.diff")
with(y1, scatter.smooth(step.diff ~ day, type = "l", span = 2/3,
                        xlab = "Study Day (excluding travel)",
                        ylab = "Mean Difference in Log of Proximal Step Count",
                        lpars = list(lwd = 2, col = color)))
abline(0, 0, col = "gray50")
spaghetti.mean.plot <- recordPlot()

## Compute number of person-days on which either a suggestion was sent
## at every available decision time or no suggestions were sent 
## z is "NaN" if person was never available)
z <- apply(y[which(sapply(y$jbsteps30.zero, length) == 0), ], 1,
           function(x){
             sum(with(suggest, 
                      send[study.day.nogap == x[1] & user == x[2] & avail == T]), na.rm = T) /
               sum(with(suggest, avail[study.day.nogap == x[1] & user == x[2]]), na.rm = T)
           })

rm(x, y, y1, z)

# Create plot of percent unavailability at each decision point
## Count number of people available at each decision index 
y <- aggregate(avail ~ decision.index.nogap,  data = primary, FUN = sum)
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

plot(y$percent.unavail ~ y$decision.index.nogap, type = "l",
     xlim = c(0, 209), ylim = c(0, .5), lwd = 2,
     xlab = "Decision Point", ylab = "Proportion of Participants")
lines(y$percent.intransit ~ y$decision.index.nogap, type = "l", xlim = c(0,209), col = color)
legend("topright", legend = c("Any-Cause Unavailability", "Unavailbility due to Walking"), 
       lwd = c(2, 1), col = c("black", color))

avail.plot <- recordPlot()
rm(y)


##### Model Fitting #####

## Define function to format variables for modeling, call geeglm(), and 
## make small-sample variance corrections (uses xgeepack.R functions)

## Model 1: No time effect
model1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6),
                 id = user, weights = as.numeric(avail), 
                 scale.fix = T, data = primary)
# estimate(model1, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                I(send - .6) + study.day.nogap:I(send - .6),
                id = user, weights = as.numeric(avail), 
                scale.fix = T, data = primary)
# estimate(model2, normal = FALSE)


##### Model Fit Plots #####
# y <- aggregate(jbsteps30.log ~ study.day.nogap + send, 
#                data = subset(primary, avail == T), 
#                FUN = function(x) exp(mean(x)) - 0.5)
# with(subset(y, send == F), plot(jbsteps30.log ~ study.day.nogap, type = "l",
#                                 lwd = 1, col = "black"))
# with(subset(y, send == T), lines(jbsteps30.log ~ study.day.nogap, 
#                                  lwd = 2, col = color))
# 
# y1 <- cbind(Intercept = 1,
#            jbsteps30pre.log = 0,
#            study.day.nogap = 0:41,
#            send = 0.4,
#            interaction = 0.4 * 0:41)
# y2 <- data.frame(estimate(model2, combos = y1, normal = F),
#                  study.day.nogap = 0:41)
# with(y2, lines(Estimate ~ study.day.nogap, col = color, lwd = 2))
# y1 <- cbind(Intercept = 1,
#             jbsteps30pre.log = 0,
#             study.day.nogap = 0:41,
#             send = -0.6,
#             interaction = -0.6 * 0:41)
# y2 <- data.frame(estimate(model2, combos = y1, normal = F),
#                  study.day.nogap = 0:41)
# with(y2, lines(Estimate ~ study.day.nogap))

##### Time Trend Analysis #####

## Minimal model: intercept and step count in prior 30 minutes. First, use all
## participants, then split by treated/untreated
minmod       <- geeglm(jbsteps30.log ~ jbsteps30pre.log,
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = primary)
minmod.send0 <- geeglm(jbsteps30.log ~ jbsteps30pre.log, 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T,
                       data = subset(primary, send == F))
minmod.send1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log,
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T,
                       data = subset(primary, send == T))

### LOESS plots: Raw residuals

span <- c(.09, .3)
sample <- sample(1:length(minmod$residuals), ceiling(.4 * length(minmod$residuals)))
par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)

## Main effect of day on study
for (i in 1:length(span)) {
  plot(minmod$residuals[sample] ~ primary$study.day.nogap[sample], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 41))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(with(subset(primary), 
             predict(loess(minmod$residuals ~ study.day.nogap,
                           span = span[i]))),
        col = color, lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}
loess.plot.main <- recordPlot()

## Interaction of day on study with treatment
par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)
for (i in 1:length(span)) {
  plot(minmod$residuals[sample] ~ primary$study.day.nogap[sample], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 41))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(with(subset(primary), 
             predict(loess(minmod.send1$residuals ~ 
                             study.day.nogap[primary$send == T],
                           span = span[i]),
                     newdata = seq(0, 41)) - 
               predict(loess(minmod.send0$residuals ~
                               study.day.nogap[primary$send == F],
                             span = span[i]),
                       newdata = seq(0, 41))),
        col = color, lwd = 6)
  abline(0, 0, col = "red", lwd = 2)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}
loess.plot.intr <- recordPlot()

### LOESS Plots: Mean residuals
par(mfrow = c(1, 1), mar = c(3, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0),
    oma = rep(0, 4), las = 1, tcl = 0.25)

resids <- subset(primary, select = c("jbsteps30.zero", "jbsteps30pre.zero",
                                     "study.day.nogap", "send", "avail"))

resids$resid.full <- with(resids, log(jbsteps30.zero + .5) - coef(minmod)[1] -
                            coef(minmod)[2] * log(jbsteps30pre.zero + .5))

resids$resid.send <- NA
resids$resid.send[resids$send == T] <- minmod.send1$residuals
resids$resid.send[resids$send == F] <- minmod.send0$residuals

y <- aggregate(resid.full ~ study.day.nogap,
               data = subset(resids, avail == T & study.day.nogap <= 41),
               FUN = mean)
with(y, scatter.smooth(resid.full ~ study.day.nogap, type = "l", span = 1/3,
                       xlab = "Study Day (excluding travel)",
                       ylab = "Mean Residual",
                       lpars = list(lwd = 2, col = color)))
abline(0, 0, col = "grey50")

mean.resid.main.effect <- recordPlot()

x <- aggregate(resid.send ~ send + study.day.nogap, 
               data = subset(resids, avail == T), FUN = mean)
y <- aggregate(resid.send ~ study.day.nogap, 
               data = subset(x, study.day.nogap <= 41), FUN = diff)

with(y, scatter.smooth(resid.send ~ study.day.nogap, type = "l", span = 1/3,
                       xlab = "Study Day (excluding travel)",
                       ylab = "Mean Difference in Residual",
                       lpars = list(lwd = 2, col = color)))
abline(0, 0, col = "grey50")

mean.resid.interaction <- recordPlot()

### LOESS Plots: Mean residuals with quadratic time
primary$study.day.nogap.sq <- primary$study.day.nogap ^ 2
quadmod       <- geeglm(jbsteps30.log ~ jbsteps30pre.log + 
                      study.day.nogap + study.day.nogap.sq,
                      id = user, weights = as.numeric(avail), 
                      scale.fix = T, data = primary)
quadmod.send0 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + 
                          study.day.nogap + study.day.nogap.sq, 
                        id = user, weights = as.numeric(avail), 
                        scale.fix = T,
                        data = subset(primary, send == F))
quadmod.send1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + 
                          study.day.nogap + study.day.nogap.sq,
                        id = user, weights = as.numeric(avail), 
                        scale.fix = T,
                        data = subset(primary, send == T))

resids <- cbind(resids[, 1:3], 
                study.day.nogap.sq = primary$study.day.nogap.sq,
                resids[, 4:7])

resids$resid.full <- quadmod$residuals

resids$resid.send <- NA
resids$resid.send[resids$send == T] <- quadmod.send1$residuals
resids$resid.send[resids$send == F] <- quadmod.send0$residuals

y <- aggregate(resid.full ~ study.day.nogap,
               data = subset(resids, avail == T & study.day.nogap <= 41),
               FUN = mean)
with(y, scatter.smooth(resid.full ~ study.day.nogap, type = "l", span = 1/3,
                       xlab = "Study Day (excluding travel)",
                       ylab = "Mean Residual",
                       lpars = list(lwd = 2, col = color)))
abline(0, 0, col = "grey50")

mean.resid.main.effect.quad <- recordPlot()

x <- aggregate(resid.send ~ send + study.day.nogap, 
               data = subset(resids, avail == T), FUN = mean)
y <- aggregate(resid.send ~ study.day.nogap, 
               data = subset(x, study.day.nogap <= 41), FUN = diff)

with(y, scatter.smooth(resid.send ~ study.day.nogap, type = "l", span = 1/3,
                       xlab = "Study Day (excluding travel)",
                       ylab = "Mean Difference in Residual",
                       lpars = list(lwd = 2, col = color)))
abline(0, 0, col = "grey50")

mean.resid.interaction.quad <- recordPlot()

##### Active vs. Sedentary Suggestions #####
model3 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap + 
                   I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                   study.day.nogap:I(send.active - 0.3) + 
                   study.day.nogap:I(send.sedentary - 0.3),
                 id = user, weights = as.numeric(avail),
                 data = primary, scale.fix = T)
# estimate(model3, normal = FALSE)

model4 <-  geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                    I(send.sedentary - 0.3),
                  id = user, weights = as.numeric(avail),
                  data = primary, scale.fix = T)

##### Sensitivity Analyses #####
### Remove ID 35
sens1 <- subset(primary, user != 35)

## Model 1: No time effect
model1.sens1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens1)
# estimate(model1.sens1, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                      I(send - .6) + study.day.nogap:I(send - .6),
                    id = user, weights = as.numeric(avail), 
                    scale.fix = T, data = sens1)
# estimate(model2.sens1, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens1)
# estimate(model3.sens1, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens1 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens1, scale.fix = T)

### Require 37 days on study
sens2 <- subset(analysis.data(0:36)$data, user != 35)

## Model 1: No time effect
model1.sens2 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens2)
# estimate(model1.sens2, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens2 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send - .6) + study.day.nogap:I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens2)
# estimate(model2.sens2, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens2 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens2)
# estimate(model3.sens2, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens2 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens2, scale.fix = T)

### Require 38 days on study
sens3 <- subset(analysis.data(0:37)$data, user != 35)

## Model 1: No time effect
model1.sens3 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens3)
# estimate(model1.sens3, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens3 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send - .6) + study.day.nogap:I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens3)
# estimate(model2.sens3, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens3 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens3)
# estimate(model3.sens3, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens3 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens3, scale.fix = T)

### Require 41 days on study
sens4 <- subset(analysis.data(0:40)$data, user != 35)

## Model 1: No time effect
model1.sens4 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens4)
# estimate(model1.sens4, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens4 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send - .6) + study.day.nogap:I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens4)
# estimate(model2.sens4, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens4 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens4)
# estimate(model3.sens4, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens4 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens4, scale.fix = T)

### Require 42 days on study
sens5 <- subset(analysis.data(0:41)$data, user != 35)

## Model 1: No time effect
model1.sens5 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens5)
# estimate(model1.sens5, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens5 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send - .6) + study.day.nogap:I(send - .6),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens5)
# estimate(model2.sens5, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens5 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens5)
# estimate(model3.sens5, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens5 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens5, scale.fix = T)

### Remove ID 14's original travel dates
## 9/18/15 - 9/24/15
sens6 <- primary
u14travel <- seq.Date(from = as.Date("2015-09-18"), 
                      to = as.Date("2015-09-24"),
                      by = "day")
sens6$study.day.nogap[sens6$user == 14 & sens6$study.date %in% u14travel] <- NA
sens6$study.day.nogap[sens6$user == 14 & sens6$study.date >= as.Date("2015-09-25")] <-
  sens6$study.day.nogap[sens6$user == 14 & sens6$study.date >= as.Date("2015-09-25")] - 7
sens6 <- subset(sens6, !is.na(study.day.nogap))

## Model 1: No time effect
model1.sens6 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens6)
# estimate(model1.sens4, normal = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2.sens6 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send - .6) + study.day.nogap:I(send - .6), 
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens6)
# estimate(model2.sens4, normal = FALSE)

## Model 3: Active and sedentary suggestions
model3.sens6 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + study.day.nogap +
                         I(send.active - 0.3) + I(send.sedentary - 0.3) + 
                         study.day.nogap:I(send.active - 0.3) + 
                         study.day.nogap:I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail), 
                       scale.fix = T, data = sens6)
# estimate(model3.sens4, normal = FALSE)

## Model 4: Active and sedentary suggestions marginal over time
model4.sens6 <- geeglm(jbsteps30.log ~ jbsteps30pre.log + I(send.active - 0.3) +
                         I(send.sedentary - 0.3),
                       id = user, weights = as.numeric(avail),
                       data = sens6, scale.fix = T)