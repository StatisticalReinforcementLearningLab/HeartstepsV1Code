### MAIN EFFECTS ANALYSIS FOR HEARTSTEPS ###

##### Setup #####

## Load helper functions and data frames
source("init.R")
setwd(sys.var$mbox.data)
load("csv.RData")
load("analysis.RData")
setwd(sys.var$repo)

## Formatting choices
par(mar = c(3, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0), oma = rep(0, 4), las = 1, tcl = 0.25)
color <- "royalblue1"

##### Time inclusion criteria #####

## Decide minimum time on study for inclusion in analysis
## and subset data to exclude participants who don't meet the threshold. Note that 
## NOTE: to save memory, subsetting is always done on-the-fly: the data frame 
## must be accessed using eval(d). 
days <- 0:35
ids  <- unique(suggest$user[suggest$study.day.nogap == rev(days)[1] &
                              !is.na(suggest$study.day.nogap)])
d    <- quote(subset(suggest, !is.na(study.day.nogap) & user %in% ids))


#### Describe missingness of Jawbone data #####

## Get list of dates for which each user has Jawbone data. Returns a data frame
## in which the "start.udate" variable is a list of dates for each user (NOTE 
## that the names of the list entries are NOT user IDs!)
user.days <- aggregate(start.udate ~ user, data = jawbone, FUN = unique)

## Find number of days (excluding travel) for which each user has no Jawbone data.
## Note that the names of objects in user.days$start.udate have a leading zero.
user.nojb <- sapply(unique(suggest$user), function(x) {
  index <- which(user.days$user == x)
  sum(!(unique(suggest$study.date[suggest$user == x & suggest$travel == F]) %in% 
          user.days$start.udate[[ifelse(index < 10, paste0('0', index), paste(index))]]))
})


##### Plots #####

## Bar chart for number of days on which step count is completely missing/zero for
## each participant. Height of bar is number of days with no step count data,
## bar labels are percent of total days on study.
text(barplot(height = user.nojb, names.arg = unique(suggest$user), ylim = c(0, 27),
             xlab = "User", ylab = "Days Missing Step Count"),
     user.nojb + 1,
     labels = sapply(as.character(round(user.nojb / 
                                          aggregate(study.day.nogap ~ user, data = suggest,
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
y <- aggregate(avail ~ decision.index.nogap,  data = eval(d), FUN = sum)
## Count number of people currently walking at each decision index and merge with above
y <- merge(y, aggregate(recognized.activity ~ decision.index.nogap,
                        data = eval(d), FUN = function(x) sum(x == "ON_FOOT")),
           by = "decision.index.nogap")
## Merge above with total number of people on-study at each decision point
y <- merge(y, as.data.frame(table(eval(d)$decision.index.nogap)),
           by.x = "decision.index.nogap", by.y = "Var1")
## Compute percent unavailable and percent walking (latter is only among unavailable people)
y <- cbind(y, "percent.unavail" = 1 - y$avail / y$Freq, "percent.intransit" = y$recognized.activity / y$Freq)

plot(y$percent.unavail ~ y$decision.index.nogap, type = "l",
     xlim = c(0, 209), lwd = 2,
     xlab = "Decision Point", ylab = "Proportion of Participants")
lines(y$percent.intransit ~ y$decision.index.nogap, type = "l", xlim = c(0,209), col = color)
legend("topright", legend = c("Any-Cause Unavailability", "Unavailbility due to Walking"), 
       lwd = c(2, 1), col = c("black", color))
avail.plot <- recordPlot()
rm(y)


##### Model Fitting #####

## Define function to format variables for modeling, call geeglm(), and 
## make small-sample variance corrections (uses xgeepack.R functions)
fit <- function(formula, combos = NULL, data = eval(d)) {
  d <- data
  
  d$send.center        <- as.numeric(d$send) - .6
  d$jbsteps30.log      <- log(d$jbsteps30.zero + .5)
  d$jbsteps30pre.log   <- log(d$jbsteps30pre.zero + .5)
  d$study.day.nogap.sq <- d$study.day.nogap ^ 2
  
  formula <- substitute(formula)
  
  fit <- geeglm(formula = formula, id = user, weights = as.numeric(avail),
                data = d, scale.fix = T)
  
  temp <- bread.geeglm(fit)
  fit$var <- temp %*% meat.geeglm(fit, g = NULL, gn = NULL, small = TRUE) %*% t(temp)
  fit
}

## Model 1: No time effect
model1 <- fit(jbsteps30.log ~ jbsteps30pre.log + send.center)
estimate(model1, ztest = FALSE)

## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status
model2 <- fit(jbsteps30.log ~ study.day.nogap + jbsteps30pre.log + 
                send.center + study.day.nogap:send.center,
              data = subset(eval(d), jbsteps30 != 0 & jbsteps30pre != 0))
estimate(model2, ztest = FALSE)


##### Time Trend Analysis #####

## Minimal model: intercept and step count in prior 30 minutes. First, use all
## participants, then split by treated/untreated
minmod       <- fit(jbsteps30.log ~ jbsteps30pre.log)
minmod.send0 <- fit(jbsteps30.log ~ jbsteps30pre.log, 
                    data = subset(eval(d), send == F))
minmod.send1 <- fit(jbsteps30.log ~ jbsteps30pre.log,
                    data = subset(eval(d), send == T))

#### LOESS plots
span <- c(.09, .3)
sample <- sample(1:length(minmod$residuals), ceiling(.5 * length(minmod$residuals)))
par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)

## Main effect of day on study
for (i in 1:length(span)) {
  plot(minmod$residuals[sample] ~ eval(d)$study.day.nogap[sample], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 41))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(with(subset(eval(d), avail == T), 
             predict(loess(minmod$residuals[eval(d)$avail == T] ~ study.day.nogap,
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
for (i in 1:length(span)) {
  plot(minmod$residuals[sample] ~ eval(d)$study.day.nogap[sample], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 41))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(with(subset(eval(d), avail == T), 
             predict(loess(minmod.send1$residuals[send == T] ~ 
                             study.day.nogap[send == T & avail == T],
                           span = span[i]),
                     newdata = seq(0, 41)) - 
               predict(loess(minmod.send0$residuals[send == F] ~
                               study.day.nogap[send == F & avail == T],
                             span = span[i]),
                       newdata = seq(0, 41))),
        col = color, lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}
loess.plot.intr <- recordPlot()