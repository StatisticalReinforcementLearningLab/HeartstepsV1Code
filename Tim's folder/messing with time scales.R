
## This is the sloppy code I'm using to start messing around with time scales

## First I need to follow directions on github i.e. I need to run the two scripts to update the mbox files, always make sure I'm in the right directory first
setwd("/tnecamp/Desktop/heartstep_personal/heartstepsdata")
setwd("heartstepsdata")
setwd("/Volumes")
setwd("Macintosh HD/Users/tnecamp/Desktop/heartstep_personal/heartstepsdata")


setwd("/Volumes/Macintosh HD/Users/tnecamp/Desktop/heartstep_personal/heartstepsdata")

### MAIN EFFECTS ANALYSIS FOR HEARTSTEPS ###

##### Setup #####

## Load helper functions and data frames
source("init.R")
setwd(sys.var$mbox.data)
load("csv.RData")
load("analysis.RData")
setwd(sys.var$repo)


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


d2 = eval(d)
d2$send.center        <- as.numeric(d2$send) - .6
d2$jbsteps30.log      <- log(d2$jbsteps30.zero + .5)
d2$jbsteps30pre.log   <- log(d2$jbsteps30pre.zero + .5)
d2$study.day.nogap.sq <- d2$study.day.nogap ^ 2


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

## This is the model actually used to get table in model 2 section
model2 <- fit(jbsteps30.log ~ study.day.nogap + jbsteps30pre.log + 
                send.center + study.day.nogap:send.center)
estimate(model2, ztest = FALSE)


##### Time Trend Analysis #####

## Minimal model: intercept and step count in prior 30 minutes. First, use all
## participants, then split by treated/untreated
minmod       <- fit(jbsteps30.log ~ jbsteps30pre.log)

## Tim: I'm not sure why he has these residuals here.  I don't really care about these. 
## I really only care about splitting them after fitting the general model above.
## Getting the residuals below is not just looking for a treatment effect but also
## allowing an interecation between pre step count and post
minmod.send0 <- fit(jbsteps30.log ~ jbsteps30pre.log, 
                    data = subset(eval(d), send == F))
minmod.send1 <- fit(jbsteps30.log ~ jbsteps30pre.log,
                    data = subset(eval(d), send == T))


par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)

## Plot to show difference in residuals between treated and untreated points
for (i in 1:length(span)) {
  plot(minmod$residuals[d2$avail] ~ d2$study.day.nogap[d2$avail], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 50))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(predict(loess(minmod$residuals[d2$avail & d2$send] ~ d2$study.day.nogap[d2$avail & d2$send],
                           span = span[i])),
        col = color, lwd = 6)
  lines(predict(loess(minmod$residuals[d2$avail & !d2$send] ~ d2$study.day.nogap[d2$avail & !d2$send],
                           span = span[i])),
        col = "red", lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}
loess.plot.main <- recordPlot()









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





## Tim's LOESS plots, without doing a sub sample and dropping missing data values

##### Time Trend Analysis #####

## Minimal model: intercept and step count in prior 30 minutes. First, use all
## participants, then split by treated/untreated
minmod       <- fit(jbsteps30.log ~ jbsteps30pre.log)


#### LOESS plots           
span <- c(.09, .3)
par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)

## Main effect of day on study
for (i in 1:length(span)) {
  plot(minmod$residuals[d2$avail] ~ d2$study.day.nogap[d2$avail], axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 50))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(with(subset(d2, avail == T), 
             predict(loess(minmod$residuals[d2$avail] ~ d2$study.day.nogap[d2$avail],
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
















## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status, with deleting 0's
model2 <- fit(jbsteps30.log ~ study.day.nogap + jbsteps30pre.log + 
                send.center + study.day.nogap:send.center,
              data = subset(eval(d), jbsteps30 != 0 & jbsteps30pre != 0))
estimate(model2, ztest = FALSE)


## Model 2: Linear day-on-study effect and interaction between linear 
## day on study and centered treatment status without deleting 0's
model100 <- fit(jbsteps30.log ~ jbsteps30pre.log)
estimate(model100, ztest = FALSE)


geeglm(formula = jbsteps30.log ~ study.day.nogap + jbsteps30pre.log + 
                send.center + rand_col, id = user, weights = as.numeric(avail), data = d2, scale.fix=T)


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


# This is me trying to create a vector to play with to makes graphs.  It only looks at avaiable people

  
model100 <- fit(jbsteps30.log ~ jbsteps30pre.log, data=d2)
estimate(model100, ztest = FALSE)
plot((d2$study.day.nogap)[(d2$avail)&(d2$send)], (model100$residuals)[(d2$avail)&(d2$send)])
points((d2$study.day.nogap)[(d2$avail)&!(d2$send)], (model100$residuals)[(d2$avail)&!(d2$send)], col="red")

plot((d2$study.day.nogap), (model100$residuals))


#Next I need to make loess plots for these situations, also, need to figure out time scaling and make graphs for thos situations as well, all need to run gee analysis


d3 = d2[d2$avail,]

names(d2)
with(d2[d2$avail,][1:200,], cbind(study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))

with(d2[(d2$avail &d2$user==6),], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))



!d2[(d2$avail),study.day]==d2[(d2$avail),study.day.nogap]

sum(d3$study.day!=d3$study.day.nogap)
with(d3[d3$study.day!=d3$study.day.nogap,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))

with(d3[(d3$study.day!=d3$study.day.nogap),], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))

with(d3[d3$user==17,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))

with(d3[d3$user==2,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.year, time.stamp.mon, time.stamp.yday, time.stamp.mday, time.stamp.wday))


## Create Susan's time scale
users.with.gaps = unique(d3[(d3$study.day!=d3$study.day.nogap),1])
all.users = unique(d3[,1])
users.without.gaps = setdiff(all.users, users.with.gaps)


new_df = data.frame(matrix(nrow=dim(d3)[1],ncol =2 ))
names(new_df)= c( "start.wday","susan.time")

for(user_iter in users.without.gaps)
{
	user_row = d3$user == user_iter
	start_wday = d3$time.stamp.wday[user_row][1] - d3$study.day[user_row][1]
	new_df[user_row,1] =  start_wday
	new_df[user_row,2] = start_wday + d3$study.day[user_row]
	print(sum(new_df[user_row,2]%%7 != d3$time.stamp.wday[user_row]))
}

users.with.gaps

## Do it for person 1
user_row = d3$user == 1
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat = cbind(3, 3+d3$study.day.nogap[user_row])

temp_mat[101:165,2]= temp_mat[101:165,2]+6
new_df[user_row,] = temp_mat

## Do it for person 3
user_row = d3$user == 3
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat = cbind(6, 6+d3$study.day.nogap[user_row])
cbind(temp_mat1,temp_mat)

temp_mat[78:164,2]= temp_mat[78:164,2]+1
cbind(temp_mat1,temp_mat)

temp_mat[,2]%%7 == d3$time.stamp.wday[user_row]

new_df[user_row,] = temp_mat


## Do it for person 6
user_row = d3$user == 6
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat = cbind(1, 1+d3$study.day.nogap[user_row])
cbind(temp_mat1,temp_mat)

temp_mat[41:133,2]= temp_mat[41:133,2]+6
cbind(temp_mat1,temp_mat)

temp_mat[,2]%%7 == d3$time.stamp.wday[user_row]

new_df[user_row,] = temp_mat

## Do it for person 14
user_row = d3$user == 14
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat = cbind(4, 4+d3$study.day.nogap[user_row])
cbind(temp_mat1,temp_mat)

cbind(temp_mat1,temp_mat)

temp_mat[,2]%%7 == d3$time.stamp.wday[user_row]

new_df[user_row,] = temp_mat


## Do it for person 15
user_row = d3$user == 15
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat1
temp_mat = cbind(4, 4+d3$study.day.nogap[user_row])
cbind(temp_mat1,temp_mat)

temp_mat[76:185,2]= temp_mat[76:185,2]+4
cbind(temp_mat1,temp_mat)

temp_mat[,2]%%7 == d3$time.stamp.wday[user_row]

new_df[user_row,] = temp_mat

## Do it for person 39
user_row = d3$user == 39
temp_mat1 = with(d3[user_row,], cbind(user, study.date, study.day, study.day.nogap, utime.stamp, time.stamp.wday))
temp_mat1
temp_mat = cbind(3, 3+d3$study.day.nogap[user_row])
cbind(temp_mat1,temp_mat)

temp_mat[19:117,2]= temp_mat[19:117,2]+4
cbind(temp_mat1,temp_mat)

temp_mat[,2]%%7 == d3$time.stamp.wday[user_row]

new_df[user_row,] = temp_mat

## Now combine everything
sum(new_df[,2]%%7 != d3$time.stamp.wday)
d4 = cbind(d3, new_df)


##### Make LOESS plots for new time variable #####

## Minimal model: intercept and step count in prior 30 minutes. First, use all
## participants, then split by treated/untreated
minmod <- geeglm(formula = jbsteps30.log ~ jbsteps30pre.log, id = user, data = d4, scale.fix=T)
estimate(minmod, ztest = FALSE)

span <- c(.09, .3)
par(mfrow = c(1, length(span)), mar = c(0, 0, 2, 0), oma = c(4, 4, 1, 1), 
    mgp = c(2, 0.6, 0), font.main = 1)
    
 ## Main effect of day on study
for (i in 1:length(span)) {
  plot(minmod$residuals ~ d4$susan.time, axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 55))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(predict(loess(minmod$residuals ~ d4$susan.time, span = span[i])),
        col = color, lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day aligned by day of the week", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}


## Plot to show difference in residuals between treated and untreated points with aligned days
for (i in 1:length(span)) {
  plot(minmod$residuals ~ d4$susan.time, axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 50))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(predict(loess(minmod$residuals[d4$send] ~ d4$susan.time[d4$send],
                           span = span[i])),
        col = color, lwd = 6)
  lines(predict(loess(minmod$residuals[!d4$send] ~ d4$susan.time[!d4$send],
                           span = span[i])),
        col = "red", lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day aligned by day of the week", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}



## Plot to show difference in residuals between treated and untreated points with unaligned days
for (i in 1:length(span)) {
  plot(minmod$residuals ~ d4$susan.time, axes = F,
       xlab = "", ylab = "", main = "", type = "p", xlim = c(0, 50))
  box(col = "grey40")
  axis(side = 1, outer = T, cex = .6)
  lines(predict(loess(minmod$residuals[d4$send] ~ d4$study.day.nogap[d4$send],
                           span = span[i])),
        col = color, lwd = 6)
  lines(predict(loess(minmod$residuals[!d4$send] ~ d4$study.day.nogap[!d4$send],
                           span = span[i])),
        col = "red", lwd = 6)
  if (i == 1) {
    axis(side = 2, outer = T, cex = .6)
    title(ylab = "Residual", outer = T)
    mtext("Study Day unaligned", side = 1, outer = T, line = 2.5)
  }
  title(main = paste("Span:", span[i]), cex = .3)
}

## Aligned model
aligned_mod = geeglm(formula = jbsteps30.log ~ susan.time + jbsteps30pre.log + send.center + send.center:susan.time, id = user, weights = as.numeric(avail), data = d4, scale.fix=T)
estimate(aligned_mod)
                
## Unaligned model
unaligned_mod = geeglm(formula = jbsteps30.log ~ study.day.nogap + jbsteps30pre.log + send.center + send.center:study.day.nogap, id = user, weights = as.numeric(avail), data = d4, scale.fix=T)
estimate(unaligned_mod)




data = subset(suggest, !is.na(study.day.nogap) & study.day.nogap <= 41 & user %in% ids & avail == T)

data = subset(suggest, is.na(study.day.nogap))

## Make cleaner graph where it averages the residuals at each time point
names(d4)

d4$minmod.residuals = minmod$residuals

y1 = aggregate(minmod.residuals ~ susan.time, data = d4, FUN = mean)
plot(y1)
lines(predict(loess(y1$minmod.residuals ~ y1$susan.time, span = .9)),
        col = color, lwd = 6)
        
y1 = aggregate(minmod.residuals ~ study.day.nogap, data = d4[d4$study.day.nogap < 42,], FUN = mean)
plot(y1, type = 'l')
lines(predict(loess(y1$minmod.residuals ~ y1$study.day.noga, span = 1/3)),
        col = color, lwd = 6)
        
y1 = aggregate(minmod.residuals ~ susan.time, data = d4[d4$susan.time > 5 & d4$susan.time < 45, ], FUN = mean)
plot(y1, type = 'l')
lines(predict(loess(y1$minmod.residuals ~ y1$susan.time, span = 1/3)),
        col = color, lwd = 6)
abline(a=0, b=0)
        
y1 = aggregate(jbsteps30.log ~ susan.time+100, data = d4[d4$susan.time > 5 & d4$susan.time < 45, ], FUN = mean)
plot(y1, type = 'l', xlab='study days aligned', ylab = 'log step count')
lines(6:44,predict(loess(y1$jbsteps30.log ~ y1$susan.time, span = 1/3)),
        col = color, lwd = 6)
        
y1 = aggregate(jbsteps30.log ~ study.day.nogap, data = d4[d4$study.day.nogap < 42,], FUN = mean)
plot(y1, type = 'l', xlab='study days unaligned', ylab = 'log step count')
lines(0:41, predict(loess(y1$jbsteps30.log ~ y1$study.day.nogap, span = 1/3)),
        col = color, lwd = 6)
        
y1 = aggregate(jbsteps30.log ~ susan.time + send, data = d4[d4$susan.time > 5 & d4$susan.time < 45, ], FUN = mean)
y1.send = y1[y1$send,]
y1.no.send = y1[!y1$send,]
plot(y1.send$susan.time, y1.send$jbsteps30.log, type = 'l',xlab='study days aligned', ylab = 'log step count')
lines(6:44,predict(loess(jbsteps30.log ~ susan.time, y1.send, span = 2/3)),
        col = color, lwd = 1)
points(y1.no.send$susan.time, y1.no.send$jbsteps30.log, type = 'l', col='red')
lines(6:44, predict(loess(jbsteps30.log ~ susan.time, y1.no.send, span = 2/3)),
        col = 'green', lwd = 1)
        
y1 = aggregate(jbsteps30.log ~ study.day.nogap + send, data = d4[d4$study.day.nogap < 45,], FUN = mean)
y1.send = y1[y1$send,]
y1.no.send = y1[!y1$send,]
plot(y1.send$study.day.nogap, y1.send$jbsteps30.log, type = 'l',xlab='study days unaligned', ylab = 'log step count')
lines(0:44,predict(loess(jbsteps30.log ~ study.day.nogap, y1.send, span = 2/3)),
        col = color, lwd = 1)
points(y1.no.send$study.day.nogap, y1.no.send$jbsteps30.log, type = 'l', col='red')
lines(0:44, predict(loess(jbsteps30.log ~ study.day.nogap, y1.no.send, span = 2/3)),
        col = 'green', lwd = 1)

y1 = aggregate(minmod.residuals ~ susan.time + send, data = d4[d4$susan.time > 5 & d4$susan.time < 45, ], FUN = mean)
y1.send = y1[y1$send,]
y1.no.send = y1[!y1$send,]
plot(y1.send$susan.time, y1.send$minmod.residuals, type = 'l')
lines(predict(loess(minmod.residuals ~ susan.time, y1.send, span = 2/3)),
        col = color, lwd = 6)
points(y1.no.send$susan.time, y1.no.send$minmod.residuals, type = 'l', col='red')
lines(predict(loess(minmod.residuals ~ susan.time, y1.no.send, span = 2/3)),
        col = 'green', lwd = 6)
        
        
y1 = aggregate(minmod.residuals ~ study.day.nogap + send, data = d4[d4$study.day.nogap < 45,], FUN = mean)
y1.send = y1[y1$send,]
y1.no.send = y1[!y1$send,]
plot(y1.send$study.day.nogap, y1.send$minmod.residuals, type = 'l')
lines(0:44, predict(loess(minmod.residuals ~ study.day.nogap, y1.send, span = 2/3)),
        col = color, lwd = 6)
points(y1.no.send$study.day.nogap, y1.no.send$minmod.residuals, type = 'l', col='red')
lines(0:44, predict(loess(minmod.residuals ~ study.day.nogap, y1.no.send, span = 2/3)),
        col = 'green', lwd = 6)


###  Look at weekday/weekend effect ###
names(d4)
weekend = d4$time.stamp.wday ==0 | d4$time.stamp.wday ==6
d4$weekend = weekend
weekend_mod = geeglm(formula = jbsteps30.log ~ weekend + jbsteps30pre.log + send.center + send.center:weekend, id = user, data = d4, scale.fix=T)
estimate(weekend_mod)

weekend_mod = geeglm(formula = jbsteps30.log ~ weekend + jbsteps30pre.log, id = user, data = d4, scale.fix=T)
estimate(weekend_mod)


weekend_mod = geeglm(formula = jbsteps30.log ~ weekend, id = user, data = d4, scale.fix=T)
estimate(weekend_mod)


## Looking at seasonal effect
d5 = d4[!is.na(d4$temp),]
temp_mod = geeglm(formula = jbsteps30.log ~ temperature + jbsteps30pre.log + send.center + send.center:temperature, id = user, data = d5, scale.fix=T)
estimate(temp_mod)