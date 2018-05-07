## Pre-clear the global memory
rm(list =  ls())

library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

## Required packages and source files
setwd("~//Documents/github/heartstepsdata/Walter/rand-probs/block-plus-oracle")
source("oracle_functions.R");require(mgcv); require(chron); 
require(foreach); require(lme4)

setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")
setwd("~//Documents/github/heartstepsdata/Walter/rand-probs/block-plus-oracle")
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
buckets = list(bucket1,bucket2, bucket3)

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") + minutes(40)

## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)
seq.hour = c(14:23,0:1)

## Setup
denom = 2
init.N = c(0.0,1.74/denom)
eta = 0.0
lambda = 0.0

## Extract a person-day
set.seed("541891")
all.persondays = unique(window.time[,c(1,3)])

## Generate the 5 random partitions
partitions = sample(1:nrow(all.persondays), nrow(all.persondays), replace = FALSE)
block.size = ceiling(nrow(all.persondays)/5)

all.persondays[,3] = unlist(lapply(1:nrow(all.persondays), which.partition))

all.persondays = data.frame(all.persondays)
names(all.persondays) = c("user", "study.day", "block")

blockid = 1
N.one = c(0.0,1.56)
# otherblock.assignment.fn(all.persondays, blockid, N.one)

blockid = 2
N.two = c(0.0,1.6)
# otherblock.assignment.fn(all.persondays, blockid, N.two)

blockid = 3
N.three = c(0.0,1.565)
# otherblock.assignment.fn(all.persondays, blockid, N.three)

blockid = 4
N.four = c(0.0,1.565)
# otherblock.assignment.fn(all.persondays, blockid, N.four)

blockid = 5
N.five = c(0.0,1.56)
# otherblock.assignment.fn(all.persondays, blockid, N.five)

all.Ns= c(N.one[2], N.two[2], 
          N.three[2], N.four[2],
          N.five[2])

set.seed("541891")
if (!file.exists("simulation_At.RDS")) {
  # total.At = sapply(1:nrow(all.persondays), cv.assignment.fn, all.persondays, all.Ns)
  total.At = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind) %dopar% cv.assignment.fn(i,all.persondays, all.Ns)
  saveRDS(total.At, file = "/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/simulation_At.RDS")
} else {
  total.At = readRDS("/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/simulation_At.RDS")
}
mean(colSums(total.At[1:136,]), na.rm = TRUE)
sd(colSums(total.At[1:136,]), na.rm = TRUE)/sqrt(nrow(all.persondays))

sim.df = data.frame(colSums(total.At[1:136,]))
names(sim.df) = c("ints")

# pdf(file = "histogram_numtreatments.pdf", width = 7, height=7)
values = sim.df$ints
counts <- table(values)
counts <- counts/sum(counts);
names(counts) <- 0:(length(counts)-1);
barplot(counts, ylim = c(0, 0.35), ylab = "Percent", xlab = "Number of Treatments", cex.lab = 1.3)
# dev.off()

time.steps = 1:nrow(total.At[1:136,])
hour = floor(time.steps/12 + 14 + 35/60)%%24

hourly.At = data.frame("hour" = hour,
                       "num.At" = rowMeans(total.At[1:136,]),
                       "num.Xt" = rowMeans(total.At[137:nrow(total.At),])
                       )

hourly.results.At = aggregate(num.At~hour, data = hourly.At, FUN = mean)
hourly.results.Xt = aggregate(num.Xt~hour, data = hourly.At, FUN = mean)

altered.hour.At = (hourly.results.At$hour-14)%%24
num.At = hourly.results.At$num.At[order(altered.hour.At)]

altered.hour.At = altered.hour.At[order(altered.hour.At)]

temp.df = data.frame(cbind(altered.hour.At, num.At))

# pdf(file = "fraction_actionreceived.pdf", width = 7, height=7)
x.axis = temp.df$altered.hour.At
y.axis = temp.df$num.At
plot(x.axis, y.axis, pch = 16,
     cex.lab=1.3, xlab = "Hour Block",
     ylab = "Average Number of Treatments", col = "red",
     ylim = c(0.00, 0.02))
lines(x.axis, y.axis, lty = 1,
      col = "red")
# dev.off()

#  Make curves per person
set.of.users = unique(all.persondays[,1])
user.mean = vector(length = length(set.of.users))
for(i in 1:length(set.of.users)) {
  user = set.of.users[i]
  user.mean[i] = mean(colSums(total.At[1:136, all.persondays[,1] == user]), na.rm = TRUE)  
}
sd(user.mean)


# pdf(file = "user_treatment_histogram.pdf", width = 10, height=7)
hist(user.mean, xlab = "Number of Treatments", cex.lab = 1.3, main = "")
# dev.off()

# sd(colSums(total.At[1:136, all.persondays[,1] == user]), na.rm = TRUE)/sqrt(nrow(all.persondays))


global.x.axis = temp.df$altered.hour.At
global.y.axis = temp.df$num.At

# pdf(file = "user_fraction_actionreceived.pdf", width = 10, height=7)
plot(global.x.axis, global.y.axis, pch = 16,
     cex.lab=1.3, xlab = "Hour Block",
     ylab = "Average Number of Treatments", col = "gray25",
     lwd = 2, type = "l",     
     ylim = c(0.00, 0.025))

for(user in set.of.users) {
  
  user.hourly.At = data.frame("hour" = hour,
                              "num.At" = rowMeans(total.At[1:136,all.persondays[,1] == user]),
                              "num.Xt" = rowMeans(total.At[137:nrow(total.At),all.persondays[,1] == user])
  )
  
  user.hourly.results.At = aggregate(num.At~hour, data = user.hourly.At, FUN = mean)
  user.hourly.results.Xt = aggregate(num.Xt~hour, data = user.hourly.At, FUN = mean)
  
  user.altered.hour.At = (user.hourly.results.At$hour-14)%%24
  user.num.At = user.hourly.results.At$num.At[order(user.altered.hour.At)]
  
  user.altered.hour.At = user.altered.hour.At[order(user.altered.hour.At)]
  
  user.temp.df = data.frame(cbind(user.altered.hour.At, user.num.At))
  
  x.axis = user.temp.df$user.altered.hour.At
  y.axis = user.temp.df$user.num.At
  lines(x.axis, y.axis, lty = 1,
        col = "lightgray")
}
  
lines(global.x.axis, global.y.axis, lty = 2, lwd = 2,
      col = "gray20")
# dev.off()


## Uniformity plots
## Calculate p.hat per person-day
if (!file.exists("simulation_phat.RDS")) {
  # total.phat = sapply(1:nrow(all.persondays), cv.assignment.multiple.fn, all.persondays, all.Ns, num.iters)
  num.iters = 1000
  total.phat = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind) %dopar% cv.assignment.multiple.fn(i,all.persondays, all.Ns, num.iters)
  saveRDS(total.phat, file = "/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-oracle/simulation_phat.RDS")
} else {
  total.phat = readRDS("/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-oracle/simulation_phat.RDS")
}

## Compute the squared distance
results.phat = vector(length = ncol(total.phat))

for (col in 1:ncol(total.phat)) {
  current.values = total.phat[,col]
  current.Xt = current.values[137:length(current.values)]
  current.phat = current.values[1:136]
  current.phat = current.phat[current.Xt == 1]
  results.phat[col] = sd(current.phat)
}

library(ggplot2)
sim.df = data.frame(results.phat)
names(sim.df) = c("metric")

p1 <- ggplot(data=sim.df, aes(metric)) + geom_histogram() + xlab("Squared Distance Metric")

## Plot of the average number of 
## interventions across person-days
p1 


# total.phat = readRDS("total_phat.RDS")