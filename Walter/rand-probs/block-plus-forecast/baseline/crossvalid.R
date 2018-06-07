## Pre-clear the global memory
rm(list =  ls())

library(doParallel)
cl <- makeCluster(30)
registerDoParallel(cl)

## Required packages and source files
# setwd("~//Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast")
source("functions.R");require(mgcv); require(chron); 
require(foreach); require(lme4); require(doRNG)

setwd("/n/murphy_lab/users/wdempsey/ubicomp/data/")
# setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")
setwd("/n/murphy_lab/users/wdempsey/ubicomp/block-and-forecast/baseline")
# setwd("~//Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/baseline")
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
buckets = list(bucket1,bucket2, bucket3)

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT")
## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)

seq.hour = c(14:23,0:1)
fraction.data = matrix(nrow = length(seq.hour), ncol = 3)
if (!file.exists("fractiondata.RDS")) {
  for (i in 1:length(seq.hour)) {
    current.hour = seq.hour[i]
    result = fraction.time.in.state(current.hour, buckets)
    fraction.data[i,] = c(current.hour,result$mean,result$var)
  }
  saveRDS(fraction.data, file = "fractiondata.RDS")
} else{
  fraction.data = readRDS("fractiondata.RDS")
}
fraction.df = data.frame(fraction.data)
names(fraction.df) = c("current.hour", "mean", "var")

# # pdf(file = "fraction_sedentary.pdf", width = 10, height=7)
# x.axis = (fraction.df$current.hour-14)%%24
# plot(x.axis, fraction.df$mean, pch = 16,
#      cex.lab=1.3, xlab = "Hour Block",
#      ylab = "# of minutes 'Sedentary'", col = "red",
#     ylim = c(0.45, 0.55))
# lines(x.axis, fraction.df$mean, lty = 1,
#      col = "red")
# # dev.off()

## Setup
denom = 2
init.N = c(0.0,1.74/denom)
eta = 0.0
lambda = 0.0

## Extract a person-day
set.seed("139137")
all.persondays = unique(window.time[,c(1,3)])

## Generate the 5 random partitions of the people
unique.users = unique(all.persondays$user)
partitions = sample(unique.users, length(unique.users), replace = FALSE)
block.size = ceiling(length(unique.users)/5)

all.persondays[,3] = unlist(lapply(all.persondays$user, which.partition))

all.persondays = data.frame(all.persondays)
names(all.persondays) = c("user", "study.day", "block")

set.seed("541891")
blockid = 1
N.one = c(0.0,0.88)
# otherblock.assignment.fn(all.persondays, blockid, N.one)

blockid = 2
N.two = c(0.0,0.88)
# otherblock.assignment.fn(all.persondays, blockid, N.two)

blockid = 3
N.three = c(0.0,0.85)
# otherblock.assignment.fn(all.persondays, blockid, N.three)

blockid = 4
N.four = c(0.0,0.87)
# otherblock.assignment.fn(all.persondays, blockid, N.four)

blockid = 5
N.five = c(0.0,0.875)
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

## Uniformity plots
## Calculate p.hat per person-day
if (!file.exists("simulation_phat.RDS")) {
  # total.phat = sapply(1:nrow(all.persondays), cv.assignment.multiple.fn, all.persondays, all.Ns, num.iters)
  num.iters = 1000
  total.phat = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind, .options.RNG =541891) %dorng% cv.assignment.multiple.fn(i,all.persondays, all.Ns, num.iters)
  saveRDS(total.phat, file = "/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/simulation_phat.RDS")
} else {
  total.phat = readRDS("/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/simulation_phat.RDS")
}

# ## Compute the squared distance
# results.phat = vector(length = ncol(total.phat))
# 
# for (col in 1:ncol(total.phat)) {
#   current.values = total.phat[,col]
#   current.Xt = current.values[137:length(current.values)]
#   current.phat = current.values[1:136]
#   current.phat = current.phat[current.Xt == 1]
#   results.phat[col] = sd(current.phat)
# }
# 
# library(ggplot2)
# sim.df = data.frame(results.phat)
# names(sim.df) = c("metric")
# 
# p1 <- ggplot(data=sim.df, aes(metric)) + geom_histogram() + xlab("Squared Distance Metric")
# 
# ## Plot of the average number of 
# ## interventions across person-days
# p1 
# 
# 
# # total.phat = readRDS("total_phat.RDS")