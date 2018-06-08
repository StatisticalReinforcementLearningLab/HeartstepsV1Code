## Pre-clear the global memory
rm(list =  ls())

library(doParallel)
cl <- makeCluster(30)
registerDoParallel(cl)

## Required packages and source files
source("user_functions.R");require(mgcv); require(chron); 
require(foreach); require(lme4)

setwd("/n/murphy_lab/users/wdempsey/ubicomp/data/")
# setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")
setwd("/n/murphy_lab/users/wdempsey/ubicomp/block-and-forecast/baseline")
# setwd("~//Documents/github/heartstepsdata/Walter/rand-probs/block-plus-forecast/personalized-forecast")
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
buckets = list(bucket1,bucket2, bucket3)

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") + minutes(40)

## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)
seq.hour = c(14:23,0:1)

## Extract a person-day
set.seed("139137")
all.persondays = unique(window.time[,c(1,3)])

## Generate the 5 random partitions of the people
unique.users = unique(all.persondays$user)
partitions = sample(unique.users, length(unique.users), replace = FALSE)
block.size = ceiling(length(unique.users)/5)

all.persondays[,3] = unlist(lapply(all.persondays$user, which.partition))

all.persondays = data.frame(all.persondays)
names(all.persondays) = c("user", "day", "block")

## Generate better predictions
param.list = allmodel.params(seq.hour, all.persondays) # Compute all necessary model parameters
sedwidthdf.list = list.of.sedwidthdfs(seq.hour, window.time) # Compute all necessary dfs

## Setup
denom = 2
init.N = c(0.0,1.65)
eta = 0.0
lambda = 0.0

set.seed("541891")
blockid = 1
N.one = c(0.0,0.86)
# otherblock.assignment.fn(all.persondays, blockid, N.one, param.list, sedwidth.df)

set.seed("541891")
blockid = 2
N.two = c(0.0,0.86)
# otherblock.assignment.fn(all.persondays, blockid, N.two, param.list, sedwidth.df)

set.seed("541891")
blockid = 3
N.three = c(0.0,0.825)
# otherblock.assignment.fn(all.persondays, blockid, N.three, param.list, sedwidth.df)

set.seed("541891")
blockid = 4
N.four = c(0.0,0.86)
# otherblock.assignment.fn(all.persondays, blockid, N.four, param.list, sedwidth.df)

set.seed("541891")
blockid = 5
N.five = c(0.0,0.845)
# otherblock.assignment.fn(all.persondays, blockid, N.five, param.list, sedwidth.df)

all.Ns= c(N.one[2], N.two[2], 
          N.three[2], N.four[2],
          N.five[2])

if (!file.exists("simulation_phat.RDS")) {
  set.seed("541891")
  total.At = sapply(1:nrow(all.persondays), cv.assignment.fn, all.persondays, all.Ns, param.list, sedwidthdf.list)
  saveRDS(total.At, file = "simulation_At.RDS")
} else{
  total.At = readRDS("simulation_At.RDS")
}
mean(colSums(total.At[1:144,]), na.rm = TRUE)
sd(colSums(total.At[1:144,]), na.rm = TRUE)/sqrt(nrow(all.persondays))

## Uniformity plots
## Calculate p.hat per person-day
if (!file.exists("simulation_phat.RDS")) {
  # total.phat = sapply(1:nrow(all.persondays), cv.assignment.multiple.fn, all.persondays, all.Ns, num.iters)
  num.iters = 1000
  total.phat = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind) %dopar% cv.assignment.multiple.fn(i,all.persondays, all.Ns, num.iters)
  saveRDS(total.phat, file = "simulation_phat.RDS")
} else {
  total.phat = readRDS("simulation_phat.RDS")
}