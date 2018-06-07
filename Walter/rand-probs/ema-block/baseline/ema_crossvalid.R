## Pre-clear the global memory
rm(list =  ls())

## Parallel compute setup 
library(doParallel)
cl <- makeCluster(30)
registerDoParallel(cl)

## Required packages and source files
## setwd("/Users/walterdempsey/Documents/github/heartstepsdata/Walter/rand-probs/ema-block/baseline")
source("ema_functions.R");require(mgcv); require(chron); require(foreach); require(doRNG)

setwd("/n/murphy_lab/users/wdempsey/ubicomp/data/")
# setwd("/Volumes/dav/HeartSteps/Walter")
window.time = read.csv("window_time.csv")
# Sedentary.values = read.csv("sed_values.csv")
# Sedentary.length = read.csv("sed_length.csv")
setwd("/n/murphy_lab/users/wdempsey/ubicomp/ema-block/")
# setwd("~/Documents/github/heartstepsdata/Walter/rand-probs/ema-block/baseline")
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
buckets = list(bucket1,bucket2, bucket3)

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT")

## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)
seq.hour = c(14:23,0:1)

## Setup
init.N = 0.5

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

## Pre-compute prob.buckets
prob.buckets.list = list()
offset = list()

for(i in 1:5) {
  prob.buckets.list[[i]] = calc.prob.buckets(i, all.persondays, window.time, init.N, 0)
}

blockid = 1
N.one = 0.5
prob.buckets = prob.buckets.list[[blockid]]
offset[[1]] = otherblock.assignment.fn(all.persondays, blockid, N.one, prob.buckets)/3

blockid = 2
N.two = 0.5
prob.buckets = prob.buckets.list[[blockid]]
offset[[blockid]] = otherblock.assignment.fn(all.persondays, blockid, N.two, prob.buckets)/3

blockid = 3
N.three = 0.5
prob.buckets = prob.buckets.list[[blockid]]
offset[[blockid]] = otherblock.assignment.fn(all.persondays, blockid, N.three, prob.buckets)/3

blockid = 4
N.four = 0.5
prob.buckets = prob.buckets.list[[blockid]]
offset[[blockid]] = otherblock.assignment.fn(all.persondays, blockid, N.four, prob.buckets)/3

blockid = 5
N.five = 0.5
prob.buckets = prob.buckets.list[[blockid]]
offset[[blockid]] = otherblock.assignment.fn(all.persondays, blockid, N.five, prob.buckets)/3

all.Ns= c(N.one, N.two, 
          N.three, N.four,
          N.five)

## Recompute with offsets
## Subtracting offset to account for availability 
for(i in 1:5) {
  prob.buckets.list[[i]] = calc.prob.buckets(i, all.persondays, window.time, N.one, offset[[i]])
}


set.seed("541891")
if (!file.exists("simulation_At.RDS")) {
  # total.At = sapply(1:nrow(all.persondays), cv.assignment.fn, all.persondays, all.Ns)
  total.At = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind, .options.RNG =541891) %dorng% cv.assignment.fn(i,all.persondays, all.Ns, prob.buckets.list)
  saveRDS(total.At, file = "simulation_At.RDS")
} else {
  total.At = readRDS("simulation_At.RDS")
}
mean(colSums(total.At[1:144,]), na.rm = TRUE)
sd(colSums(total.At[1:144,]), na.rm = TRUE)/sqrt(nrow(all.persondays))

## Calculate p.hat per person-day
## Only compute if file doesn't exist
if (!file.exists("simulation_phat.RDS")) {
  num.iters = 1000
  total.phat = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind, .options.RNG =541891) %dorng% cv.assignment.multiple.fn(i,all.persondays, all.Ns, num.iters, prob.buckets.list)  
  saveRDS(total.phat, file = "simulation_phat.RDS")
} else {
  total.phat = readRDS("simulation_phat.RDS")
}
