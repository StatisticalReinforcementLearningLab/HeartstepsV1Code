## Pre-clear the global memory
rm(list =  ls())

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
set.seed("541891")
all.persondays = unique(window.time[,c(1,3)])

## Generate the 5 random partitions
partitions = sample(1:nrow(all.persondays), nrow(all.persondays), replace = FALSE)
block.size = ceiling(nrow(all.persondays)/5)

all.persondays[,3] = unlist(lapply(1:nrow(all.persondays), which.partition))

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

# Recompute with offsets
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

# sim.df = data.frame(colSums(total.At[1:144,]))
# names(sim.df) = c("ints")
# 
# pdf(file = "emablocking_histogram_numtreatments.pdf", width = 7, height=7)
# values = sim.df$ints
# counts <- table(values)
# counts <- counts/sum(counts);
# names(counts) <- 0:(length(counts)-1);
# barplot(counts, ylim = c(0, 0.35), ylab = "Percent", xlab = "Number of Treatments", cex.lab = 1.3)
# dev.off()
# 
# time.steps = 1:nrow(total.At[1:144,])
# hour = floor(time.steps/12 + 14 + 35/60)%%24
# 
# hourly.At = data.frame("hour" = hour,
#                        "num.At" = rowMeans(total.At[1:144,]),
#                        "num.Xt" = rowMeans(total.At[137:nrow(total.At),])
#                        )
# 
# hourly.results.At = aggregate(num.At~hour, data = hourly.At, FUN = mean)
# hourly.results.Xt = aggregate(num.Xt~hour, data = hourly.At, FUN = mean)
# 
# altered.hour.At = (hourly.results.At$hour-14)%%24
# num.At = hourly.results.At$num.At[order(altered.hour.At)]
# 
# altered.hour.At = altered.hour.At[order(altered.hour.At)]
# 
# temp.df = data.frame(cbind(altered.hour.At, num.At))
# 
# pdf(file = "emablocking_fraction_actionreceived.pdf", width = 7, height=7)
# x.axis = temp.df$altered.hour.At
# y.axis = temp.df$num.At
# plot(x.axis, y.axis, pch = 16,
#      cex.lab=1.3, xlab = "Hour Block",
#      ylab = "Average Number of Treatments", col = "red",
#      ylim = c(0.00, 0.02))
# lines(x.axis, y.axis, lty = 1,
#       col = "red")
# dev.off()
# 
# #  Make curves per person
# set.of.users = unique(all.persondays[,1])
# user.mean = vector(length = length(set.of.users))
# for(i in 1:length(set.of.users)) {
#   user = set.of.users[i]
#   user.mean[i] = mean(colSums(total.At[1:144, all.persondays[,1] == user]), na.rm = TRUE)  
# }
# 
# # pdf(file = "emablocking_user_treatment_histogram.pdf", width = 10, height=7)
# hist(user.mean, xlab = "Number of Treatments", cex.lab = 1.3, main = "")
# # dev.off()
# 
# # sd(colSums(total.At[1:144, all.persondays[,1] == user]), na.rm = TRUE)/sqrt(nrow(all.persondays))
# 
# 
# global.x.axis = temp.df$altered.hour.At
# global.y.axis = temp.df$num.At
# 
# # pdf(file = "emablocking_user_fraction_actionreceived.pdf", width = 10, height=7)
# plot(global.x.axis, global.y.axis, pch = 16,
#      cex.lab=1.3, xlab = "Hour Block",
#      ylab = "Average Number of Treatments", col = "gray25",
#      lwd = 2, type = "l",     
#      ylim = c(0.00, 0.025))
# 
# for(user in set.of.users) {
#   
#   user.hourly.At = data.frame("hour" = hour,
#                               "num.At" = rowMeans(total.At[1:144,all.persondays[,1] == user]),
#                               "num.Xt" = rowMeans(total.At[137:nrow(total.At),all.persondays[,1] == user])
#   )
#   
#   user.hourly.results.At = aggregate(num.At~hour, data = user.hourly.At, FUN = mean)
#   user.hourly.results.Xt = aggregate(num.Xt~hour, data = user.hourly.At, FUN = mean)
#   
#   user.altered.hour.At = (user.hourly.results.At$hour-14)%%24
#   user.num.At = user.hourly.results.At$num.At[order(user.altered.hour.At)]
#   
#   user.altered.hour.At = user.altered.hour.At[order(user.altered.hour.At)]
#   
#   user.temp.df = data.frame(cbind(user.altered.hour.At, user.num.At))
#   
#   x.axis = user.temp.df$user.altered.hour.At
#   y.axis = user.temp.df$user.num.At
#   lines(x.axis, y.axis, lty = 1,
#         col = "lightgray")
# }
#   
# lines(global.x.axis, global.y.axis, lty = 2, lwd = 2,
#       col = "gray20")
# # dev.off()
# 

## Uniformity plots
## Calculate p.hat per person-day
if (!file.exists("simulation_phat.RDS")) {
  num.iters = 1000
  total.phat = foreach(i=1:nrow(all.persondays), .packages = c("mgcv", "chron"), .combine = cbind, .options.RNG =541891) %dorng% cv.assignment.multiple.fn(i,all.persondays, all.Ns, num.iters, prob.buckets.list)  
  saveRDS(total.phat, file = "simulation_phat.RDS")
} else {
  total.phat = readRDS("simulation_phat.RDS")
}
## Compute the squared distance
# results.phat = vector(length = ncol(total.phat))
# 
# for (col in 1:ncol(total.phat)) {
#   current.values = total.phat[,col]
#   current.Xt = current.values[137:(137+135)]
#   current.phat = current.values[1:144]
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


# total.phat = readRDS("total_phat.RDS")