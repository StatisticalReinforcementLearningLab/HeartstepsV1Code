## Required packages and source files
source("user_functions.R");require(mgcv); require(lubridate); 
require(foreach); require(lme4)

setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")
setwd("~//Documents/github/heartstepsdata/Walter/figs/")
bucket1 = c(14,17); bucket2 = c(18,21); bucket3 = c(22,1)
buckets = list(bucket1,bucket2, bucket3)

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") + minutes(40)

## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)

seq.hour = c(14:23,0:1)
param.list = allmodel.params(seq.hour, all.persons) # Compute all necessary model parameters
sedwidthdf.list = list.of.sedwidthdfs(seq.hour, window.time) # Compute all necessary dfs

## Setup
denom = 2
init.N = c(0.0,1.65)
eta = 0.0
lambda = 0.0

## Extract a person-day
set.seed("541891")
all.persons = as.matrix(unique(window.time[,1]), ncol = 1)
all.persondays = unique(window.time[,c(1,3)])

## Generate the 5 random partitions
partitions = sample(1:nrow(all.persons), nrow(all.persons), replace = FALSE)
block.size = ceiling(nrow(all.persons)/5)

all.persons = cbind(all.persons, unlist(lapply(1:nrow(all.persons), which.partition)))

all.persondays = data.frame(all.persondays)
names(all.persondays) = c("user", "block")

blockid = 1
N.one = c(0.0,1.63)
# otherblock.assignment.fn(all.persondays, blockid, N.one, all.persons, param.list, sedwidth.df)

blockid = 2
N.two = c(0.0,1.63)
# otherblock.assignment.fn(all.persondays, blockid, N.two, all.persons, param.list, sedwidth.df)

blockid = 3
N.three = c(0.0,1.62)
otherblock.assignment.fn(all.persondays, blockid, N.three, all.persons, param.list, sedwidth.df)

blockid = 4
N.four = c(0.0,1.63)
otherblock.assignment.fn(all.persondays, blockid, N.four, all.persons, param.list, sedwidth.df)

blockid = 5
N.five = c(0.0,1.63)
otherblock.assignment.fn(all.persondays, blockid, N.five, all.persons, param.list, sedwidth.df)

all.Ns= c(N.one[2], N.two[2], 
          N.three[2], N.four[2],
          N.five[2])

set.seed("541891")
total.At = sapply(1:nrow(all.persondays), cv.assignment.fn, all.persondays, all.Ns, param.list, sedwidthdf.list)

mean(colSums(total.At[1:136,]), na.rm = TRUE)
sd(colSums(total.At[1:136,]), na.rm = TRUE)/sqrt(nrow(all.persondays))

sim.df = data.frame(colSums(total.At[1:136,]))
names(sim.df) = c("ints")

pdf(file = "useradj_histogram_numtreatments.pdf", width = 7, height=7)
values = sim.df$ints
counts <- table(values)
counts <- counts/sum(counts);
names(counts) <- 0:(length(counts)-1);
barplot(counts, ylim = c(0, 0.35), ylab = "Percent", xlab = "Number of Treatments", cex.lab = 1.3)
dev.off()

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

pdf(file = "useradj_fraction_actionreceived.pdf", width = 7, height=7)
x.axis = temp.df$altered.hour.At
y.axis = temp.df$num.At
plot(x.axis, y.axis, pch = 16,
     cex.lab=1.3, xlab = "Hour Block",
     ylab = "Average Number of Treatments", col = "red",
     ylim = c(0.00, 0.03))
lines(x.axis, y.axis, lty = 1,
      col = "red")
dev.off()

#  Make curves per person

set.of.users = unique(all.persondays[,1])
user.mean = vector(length = length(set.of.users))
for(i in 1:length(set.of.users)) {
  user = set.of.users[i]
  user.mean[i] = mean(colSums(total.At[1:136, all.persondays[,1] == user]), na.rm = TRUE)  
}

pdf(file = "useradj_user_treatment_histogram.pdf", width = 10, height=7)
hist(user.mean, xlab = "Number of Treatments", cex.lab = 1.3, main = "")
sd(user.mean)
dev.off()

# sd(colSums(total.At[1:136, all.persondays[,1] == user]), na.rm = TRUE)/sqrt(nrow(all.persondays))


global.x.axis = temp.df$altered.hour.At
global.y.axis = temp.df$num.At

pdf(file = "useradj_user_fraction_actionreceived.pdf", width = 10, height=7)
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
dev.off()