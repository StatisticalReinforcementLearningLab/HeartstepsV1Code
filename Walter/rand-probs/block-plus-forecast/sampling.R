library(ggplot2)
# require(doParallel)

# # reads the list of nodes which have been allocated
# # by the cluster queue manager
# nodefile <- Sys.getenv("PBS_NODEFILE")
# hostlist <- read.table(nodefile, skip=1, header=FALSE)
# 
# # builds a socket cluster using these nodes
# cl <- makeCluster(c(as.character(hostlist$V1)), type='SOCK')
# 
# registerDoParallel(cl)

## Required packages and source files
source("functions.R");require(mgcv); require(lubridate); require(foreach)
setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")

window.time$window.utime = as.POSIXct(window.time$window.utime, tz = "GMT") + minutes(40)

## Create a data.frame for Expected time Remaining
## Range of current hour = c(14:23,0:1)

seq.hour = c(14:23,0:1)
fraction.data = matrix(nrow = length(seq.hour), ncol = 3)

for (i in 1:length(seq.hour)) {
  current.hour = seq.hour[i]
  result = fraction.time.in.state(current.hour)
  fraction.data[i,] = c(current.hour,result$mean,result$var)
}

fraction.df = data.frame(fraction.data)
names(fraction.df) = c("current.hour", "mean", "var")

png("/figs/fraction_sed.png", width = 6.5,height = 3, units = "in", res = 300)
ggplot(data=fraction.df, aes(x=(current.hour-14)%%24, y=mean)) +
  geom_line()+
  geom_point()+ scale_y_continuous(limits = c(0.45, 0.55)) +
  xlab("Hour (0 = 9AM)") + ylab("Fraction of time 'Sedentary'")
dev.off()

## Setup
N = c(0.0,1.74)
eta = 0.0
lambda = 0.0

## Extract a person-day
set.seed("541891")
all.persondays = unique(window.time[,c(1,3)])
num.iters = 2000

total.At = foreach(i=1:num.iters,  .combine='cbind') %do% random.assignment.fn(all.persondays)

mean(colSums(total.At[1:136,]), na.rm = TRUE)
sd(colSums(total.At[1:136,]), na.rm = TRUE)/sqrt(num.iters)

sim.df = data.frame(colSums(total.At[1:136,]))
names(sim.df) = c("ints")

p1 <- ggplot(data=sim.df, aes(ints)) + geom_histogram() + xlab("Number of interventions")

## Plot of the average number of 
## interventions across person-days
p1 

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

p2 <- ggplot(data=temp.df, aes(x=altered.hour.At, y=num.At)) +
  geom_line()+
  geom_point()+ scale_y_continuous(limits = c(0.005, 0.02)) +
  xlab("Hour (0 = 9AM)") + ylab("Fraction of time receiving EMI per Hour")

## Plot of the fraction of time you 
## receive a treatment within each 1 
## hour bucket.
p2 

# stopCluster(cl)
