require(doParallel)

# reads the list of nodes which have been allocated
# by the cluster queue manager
nodefile <- Sys.getenv("PBS_NODEFILE")
hostlist <- read.table(nodefile, skip=1, header=FALSE)

# builds a socket cluster using these nodes
cl <- makeCluster(c(as.character(hostlist$V1)), type='SOCK')

registerDoParallel(cl)

## Required packages and source files
source("functions.R");require(mgcv); require(lubridate); require(foreach)
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

# png("../fraction_sed.png", width = 6.5,height = 3, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(5,4,1,1)+0.1)
plot((fraction.df$current.hour-14)%%24,fraction.df$mean, 
     xlim = c(0,12),
     ylim = c(0.24, 0.27), axes = FALSE, 
     ylab = "Fraction of time 'Sedentary'",
     xlab = "Hour (0 = 9AM) ")
axis(side = 1); axis(side = 2)
# dev.off()

## Setup
N = c(0.0,2.25)
eta = 0.0
lambda = 0.0

## Extract a person-day
set.seed("541891")
all.persondays = unique(window.time[,c(1,3)])
num.iters = 2000

total.At = foreach(i=1:num.iters,  .combine='cbind') %do% random.assignment.fn(all.persondays)

mean(colSums(total.At[1:136,]), na.rm = TRUE)
sd(colSums(total.At[1:136,]), na.rm = TRUE)/sqrt(num.iters)

png("../summary_plot_At.png", width = 6.5, height = 4, units = "in", res = 300)
par(mfrow = c(1,2), mar = c(5,4,1,1)+0.1)

hist(colSums(total.At[1:136,]), main = "", xlab = "# of interventions", cex.axis = 0.75, cex.lab = 0.75)

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

plot(altered.hour.At,num.At, axes = FALSE, 
     ylab = "Fraction of time receiving EMI per Hour", xlab = "Hour", 
     pch = 18, ylim = c(0.005, 0.02),
     xlim = c(0,12), cex.lab = 0.75)
axis(side = 1, cex.axis = 0.75); axis(side = 2, cex.axis = 0.75)

lw1 = loess(num.At~altered.hour.At)

lines(altered.hour.At, lw1$fitted,col = "red", lty = 2)
dev.off()

png("../summary_plot_Xt.png", width = 6.5,height = 4, units = "in", res = 300)
altered.hour.Xt = (hourly.results.Xt$hour-14)%%24
num.Xt = hourly.results.Xt$num.Xt[order(altered.hour.Xt)]

altered.hour.Xt = altered.hour.Xt[order(altered.hour.Xt)]

par(mfrow = c(1,1), mar = c(5,4,1,1)+0.1)
plot(altered.hour.Xt,num.Xt, axes = FALSE, ylab = "Fraction of time classified as 'Sedentary' ", xlab = "Hour", 
     pch = 18, ylim = c(0.2,0.35), xlim = c(0,12))
axis(side = 1); axis(side = 2)

lw1 = loess(num.Xt~altered.hour.Xt)

lines(altered.hour.Xt, lw1$fitted,col = "red", lty = 2)
dev.off()

stopCluster(cl)