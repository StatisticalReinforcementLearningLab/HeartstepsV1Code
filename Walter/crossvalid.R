library(ggplot2)

## Required packages and source files
source("functions.R");require(mgcv); require(lubridate); require(foreach)
setwd("/Volumes/dav/HeartSteps/Walter/")
window.time = read.csv("window_time.csv")
Sedentary.values = read.csv("sed_values.csv")
Sedentary.length = read.csv("sed_length.csv")
setwd("~//Documents/github/heartstepsdata/Walter/figs/")

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

pdf(file = "fraction_sedentary.pdf", width = 7, height=7)
x.axis = (fraction.df$current.hour-14)%%24
plot(x.axis, fraction.df$mean, pch = 16,
     cex.lab=1.3, xlab = "Hour Block",
     ylab = "# of minutes 'Sedentary'", col = "red",
    ylim = c(0.46, 0.56))
lines(x.axis, fraction.df$mean, lty = 2,
     col = "red")
dev.off()

## Setup
init.N = c(0.0,1.74)
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
N.one = c(0.0,1.74)
# otherblock.assignment.fn(all.persondays, blockid, N.one)

blockid = 2
N.two = c(0.0,1.77)
# otherblock.assignment.fn(all.persondays, blockid, N.two)

blockid = 3
N.three = c(0.0,1.78)
# otherblock.assignment.fn(all.persondays, blockid, N.three)

blockid = 4
N.four = c(0.0,1.76)
# otherblock.assignment.fn(all.persondays, blockid, N.four)

blockid = 5
N.five = c(0.0,1.77)
# otherblock.assignment.fn(all.persondays, blockid, N.five)

all.Ns= c(N.one[2], N.two[2], 
          N.three[2], N.four[2],
          N.five[2])

set.seed("541891")
total.At = sapply(1:nrow(all.persondays), cv.assignment.fn, all.persondays, all.Ns)

mean(colSums(total.At[1:136,]), na.rm = TRUE)
sd(colSums(total.At[1:136,]), na.rm = TRUE)/sqrt(nrow(all.persondays))

sim.df = data.frame(colSums(total.At[1:136,]))
names(sim.df) = c("ints")

p1 <- ggplot(data=sim.df, aes(ints)) + geom_histogram() + xlab("Number of interventions")

## Plot of the average number of 
## interventions across person-days
p1 

pdf(file = "histogram_numtreatments.pdf", width = 7, height=7)
values = sim.df$ints
counts <- table(values)
counts <- counts/sum(counts);
names(counts) <- 0:(length(counts)-1);
barplot(counts, ylim = c(0, 0.3), ylab = "Percent", xlab = "Number of Treatments", cex.lab = 1.3)
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

p2 <- ggplot(data=temp.df, aes(x=altered.hour.At, y=num.At)) +
  geom_line()+
  geom_point()+ scale_y_continuous(limits = c(0.005, 0.02)) +
  xlab("Hour (0 = 9AM)") + ylab("Fraction of time receiving EMI per Hour")

## Plot of the fraction of time you 
## receive a treatment within each 1 
## hour bucket.
p2 

pdf(file = "fraction_actionreceived.pdf", width = 10, height=6)
x.axis = temp.df$altered.hour.At
y.axis = temp.df$num.At
plot(x.axis, y.axis, pch = 16,
     cex.lab=1.3, xlab = "Hour Block",
     ylab = "Average Number of Treatments", col = "red",
     ylim = c(0.005, 0.025))
lines(x.axis, y.axis, lty = 1,
      col = "red")
dev.off()
