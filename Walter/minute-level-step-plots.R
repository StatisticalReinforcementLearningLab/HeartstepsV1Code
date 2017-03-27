source("init.R")
setwd(sys.var$mbox.data)
load("analysis-small.RData")
setwd(sys.var$repo)

## Given a user and day index (i.e., study.day.nogap), plot minute-by-minute
## step counts from 12:00:00 AM to 11:59:59 PM

plot.minute.steps <- function(id, day) {
  if (!(day %in% daily$study.day.nogap[daily$user == id])) 
    stop(paste("Day", day, "does not exist for user", id))
  day.date <- daily$study.date[daily$user == id & daily$study.day.nogap == day
                            & !is.na(daily$study.day.nogap)]
  d <- subset(jawbone, user == id & start.date == as.Date(day.date))
  d$start.local.strip <- as.POSIXlt(d$start.utime.local)
  d$day.time <- d$start.local.strip$hour + d$start.local.strip$min / 60 + d$start.local.strip$sec / 60
  with(d, plot(steps ~ (day.time-5)%%24, type = "p", pch = 20, xlim = c(0, 24), ylim = c(-20, 210),
               ylab = "Minute-level step count", xlab = "Hours since midnight",
               main = paste0("Minute-by-minute step count\nUser ", id, ", Day ", day,
                             " (", day.date, ")")))
  segments((d$day.time-5)%%24, 0, y1 = d$steps)
  
  dsuggest <- subset(suggest, user == id & study.date == as.Date(day.date))
  dsuggest$start.local.strip <- as.POSIXlt(dsuggest$utime.stamp)
  dsuggest$day.time <- dsuggest$start.local.strip$hour + dsuggest$start.local.strip$min / 60 + dsuggest$start.local.strip$sec / 60
  temp = (dsuggest$day.time[dsuggest$link==1 & dsuggest$avail == TRUE]-5)%%24
  points(temp, rep(-5,length(temp)), pch = 16, col = "red")
  segments(temp-0.5, rep(-5,length(temp)), temp+0.5)
  
  temp.nolink = (dsuggest$day.time[dsuggest$link==0 & dsuggest$avail == TRUE]-5)%%24
  points(temp.nolink, rep(-10,length(temp.nolink)), pch = 15, col = "red")
  segments(temp.nolink-0.5, rep(-10,length(temp.nolink)), temp.nolink+0.5)
  
  temp.unavail = (dsuggest$day.time[dsuggest$avail == FALSE]-5)%%24
  points(temp.unavail, rep(-20,length(temp.unavail)), pch = 15, col = "blue")
  segments(temp.unavail-0.5, rep(-20,length(temp.unavail)), temp.unavail+0.5)
  
}

## Generate 20 plots at random
## NB: This doesn't pull from user 35, who disappeared after 3 weeks
## NB: Only pulls up to an individual's day 35 on study
for (i in as.numeric(sample(rownames(daily[!is.na(daily$study.day.nogap) &
                                           daily$study.day.nogap <= 35 &
                                           daily$user != 35, ]),
                            20))) { #this is the number of plots to draw
  plot.minute.steps(daily$user[i], daily$study.day.nogap[i])
}

user = 11
obs = which(daily$user == user)
for (i in obs[1:6]) {
  mypath <- file.path("/Users/walterdempsey/Dropbox/Presentations/Latent Variables",paste("user11_day", daily$study.day.nogap[i], ".png", sep = ""))
  png(file=mypath)
  plot.minute.steps(daily$user[i], daily$study.day.nogap[i])
  dev.off()
}
