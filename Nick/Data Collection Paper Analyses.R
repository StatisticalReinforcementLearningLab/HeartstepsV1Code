### Descriptive and supplemental analyses for
### Data collection paper

### Nick Seewald

source("init.R")
setwd(sys.var$mbox.data)
load("analysis.RData")
load("csv.RData")
setwd(sys.var$repo)

color <- "royalblue1"

## Decide minimum time on study for inclusion in analysis
## and subset data to exclude participants who don't meet the threshold. 
analysis.data <- function(days = 0:35, max.day = 41) {
  ids  <- unique(suggest$user[suggest$study.day.nogap == rev(days)[1] &
                                !is.na(suggest$study.day.nogap)])
  d <- subset(suggest, !is.na(study.day.nogap) & user %in% ids & 
                !(avail == F & send == T) & study.day.nogap <= max.day &
                !is.na(send.active),
              select = c(user, study.day.nogap, decision.index.nogap, decision.utime,
                         slot, study.date, intake.date, intake.utime, intake.slot,
                         travel.start, travel.end, exit.date, dropout.date,
                         last.date, last.utime, last.slot, recognized.activity,
                         avail, send, send.active, send.sedentary, jbsteps10, 
                         jbsteps10.zero, jbsteps10.log, jbsteps30pre,
                         jbsteps30, jbsteps30pre.zero, jbsteps30.zero, 
                         jbsteps30pre.log, jbsteps30.log, jbsteps60pre,
                         jbsteps60, jbsteps60pre.zero, jbsteps60.zero,
                         jbsteps60pre.log, jbsteps60.log, response, location.category))
  return(list(data = d, ids = ids))
}
days <- 0:35
primary <- analysis.data(days = days)
ids     <- primary$ids
primary <- primary$data

##### Plots #####
## Plot mean daily step count for jawbone and google fit
jbdays <- aggregate(jbsteps.direct ~ study.day.nogap, data = subset(daily, user %in% ids & study.day.nogap <= 41), mean)
gfdays <- aggregate(gfsteps ~ study.day.nogap, data = subset(daily, user %in% ids & study.day.nogap <= 41), mean)

setEPS()
postscript(file = "C:/Users/nseew/Box Sync/Susan/Heartsteps/Data Collection Paper/avg-daily-stepcount.eps", width = 5.07874, height = 3)
par(mar = c(3.5, 3, 1, 0) + 0.5, mgp = c(2.25, 0.25, 0), oma = c(0, 0, 0, 0), 
    las = 1, tcl = 0.25, ps = 9)
plot(jbdays$jbsteps.direct ~ jbdays$study.day.nogap, type = "l", lty = 5, lwd = 2, ylim = c(1000,9700), xlab = "", ylab = "")
lines(gfdays$gfsteps ~ gfdays$study.day.nogap, type = "l", lty = 1, lwd = 2, col = color)
title(xlab = "Day on study", ylab = "Average daily step count")
legend("bottom", legend = c("Jawbone", "Google Fit"), lty = c(5, 1), lwd = 2, col = c("black", color), horiz = TRUE, cex = .8)
dev.off()