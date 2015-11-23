## extra code for knitr document summary.Rnw

## plot a given user's daily step counts
plot.daily.steps <- function(u) {
  d <- subset(daily, user == u)
  maxs <- max(1, d$jbsteps, d$gfsteps, na.rm = TRUE)
  maxd <- max(d$study.day)
  plot(NULL, xlim = c(0, max(maxd, 42)), ylim = c(0, maxs),
       xlab = "", ylab = "", main = "", axes = FALSE, frame.plot = FALSE)
  mtext(paste(u), 2, line = 3)
  sapply(subset(d, is.na(ema.set.length) & study.day < maxd)$study.day,
         function(j) abline(v = j, col = grey(0, 0.3)))
  meanjb <- mean(d$jbsteps, na.rm = TRUE)
  segments(0, meanjb, max(d$study.day, 42 * (d$last.date[1] > max.date)), meanjb,
           col = grey(0, 0.3))
  with(d, points(study.day, jbsteps, type = "l"))
  with(d, points(study.day, gfsteps, type = "l", lty = "dotted"))
  at <- c(0, with(d, c(study.day[na.jbsteps & !lag1.na.jbsteps] - 1,
                       study.day[!na.jbsteps & lag1.na.jbsteps], maxd, 42)))
  axis(1, at = sort(unique(at)))
  axis(2, at = sort(round(c(0, meanjb, maxs))))
}

## FIXME: add xtable function
