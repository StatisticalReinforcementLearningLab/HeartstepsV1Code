## extra code for knitr document summary.Rnw

## plot a given user's daily step counts
daily.plot <- function(u) {
  d <- subset(daily, user == u)
  maxs <- max(1, d$jbsteps, d$gfsteps, na.rm = TRUE)
  maxd <- max(d$study.day, 42)
  plot(NULL, xlim = c(0, maxd), ylim = c(0, maxs),
       xlab = "", ylab = "", main = "", axes = FALSE, frame.plot = FALSE)
  mtext(paste(u), 2, line = 3, cex = 0.75)
  sapply(subset(d, is.na(ema.set.length))$study.day,
         function(j) abline(v = j, col = grey(0, 0.3)))
  meanjb <- mean(d$jbsteps, na.rm = TRUE)
  abline(h = meanjb, col = grey(0, 0.3))
  with(d, points(study.day, jbsteps, type = "l"))
  with(d, points(study.day, gfsteps, type = "l", lty = "dotted"))
  at <- c(0, with(d, study.day[(na.jbsteps & !lag1.na.jbsteps)
                               | (!na.jbsteps & lag1.na.jbsteps)][-1]), maxd)
  axis(1, at = at)
  axis(2, at = sort(round(c(0, meanjb, maxs))))
}

ema.scale.plot <- function(u) {
  d <- subset(daily, user == u)
  
  t <- rbind(table(d$hectic, useNA = "ifany"), 
             table(d$stressful, useNA = "ifany"), 
             table(d$typical, useNA = "ifany"))
  barplot(t, beside = T)
  mtext(paste(u), 2, line = 3, cex = 0.75)
}

## FIXME: add xtable function
