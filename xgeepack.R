## geepack extras

## summarize linear combinations of regression coefficients
estimate <- function(fit, combos = NULL, var = fit$var, ztest = TRUE,
                     df = length(fit$id) - length(fit$coef)) {
  if (is.null(combos)) {
    combos <- diag(length(fit$coef))
    rownames(combos) <- names(fit$coef)
  }
  if (ztest) {
    qfun <- qnorm
    pfun <- function(q) pchisq(q, df = 1, lower.tail = FALSE)
  }
  else {
    qfun <- function(p) qt(p, df = df)
    pfun <- function(q) pf(q, lower.tail = FALSE, df1 = 1, df2 = df)
  }
  est <- combos %*% fit$coef
  if (is.null(var)) var <- fit$geese$vbeta
  se.est <- sqrt(diag(combos %*% var %*% t(combos)))
  lcl <- est - se.est * qfun(0.975)
  ucl <- est + se.est * qfun(0.975)
  pvalue <- pfun((est/se.est)^2)
  out <- cbind(est, lcl, ucl, se.est, pvalue)
  rownames(out) <- rownames(combos)
  colnames(out) <- c("Estimate", "95% LCL", "95% UCL", "SE", "p-value")
  class(out) <- "estimate"
  out
}

print.estimate <- function(object, digits = min(getOption("digits"), 3),
                           signif.stars = TRUE, ...) {
  printCoefmat(object, digits = digits, dig.tst = digits,
               signif.stars = signif.stars, has.Pvalue = TRUE,
               eps.Pvalue = 1e-4, ...)
}

plot.estimate <- function(object, xval = 0:(nrow(object) - 1), ...) {
  est <- object[, "Estimate"]
  lcl <- object[, "95% LCL"]
  ucl <- object[, "95% UCL"]
  sig <- object[, "p-value"]
  plot(NULL, xlim = c(min(xval), max(xval)), ylim = c(min(lcl), max(ucl)), ...)
  abline(h = 0, col = "lightgrey")
  sapply(1:nrow(object),
         function(j) segments(x0 = xval[j], y0 = lcl[j], y1 = ucl[j]))
  points(xval, est, pch = 21, bg = grey(sig))
}
