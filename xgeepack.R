## geepack and sandwich extras

library("Matrix")

if (!"package:sandwich" %in% search()) {
  bread <- function(x) UseMethod("bread")
  estfun <- function(x) UseMethod("estfun")
}
meat.default <- sandwich::meat
meat <- function(x) UseMethod("meat")

## define function like 'family$mu.eta', but second derivative
mu.eta2 <- function(link)
  switch(link,
         "identity" = function(eta) rep(0, length(eta)),
         "logit" = function(eta) (exp(eta) - exp(2 * eta)) / (1 + exp(eta))^3,
         function(eta) stop("Extend 'mu.eta2' to '", link, "' link function."))

## evaluate derivative of mean link function mu wrt linear predictor eta
dot.mu <- function(x, order = 1) {
  fun <- if (order == 1) x$family$mu.eta
         else x$family$mu.eta2
  as.vector(fun(x$linear.predictors))
}

## like 'geese.fit', but dispense with scale estimation, limit correlation
## structures and give output similar to 'geeglm'
geese.glm <- function(x, y, id,
                      offset = rep(0, N), soffset = rep(0, N),
                      weights = rep(1, N),
                      waves = NULL, zsca = matrix(1, N, 1),
                      zcor = NULL, corp = NULL,
                      control = geese.control(...),
                      b = NULL, alpha = NULL, gm = NULL,
                      family = gaussian(), mean.link = NULL,
                      variance = NULL, cor.link = "identity",
                      sca.link = "identity", link.same = TRUE,
                      scale.fix = TRUE, scale.value = 1,
                      corstr = c("independence", "ar1", "exchangeable",
                                 "fixed"), ...) {
  corstr <- match.arg(corstr)
  N <- length(id)
  z <- list()
  z$geese <- geese.fit(x = x, y = y, id = id,
                       offset = offset, soffset = soffset,
                       weights = weights,
                       waves = waves, zsca = zsca,
                       zcor = zcor, corp = corp,
                       control = control,
                       b = b, alpha = alpha, gm = gm,
                       family = family, mean.link = mean.link,
                       variance = variance, cor.link = cor.link,
                       sca.link = sca.link, link.same = link.same,
                       scale.fix = scale.fix, scale.value = scale.value,
                       corstr = corstr, ...)
  if (scale.fix) z$geese$gamma <- 1
  z$geese$X <- x
  z$y <- y
  z$family <- family
  ## second derivative of mean function (mu) wrt linear predictor (eta),
  z$family$mu.eta2 <- mu.eta2(family$link)
  z$id <- z$geese$id <- id
  z$offset <- offset
  z$prior.weights <- weights
  z$coefficients <- z$geese$beta
  z$corstr <- corstr
  z$linear.predictors <- if (is.null(z$offset)) z$geese$X %*% z$geese$beta
                         else z$offset + z$geese$X %*% z$geese$beta
  z$fitted <- z$family$linkinv(z$linear.predictors)
  z$residuals <- y - z$fitted
  class(z$geese) <- "geese"
  class(z) <- c("geeglm", "gee", "glm")
  z
}

## make an lm or glm object more like the geeglm class
glm2gee <- function(x, id = 1:length(fitted(x))) {
  x$id <- id
  x$corstr <- "independence"
  x$std.err <- "san.se"
  if (is.null(x$y)) x$y <- x$model[, 1]
  if (is.null(x$linear.predictors)) x$linear.predictors <- x$fitted.values
  if (is.null(x$family)) x$family <- gaussian(link = "identity")
  x$family$mu.eta2 <- mu.eta2(x$family$link)
  z <- matrix(0, 1, 1)
  b <- coef(x)
  x$geese <- list(alpha = numeric(0), beta = b, gamma = 1,
                  vbeta = z, vbeta.ajs = z, vbeta.j1s = z, vbeta.fij = z,
                  valpha = z, valpha.ajs = z, valpha.j1s = z, valpha.fij = z,
                  vgamma = z, vgamma.ajs = z, vgamma.j1s = z, vgamma.fij = z,
                  clusz = as.vector(table(id)),
                  model = list(scale.fix = TRUE, corstr = x$corstr),
                  call = x$call)
  if (inherits(x, "glm")) {
    s <- summary(x)
    x$geese$model$scale.fix <- FALSE
    x$geese$gamma <- 1 / s$dispersion
  }
  else if (!is.null(weights)) x$prior.weights <- x$weights
  else x$prior.weights <- rep(1, length(id))
  class(x$geese) <- "geese"
  class(x) <- c("geeglm", "gee", "glm", "lm")
  x
}

## return cluster sizes, with clusters identified via the 'id' argument
cluster.size <- function(x) x$geese$clusz

## return sample size
cluster.number <- function(x) length(x$geese$clusz)

## like 'bdiag', but make results similar to 'cbind' or 'rbind'
block.diag <- function(...) {
  l <- list(...)
  as.matrix(do.call("bdiag", l[sapply(l, length) != 0]))
}

## extract bread from geeglm's sandwich variance estimator
## nb: positive definite
bread.geeglm <- function(x, wcovinv = NULL, sum = TRUE, invert = TRUE, ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  b <- mapply(function(D, V) t(D) %*% V %*% D,
              D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
              V = wcovinv,
              SIMPLIFY = FALSE)
  if (sum) b <- Reduce("+", b)
  if (invert) b <- solve(b)
  b
}

## extract projection matrices
leverage <- function(x, wcovinv = NULL, invert = TRUE) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  b <- bread.geeglm(x, wcovinv)
  g <- if (invert) function(m) solve(diag(nrow(m)) - m)
       else identity
  mapply(function(D, V, k) g(D %*% b %*% t(D) %*% V),
         D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
         V = wcovinv,
         SIMPLIFY = FALSE)
}

## extract geeglm's estimating function
estfun.geeglm <- function(x, wcovinv = NULL, small = TRUE, ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  ## apply Mancl and DeRouen's (2001) small sample correction
  if (is.logical(small)) small <- small * 50
  n <- cluster.number(x)
  scale <- if (n <= small) leverage(x, wcovinv)
           else lapply(cluster.size(x), function(k) diag(1, k))
  e <- mapply(function(D, V, r, S) t(D) %*% V %*% (S %*% r),
              D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
              V = wcovinv,
              r = split(x$y - x$fitted.values, x$id),
              S = scale,
              SIMPLIFY = FALSE)
  do.call("rbind", lapply(e, t))
}

## return derivative of geeglm's estimating function wrt regression coefficients
## nb: this form does not consider the asymptotic approximation (see last line in
##     the Appendix of Liang and Zeger, 1986), possible when the model is
##     correctly specified
dot.estfun <- function(x, wcovinv = NULL, ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  b <- bread.geeglm(x, wcovinv, sum = FALSE, invert = FALSE)
  mapply(function(D, V, r, X) t(D) %*% V %*% diag(r) %*% X - b,
         D = split.data.frame(model.matrix(x) * dot.mu(x, 2), x$id),
         V = wcovinv,
         r = split(x$y - x$fitted.values, x$id),
         X = split.data.frame(model.matrix(x), x$id),
         SIMPLIFY = FALSE)
}

## extract meat from geeglm's sandwich variance estimator, where:
## 'x' is the model object for (lagged) treatment effects
## 'denom' is the model object for the "denominator" treatment probability
## 'num' is the model object for the "numerator" treatment probability
## FIXME: insert code to correct for estimated probabilities
meat.geeglm <- function(x, denom = NULL, num = NULL, lag = 0, wcovinv = NULL, 
                        trtlabel = "", ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  ## nb: small sample correction threshold can be set via '...'; no correction is
  ##     applied to the estimating functions from 'denom' and 'num'
  psi <- estfun.geeglm(x, wcovinv = wcovinv, ...)
  Reduce("+", lapply(split(psi, 1:nrow(psi)), function(row) row %o% row))
}

## extract geeglm's working covariance matrices
## nb: like glm, the 'weights' argument specifies the prior weight for the
##     scale parameter of the working variance function
working.covariance <- function(x, invert = FALSE, wcor = NULL) {
  if (is.null(wcor)) R <- working.correlation(x)
  g <- if (invert) solve else identity
  mapply(function(a, phi, w, k) g(diag(phi / w, k) %*% diag(a, k) %*%
                                  R[1:k, 1:k, drop = FALSE] %*% diag(a, k)),
         a = split(sqrt(x$family$variance(fitted(x))), x$id),
         phi = x$geese$gamma^(x$geese$model$scale.fix - 1),
         w = split(x$prior.weights, x$id),
         k = cluster.size(x),
         SIMPLIFY = FALSE)
}

## extract geeglm's working correlation matrix
working.correlation <- function(x, ...) {
  R <- diag(max(cluster.size(x)))
  if (length(x$geese$alpha)) R[lower.tri(R) | upper.tri(R)] <- x$geese$alpha
  if (x$corstr == "ar1") R <- R^abs(col(R) - row(R))
  R
}

## calculate the sandwich estimator of the covariance matrix for the regression
## coefficients
vcov.geeglm <- function(x, denom = NULL, num = NULL, ...) {
  v <- x$vcov
  if (is.null(v)) {
    b <- bread(x)
    m <- meat.geeglm(x, denom, num, ...)
    v <- b %*% m %*% t(b)
  }
  v
}

## summarize linear combinations of regression coefficients, where:
## 'combos' is a matrix whose rows give the linear combinations
## 'null' is the value of each combintation under the null hypothesis
## 'omnibus' indicates that the specified combinations should be tested
##           simultaneously instead of individually
estimate <- function(x, combos = NULL, omnibus = FALSE, null = 0, small = TRUE,
                     conf.int = 0.95, normal = FALSE, ...) {
  if (is.null(combos)) {
    combos <- diag(length(coef(x)))
    rownames(combos) <- names(coef(x))
    omnibus <- FALSE
  }
  est <- combos %*% coef(x)
  if (nrow(est) != length(null)) null <- rep(null[1], nrow(est))
  ## apply Mancl and DeRouen's (2001) small sample correction
  if (is.logical(small)) small <- small * 50
  n <- cluster.number(x)
  d1 <- if (omnibus) nrow(combos)
        else apply(combos != 0, 1, sum)
  d2 <- n - length(coef)
  ## apply Hotelling's T-squared test, following Liao et al. (2016)
  if (n <= small & !normal) {
    type <- "Hotelling"
    adj <- d1 * (d1 + d2 - 1) / d2
    qfun <- function(p) sqrt(adj * mapply(qf, p = p, df1 = d1, df2 = d2))
    pfun <- function(q) 1 - mapply(pf, q = q / adj, df1 = d1, df2 = d2)
  }
  else {
    type <- "Wald"
    qfun <- if (normal) function(p) qnorm((1 + p) / 2)
            else function(p) sqrt(adj * mapply(qf, p = p, df1 = d1, df2 = d2))
    pfun <- if (normal) function(q) 1 - mapply(pchisq, q = q, df = d1)
            else 1 - mapply(pf, q = q, df1 = d1, df2 = d2)
  }
  var.est <- combos %*% vcov(x) %*% t(combos)
  se.est <- sqrt(diag(var.est))
  crit <- qfun(conf.int)
  lcl <- est - se.est * crit
  ucl <- est + se.est * crit
  stat <- if (omnibus) rep(t(est - null) %*% solve(var.est) %*% (est - null), d1)
          else (est - null)^2 / diag(var.est)
  pvalue <- pfun(stat)
  out <- cbind(est, lcl, ucl, se.est, stat, pvalue)
  rownames(out) <- rownames(combos)
  colnames(out) <- c("Estimate",
                     paste0(round(conf.int * 100), "% ", c("LCL", "UCL")),
                     "SE", type, "p-value")
  class(out) <- "estimate"
  out
}

print.estimate <- function(object, digits = min(getOption("digits"), 3),
                           signif.stars = TRUE, eps.pvalue = 1e-4, ...) {
  printCoefmat(object, digits = digits, dig.tst = digits,
               signif.stars = signif.stars, has.Pvalue = TRUE,
               eps.Pvalue = eps.pvalue, ...)
}
