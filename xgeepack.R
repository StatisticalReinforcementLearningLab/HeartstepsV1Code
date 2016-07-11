## geepack and sandwich extras

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
  ## nb: geeglm doesn't consistently return a vector for the linear predictor,
  ##     fitted values and residuals
  z$linear.predictors <- if (is.null(z$offset)) z$geese$X %*% z$geese$beta
                         else z$offset + z$geese$X %*% z$geese$beta
  z$linear.predictors <- as.vector(z$linear.predictors)
  z$fitted.values <- as.vector(z$family$linkinv(z$linear.predictors))
  z$residuals <- as.vector(y - z$fitted.values)
  class(z$geese) <- "geese"
  class(z) <- c("geeglm", "gee", "glm")
  z
}

if (!"package:sandwich" %in% search()) {
  bread <- function(x, ...) UseMethod("bread")
  estfun <- function(x, ...) UseMethod("estfun")
}
meat.default <- sandwich::meat
meat <- function(x, ...) UseMethod("meat")

## define function like 'family$mu.eta', but second derivative
mu.eta2 <- function(link)
  switch(link,
         "identity" = function(eta) rep(0, length(eta)),
         "logit" = function(eta) (exp(eta) - exp(2 * eta)) / (1 + exp(eta))^3,
         function(eta) stop("Extend 'mu.eta2' to '", link, "' link function."))

## evaluate derivative of mean link function mu wrt linear predictor eta
dot.mu <- function(x, order = 1) {
  fun <- if (order == 1) x$family$mu.eta
         else if (is.null(x$family$mu.eta2)) mu.eta2(x$family$link)
         else x$family$mu.eta2
  as.vector(fun(x$linear.predictors))
}

## make an lm or glm object more like the geeglm class
glm2gee <- function(x, id) {
  if (missing(id))
    x$id <- 1:length(x$fitted.values)
  else {
    ids <- substitute(id)
    x$id <- if (exists(deparse(ids), envir = parent.frame())) id
            else eval(ids, eval(x$call$data))
    if (!is.null(x$call$subset)) x$id <- x$id[eval(x$call$subset)]
  }
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
                  clusz = as.vector(table(x$id)),
                  model = list(scale.fix = TRUE, corstr = x$corstr),
                  call = x$call)
  if (inherits(x, "glm")) {
    s <- summary(x)
    x$geese$model$scale.fix <- FALSE
    x$geese$gamma <- 1 / s$dispersion
  }
  else if (!is.null(x$weights)) x$prior.weights <- x$weights
  else x$prior.weights <- rep(1, length(id))
  class(x$geese) <- "geese"
  class(x) <- c("geeglm", "gee", "glm", "lm")
  x
}

## return cluster sizes, with clusters identified via the 'id' argument
cluster.size <- function(x) x$geese$clusz

## return (effective) number of clusters
cluster.number <- function(x, overall = TRUE) {
  if (overall) length(x$geese$clusz)
  else length(unique(x$id[x$prior.weights != 0]))
}

## extract bread from geeglm's sandwich variance estimator
## (i.e. the derivative of estimating function wrt regression coefficients)
## nb: under the non-identity link, the asymptotic approximation (last line in
##     the Appendix of Liang and Zeger, 1986), is valid when the model is
##     correctly specified
bread.geeglm <- function(x, wcovinv = NULL, invert = TRUE, approx = TRUE, ...) {
  approx <- approx & x$family$link != "identity"
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  g <- if (approx) function(D, V, r, X, k) 0
       else function(D, V, r, X, k) t(D) %*% V %*% diag(r, k) %*% X
  b <- mapply(function(D, DD, V, r, X, k) g(DD, V, r, X, k) - t(D) %*% V %*% D,
              D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
              DD = split.data.frame(model.matrix(x) * dot.mu(x, 2), x$id),
              V = wcovinv,
              r = split(x$y - x$fitted.values, x$id),
              X = split.data.frame(model.matrix(x), x$id),
              k = cluster.size(x),
              SIMPLIFY = FALSE)
  b <- Reduce("+", b)
  if (invert) b <- solve(b)
  b
}

## extract projection matrices
leverage <- function(x, wcovinv = NULL, invert = TRUE) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  B <- -bread.geeglm(x, wcovinv)
  g <- if (invert) function(m) solve(diag(nrow(m)) - m)
       else identity
  mapply(function(D, V, k) g(D %*% B %*% t(D) %*% V),
         D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
         V = wcovinv,
         SIMPLIFY = FALSE)
}

## extract geeglm's estimating function
estfun.geeglm <- function(x, wcovinv = NULL, small = TRUE, ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  ## apply Mancl and DeRouen's (2001) small sample correction
  if (is.logical(small)) small <- small * 50
  n <- cluster.number(x, overall = FALSE)
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

## extract meat from geeglm's sandwich variance estimator, where:
## 'x' is the model object for (lagged) effects
## 'pd' gives "denominator" treatment probability
## 'pn' gives "numerator" treatment probability
## 'label' is the term label for the main treatment effect
meat.geeglm <- function(x, pn = NULL, pd = pn, lag = 0, wcovinv = NULL,
                        label = NULL, ...) {
  if (is.null(wcovinv)) wcovinv <- working.covariance(x, invert = TRUE)
  ## nb: small sample correction threshold can be set via '...'; no correction is
  ##     applied to the estimating functions from 'pd' and 'pn'
  u <- estfun.geeglm(x, wcovinv = wcovinv, ...)
  ## any centering or weighting with estimated probabilities?
  if (inherits(pd, "geeglm")) {
    if (is.null(pn)) stop("Specify a non-NULL numerator probability 'pn'.")
    if (x$family$link != "identity")
      stop("Only the identity link is supported under centering or weighting.")
    ## centering?
    center <- inherits(pn, "geeglm")
    ## weighting?
    weight <- !identical(pd, pn)
    ## return cluster-level derivative (terms) of...
    ## ...effect estimating function wrt treatment probability
    Ux.p <- function(D, V, r, k, Dp, j) t(D) %*% V %*% diag(r, k) %*% Dp[j, ]
    ## ...(observed) treatment probability wrt its regression model coefficients
    Up.coef <- function(p, one = TRUE)
      model.matrix(p) * dot.mu(p) /
        ifelse(p$y == 1 | one, p$fitted.values, p$fitted.values - 1)
    ## evaluate general expression for extra additive term in meat
    extra <- function(p, Sig) {
      Vinv <- working.covariance(p, invert = TRUE)
      B <- bread.geeglm(p, wcovinv = Vinv, invert = TRUE, approx = FALSE)
      S <- Sig %*% B
      up <- estfun.geeglm(pd, wcovinv = Vinv)
      do.call("rbind", lapply(split(up, 1:nrow(up)),
                              function(Up) as.vector(S %*% Up)))
    }
    if (weight) {
      ## keep aligned with observations in 'x'
      obs <- align.obs(x, pd, lag)
      Sig <- mapply(Ux.p,
                    D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
                    V = wcovinv,
                    r = split(x$y - x$fitted.values, x$id),
                    k = cluster.size(x),
                    Dp = split.data.frame(Up.coef(pd, one = FALSE), pd$id),
                    j = obs,
                    SIMPLIFY = FALSE)
      u <- u + extra(pd, Reduce("+", Sig))
    }
    if (center) {
      if (is.null(label)) stop("Specify a non-NULL treatment term label.")
      ## indices of design matrix related to treatment effects
      k <- which.terms(x, label)
      obs <- align.obs(x, pn, lag)
      Sig1 <- mapply(Ux.p,
                     D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
                     V = wcovinv,
                     r = split(x$y - x$fitted.values, x$id),
                     k = cluster.size(x),
                     Dp = split.data.frame(Up.coef(pn, one = FALSE), pn$id),
                     j = obs,
                     SIMPLIFY = FALSE)
      Sig1 <- Reduce("+", Sig1)
      ## design matrix component in second term of partial derivative is...
      mm2 <- model.matrix(x)
      ## ... zero in columns for main effect
      mm2[, -k] <- 0
      ## ... scaled by negative probability in columns for treatment effect
      mm2[, k] <- mm2[, k] / as.vector(mm2[, k[1]])
      mm2[, k] <- -mm2[, k] * as.vector(pn$fitted.values)[unlist(obs)]
      Sig2 <- mapply(Ux.p,
                     D = split.data.frame(mm2 * dot.mu(x), x$id),
                     V = wcovinv,
                     r = split(x$y - x$fitted.values, x$id),
                     k = cluster.size(x),
                     Dp = split.data.frame(Up.coef(pn), pn$id),
                     j = obs,
                     SIMPLIFY = FALSE)
      Sig2 <- Reduce("+", Sig2)
      ## residual component in third term reduces to probability factor
      resid3 <- as.vector(model.matrix(x)[, k, drop = FALSE] %*% coef(x)[k] *
                          as.vector(pn$fitted.values)[unlist(obs)])
      Sig3 <- mapply(Ux.p,
                     D = split.data.frame(model.matrix(x) * dot.mu(x), x$id),
                     V = wcovinv,
                     r = split(resid3, x$id),
                     k = cluster.size(x),
                     Dp = split.data.frame(Up.coef(pn), pn$id),
                     j = obs,
                     SIMPLIFY = FALSE)
      Sig3 <- Reduce("+", Sig3)
      u <- u + extra(pn, Sig1 + Sig2 + Sig3)
    }
  }
  Reduce("+", lapply(split(u, 1:nrow(u)), function(row) row %o% row))
}

## return indices for observations in model 'p' that are aligned with model 'x'
align.obs <- function(x, p, lag) {
  if (!identical(unique(x$id), unique(p$id)))
    stop("Treatment probabiliy model(s) should be based on the same sample.")
  obs.beg <- as.vector(table(p$id) - table(x$id)) + 1 - lag
  if (any(obs.beg < 1))
    stop("Treatment probability model(s) based on too few observations.")
  obs.end <- as.vector(table(p$id)) - lag
  mapply(function(i, j) i:j, obs.beg, obs.end, SIMPLIFY = FALSE)
}

## return model 'x' design matrix column indices based on the term 'label'
which.terms <- function(x, label) {
  f <- attributes(x$terms)$factors
  j <- which(colnames(f) == label)
  if (length(j) != 1) stop("Treatment term label not found in 'x'.")
  k <- which(f[rownames(f) == label, ] != 0)
  w <- k >= attributes(x$terms)$intercept
  if (j %in% k[w]) j <- j + 1
  k[w] <- k[w] + 1
  c(j, k[k != j])
}

## extract geeglm's working covariance matrices
## nb: like glm, the 'weights' argument specifies the prior weight for the
##     scale parameter of the working variance function
working.covariance <- function(x, invert = FALSE, wcor = NULL) {
  if (is.null(wcor)) R <- working.correlation(x)
  phi <- x$geese$gamma^(x$geese$model$scale.fix - 1)
  g <- if (invert) function(V, w, k) diag(w, k) %*% solve(V)
       else function(V, w, k) diag(ifelse(w == 0, 0, 1 / w), k) %*% V
  mapply(function(a, s, w, k) g(phi * diag(a, k) %*%
                                R[1:k, 1:k, drop = FALSE] %*% diag(a, k), w, k),
         a = split(sqrt(x$family$variance(x$fitted.values)), x$id),
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
vcov.geeglm <- function(x, ...) {
  V <- x$vcov
  if (is.null(V)) {
    W <- working.covariance(x, invert = TRUE)
    B <- bread.geeglm(x, wcovinv = W)
    M <- meat.geeglm(x, wcovinv = W, ...)
    V <- B %*% M %*% t(B)
  }
  V
}

## summarize linear combinations of regression coefficients, where:
## 'combos' is a matrix whose rows give the linear combinations
## 'null' gives the value of each combintation under the null hypothesis
## 'omnibus' indicates that the specified combinations should be tested
##           simultaneously instead of individually
estimate <- function(x, combos = NULL, omnibus = FALSE, null = 0,
                     small = TRUE, conf.int = 0.95, normal = FALSE, ...) {
  if (is.null(combos)) {
    combos <- diag(length(coef(x)))
    rownames(combos) <- names(coef(x))
    omnibus <- FALSE
  }
  est <- combos %*% coef(x)
  if (nrow(est) != length(null)) null <- rep(null[1], nrow(est))
  ## apply Mancl and DeRouen's (2001) small sample correction
  if (is.logical(small)) small <- small * 50
  n <- cluster.number(x, overall = FALSE)
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
  class(out) <- c("estimate", "matrix")
  out
}

print.estimate <- function(object, digits = min(getOption("digits"), 3),
                           signif.stars = TRUE, eps.pvalue = 1e-4, ...) {
  printCoefmat(object, digits = digits, dig.tst = digits,
               signif.stars = signif.stars, has.Pvalue = TRUE,
               eps.Pvalue = eps.pvalue, ...)
}
