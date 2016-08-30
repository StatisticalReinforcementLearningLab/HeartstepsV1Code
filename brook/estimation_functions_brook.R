## Brook's Estimation functions for Heartsteps


# Calculate the sandwich variance-covariance estimator
# Independent working correlation
# small = TRUE for the Mancl & DeRouen adjustment
vcov.heartsteps.bgl <- function(x, small = TRUE){
  require(Matrix)
  X <- model.matrix(x)
  ids <- x$geese$id
  Xl <- split(as.data.frame(X), ids)
  residl <- split(x$y - x$fitted.values, ids)
  meatmat <- matrix(0, nrow=ncol(X), ncol=ncol(X))
  W <- diag(x$geese$weights) # matrix of all weights
  Wl <- split(x$geese$weights, ids) # list of vectors
  XWXi <- solve(crossprod(X, W) %*% X) # BREAD
  for (i in 1:length(Xl)){
    Xj <- as.matrix(Xl[[i]]) # Design matrix for this user
    ej <- residl[[i]] # Residuals for this user
    Wj <- diag(Wl[[i]]) # Weights for this user
    Inj <- diag(ncol(Wj)) # Identity of size n_j
    adj_mat <- Inj 
    XtWj <- crossprod(Xj, Wj) # t(X_j) * W_j
    if (small){
      Hjj <- Xj %*% XWXi %*% XtWj # Hat matrix for jth user
      adj_mat <- solve(Inj - Hjj) # Adjustment matrix
    }
    Mj <- XtWj %*% adj_mat %*% ej # p by 1 vector
    meatmat <- meatmat + tcrossprod(Mj)
  }
  return(XWXi %*% meatmat %*% XWXi) # sandwich estimator
}


test.coef.small <- function(beta, S, ix, n, alpha=0.05){
  pprime <- length(ix)
  q <- length(beta) - pprime
  numer <- pprime
  denom <- n - q - pprime
  tsq <- beta[ix] %*% solve(S[ix, ix]) %*% beta[ix]
  pval <- pf(tsq, df1 = numer, df2 = denom, lower.tail=F)
  lefttail <- (n - q - pprime) * (1-alpha) / (pprime * (n-q-1))
  critval <- qf(lefttail, df1 = pprime, df2=n-q-pprime, lower.tail=T)
  ci <- NULL
  if (length(ix) == 1){
    lwr <- beta[ix] - sqrt(S[ix,ix]) * sqrt(critval)
    upr <- beta[ix] + sqrt(S[ix,ix]) * sqrt(critval)
    ci <- setNames(c(lwr,upr),c('lwr','upr'))
  }
  return(list(tsquare = tsq,
              pval = pval,
              fcrit = critval,
              ci=ci))
}

pointwise.table.small <- function(beta, S, n, alpha=0.05){
  testeach <- lapply(1:length(beta),
                     function(i) test.coef.small(beta, S, ix=i, n=n))
  ret <- t(
    rbind('Hotelling'=sapply(1:length(beta),
                             function(i) return(getElement(testeach[[i]],
                                                           'tsquare'))),
          'p-value' = sapply(1:length(beta),
                             function(i) return(getElement(testeach[[i]],
                                                           'pval'))),
          sapply(1:length(beta),
                 function(i) return(getElement(testeach[[i]], 'ci'))))
  )
  return(ret)
}