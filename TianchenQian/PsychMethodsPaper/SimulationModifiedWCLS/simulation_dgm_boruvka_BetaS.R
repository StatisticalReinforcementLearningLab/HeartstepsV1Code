# For continuous outcome, paper with Mike Russell
# Comparing the efficiency between WCLS and WCLS with \tilde{p}S_t added to the control variables

# Tianchen Qian, 2019.07.17

# WCLS uses xgeepack.R (Boruvka's code)


rm(list = ls())

# library(plyr)
# library(dplyr)
# library(tidyverse)
# library(xtable)


# Fit WCLS ----------------------------------------------------------------


library(geepack)
library(foreach)
library(doMC)
library(doRNG)

max_cores <- 16
registerDoMC(min(detectCores() - 1, max_cores))


source("xgeepack.R")
source("dgm_boruvka_BetaS.R")
source("efficient_est_continuous.R")

sample_size <- 100
total_T <- 30

nsim <- 10000


# df_varname <- c("beta_0", "beta_1")
# coefs_1 <- coefs_2 <- data.frame(matrix(NA, nrow = nsim, ncol = length(df_varname)))
# names(coefs_1) <- names(coefs_2) <- df_varname

set.seed(20190717)

writeLines(c(""), "log_parallel.txt")

sink("log_parallel.txt", append = FALSE)

result <- foreach(isim = 1:nsim, .combine = "rbind") %dorng% {
    if (isim %% 100 == 0) {
        cat(paste("Starting iteration",isim,"\n"))
    }
    dta <- dgm_boruvka_BetaS(sample_size, total_T)
    
    # dta <- dgm_boruvka_BetaS(1000, 30)
    
    # # estimating \tilde{p}_t
    # rho_hat <- mean(dta$A == 1)
    # dta$prob_A_tilde <- rho_hat
    # # dta$prob_A_tilde <- 0.5
    
    # another way to estimate \tilde{p}_t
    fit_ps <- glm(A ~ S, family = "binomial", data = dta) # propensity score
    dta$prob_A_tilde <- predict(fit_ps, newdata = dta, type = "response")
    dta$prob_A_tilde <- pmin(0.8, pmax(0.2, dta$prob_A_tilde))
    
    W <- ifelse(dta$A, dta$prob_A_tilde / dta$prob_A, (1-dta$prob_A_tilde) / (1-dta$prob_A) )
    
    # for WCLS
    xmat_1 <- model.matrix( ~ S + I(A - prob_A_tilde) * S, data = dta)
    # head(xmat_1)
    fit_1 <- geese.glm(x = xmat_1, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W)
    
    # for WCLS with \tilde{p}_t * S_t added to the control variables
    # This is problematic for this particular generative model, with error:
    #     Error in lm.fit(zsca, qlf(pr2), offset = soffset) : NA/NaN/Inf in 'y'
    # The reason is probably that all variables take binary values.
    xmat_2 <- model.matrix( ~ S * prob_A_tilde + I(A - prob_A_tilde) * S, data = dta)
    # xmat_2 <- model.matrix( ~ S + prob_A_tilde + I(A - prob_A_tilde), data = dta)
    # # head(xmat_2)
    fit_2 <- try(geese.glm(x = xmat_2, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W))
    
    # for WCLS with A_t not centering in the residual part (only centered in the derivative part)
    fit_3 <- efficient_est_continuous(
        Y = dta$Y, # outcome
        A = dta$A, # treatment indicator
        Zdm = model.matrix( ~ S, data = dta), # control variables
        Xdm = model.matrix( ~ S, data = dta), # moderators
        p_t = dta$prob_A, # randomization probability
        p_t_tilde = dta$prob_A_tilde,
        id = dta$userid,
        decision_time = dta$day,
        avail = NULL,
        estimator_initial_value = fit_1$coefficients
        # estimator_initial_value = c(0.03169698, 0.26989075, -0.2, 0.5)
    )
    
    # for WCLS with A_t not centering in the residual part (only centered in the derivative part)
    fit_4 <- efficient_est_continuous_Susan(
        Y = dta$Y, # outcome
        A = dta$A, # treatment indicator
        Zdm = model.matrix( ~ S, data = dta), # control variables
        Xdm = model.matrix( ~ S, data = dta), # moderators
        p_t = dta$prob_A, # randomization probability
        p_t_tilde = dta$prob_A_tilde,
        id = dta$userid,
        decision_time = dta$day,
        avail = NULL,
        estimator_initial_value = fit_1$coefficients
        # estimator_initial_value = c(0.03169698, 0.26989075, -0.2, 0.5)
    )
    
    xmat_5 <- model.matrix( ~ S * prob_A + I(A - prob_A_tilde) * S, data = dta)
    # xmat_2 <- model.matrix( ~ S + prob_A_tilde + I(A - prob_A_tilde), data = dta)
    # # head(xmat_2)
    fit_5 <- try(geese.glm(x = xmat_5, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W))
    
    
    
    coef_1 <- c(fit_1$coefficients["I(A - prob_A_tilde)"], fit_1$coefficients["S:I(A - prob_A_tilde)"])
    if (!class(fit_2) == "try-error") {
      coef_2 <- c(fit_2$coefficients["I(A - prob_A_tilde)"], fit_2$coefficients["S:I(A - prob_A_tilde)"])
    } else {
      coef_2 <- c(NA, NA)
    }
    coef_3 <- fit_3$beta_hat
    coef_4 <- fit_4$beta_hat
    if (!class(fit_5) == "try-error") {
      coef_5 <- c(fit_5$coefficients["I(A - prob_A_tilde)"], fit_5$coefficients["S:I(A - prob_A_tilde)"])
    } else {
      coef_5 <- c(NA, NA)
    }
    
    output <- c(coef_1, coef_2, coef_3, coef_4, coef_5)
}
sink()

# saveRDS(result, file = "simulation_result_n=30,T=210_nsim1000.RDS")
saveRDS(result, file = "simulation_result_dgm_boruvka_BetaS_n=100,T=30_nsim10000_glmptilde.RDS")
# saveRDS(result, file = "simulation_result_dgm_boruvka_BetaS_n=100,T=30_nsim10000_constptilde.RDS")

coefs_1 <- result[, 1:2]
coefs_2 <- result[, 3:4]
coefs_3 <- result[, 5:6]
coefs_4 <- result[, 7:8]
coefs_5 <- result[, 9:10]

rbind(colMeans(coefs_1),
      colMeans(coefs_2),
      colMeans(coefs_3),
      colMeans(coefs_4),
      colMeans(coefs_5))
#      I(A - prob_A_tilde) S:I(A - prob_A_tilde)
# [1,]          -0.2008005             0.5014293
# [2,]          -0.2008211             0.5013725
# [3,]          -0.2007759             0.5013039
# [4,]          -0.2009241             0.5006710
# [5,]          -0.2006217             0.5011024

rbind(c(-0.2, 0.5) - colMeans(coefs_1),
      c(-0.2, 0.5) - colMeans(coefs_2),
      c(-0.2, 0.5) - colMeans(coefs_3),
      c(-0.2, 0.5) - colMeans(coefs_4),
      c(-0.2, 0.5) - colMeans(coefs_5))
#      I(A - prob_A_tilde) S:I(A - prob_A_tilde)
# [1,]        0.0008005022         -0.0014293021
# [2,]        0.0008210641         -0.0013725076
# [3,]        0.0007759355         -0.0013038778
# [4,]        0.0009240722         -0.0006710174
# [5,]        0.0006217013         -0.0011024147

rbind(apply(coefs_1, 2, sd),
      apply(coefs_2, 2, sd),
      apply(coefs_3, 2, sd),
      apply(coefs_4, 2, sd),
      apply(coefs_5, 2, sd))
#      I(A - prob_A_tilde) S:I(A - prob_A_tilde)
# [1,]          0.04047723            0.07320246
# [2,]          0.04046373            0.07317251
# [3,]          0.04048382            0.07313263
# [4,]          0.04121048            0.08754893
# [5,]          0.03977186            0.07237008