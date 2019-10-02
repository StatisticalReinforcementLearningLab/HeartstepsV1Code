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
source("dgm_simple.R")
# source("dgm_boruvka.R")
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
    dta <- dgm_simple(sample_size, total_T)
    
    # # estimating \tilde{p}_t
    rho_hat <- mean(dta$A == 1)
    dta$prob_A_tilde <- rho_hat
    
    # another way to estimate \tilde{p}_t
    # fit_ps <- glm(A ~ Z, family = "binomial", data = dta) # propensity score
    # dta$prob_A_tilde <- predict(fit_ps, newdata = dta, type = "response")
    # dta$prob_A_tilde <- pmin(0.8, pmax(0.2, dta$prob_A_tilde))
    # dta$prob_A_tilde <- dta$prob_A
    
    W <- ifelse(dta$A, dta$prob_A_tilde / dta$prob_A, (1-dta$prob_A_tilde) / (1-dta$prob_A) )
    
    # for WCLS
    xmat_1 <- model.matrix( ~ Z + I(A - prob_A_tilde), data = dta)
    # head(xmat_1)
    fit_1 <- geese.glm(x = xmat_1, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W)
    
    # # for WCLS with \tilde{p}S_t added to the control variables
    xmat_2 <- model.matrix( ~ Z + prob_A_tilde + I(A - prob_A_tilde), data = dta)
    # # head(xmat_2)
    # fit_2 <- geese.glm(x = xmat_2, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W)
    fit_2 <- try(geese.glm(x = xmat_2, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W))
    
    # for WCLS with A_t not centering in the residual part (only centered in the derivative part)
    fit_3 <- efficient_est_continuous(
        Y = dta$Y, # outcome
        A = dta$A, # treatment indicator
        Zdm = model.matrix( ~ Z, data = dta), # control variables
        Xdm = model.matrix( ~ 1, data = dta), # moderators
        p_t = dta$prob_A, # randomization probability
        p_t_tilde = dta$prob_A_tilde,
        id = dta$userid,
        decision_time = dta$day,
        avail = NULL,
        estimator_initial_value = fit_1$coefficients
    )
    
    # for WCLS with A_t not centering in the residual part (only centered in the derivative part)
    fit_4 <- efficient_est_continuous_Susan(
        Y = dta$Y, # outcome
        A = dta$A, # treatment indicator
        Zdm = model.matrix( ~ Z, data = dta), # control variables
        Xdm = model.matrix( ~ 1, data = dta), # moderators
        p_t = dta$prob_A, # randomization probability
        p_t_tilde = dta$prob_A_tilde,
        id = dta$userid,
        decision_time = dta$day,
        avail = NULL,
        estimator_initial_value = fit_1$coefficients
    )
    
    xmat_5 <- model.matrix( ~ Z + prob_A + I(A - prob_A_tilde), data = dta)
    # xmat_2 <- model.matrix( ~ S + prob_A_tilde + I(A - prob_A_tilde), data = dta)
    # # head(xmat_2)
    fit_5 <- try(geese.glm(x = xmat_5, y = dta$Y, id = dta$userid, family = gaussian(), corstr = "independence", w = W))
    
    
    coef_1 <- c(fit_1$coefficients["I(A - prob_A_tilde)"])
    if (!class(fit_2) == "try-error") {
        coef_2 <- fit_2$coefficients["I(A - prob_A_tilde)"]
    } else {
        coef_2 <- NA
    }
    coef_3 <- fit_3$beta_hat
    coef_4 <- fit_4$beta_hat
    if (!class(fit_5) == "try-error") {
        coef_5 <- fit_5$coefficients["I(A - prob_A_tilde)"]
    } else {
        coef_5 <- NA
    }
    
    output <- c(coef_1, coef_2, coef_3, coef_4, coef_5)
}
sink()

# saveRDS(result, file = "simulation_result_dgm_simple_n=30,T=210_nsim1000.RDS")
saveRDS(result, file = "simulation_result_dgm_simple_n=100,T=30_marginal_nsim10000.RDS")

colMeans(result)
# 0.5001195                  NA           0.5001195           0.5001195           0.5000973 
apply(result, 2, sd)
# 0.05328225                  NA          0.05328225          0.05328225          0.05323037 