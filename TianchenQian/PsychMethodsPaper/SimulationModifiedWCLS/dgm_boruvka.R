
# 2018.07.11, Tianchen Qian
# for continuous outcome

library(plyr) # for adply() function
library(mvtnorm)

expit <- function(x){
    return(exp(x)/(1+exp(x)))
}

# From Section 6 of Boruvka et al. (2018) JASA paper
# This is the generative model described at the beginning of Section 6,
# with parameter value as used in the second and third simulation studies.
dgm_boruvka <- function(sample_size, total_T) {
    theta_1 <- 0.8
    theta_2 <- 0
    beta_0 <- -0.2
    beta_1 <- 0.5
    xi <- 0
    eta_1 <- -0.8
    eta_2 <- 0.8
    
    df_names <- c("userid", "day", "Y", "A", "S", "expect_Y", "prob_A", "prob_S", "expect_S", "epsilon")
    # Note that I'm keeping the columns that are not part of the actual data set.
    # These columns are used in generating the data, and they will also be 
    # helpful if we need to do some debugging.
    
    dta <- array(NA, dim = c(sample_size, total_T, length(df_names)),
                 dimnames = list(NULL, NULL, df_names))
    
    dta[, , "userid"] <- rep(1:sample_size, times = total_T)
    dta[, , "day"] <- rep(1:total_T, each = sample_size)
    
    # correlated error
    # (for binary outcome we wouldn't need this epsilon thing, but I just kept it here for completeness)
    Sigma <- diag(total_T) 
    Sigma <- 0.5 ^ abs(row(Sigma)-col(Sigma))
    dta[, , "epsilon"] <- rmvnorm(sample_size, mean = rep(0, total_T), sigma = Sigma)
    
    # day t = 1
    t <- 1
    dta[, t, "prob_S"] <- expit(xi * 0)
    dta[, t, "expect_S"] <- 2 * dta[, t, "prob_S"] - 1
    dta[, t, "S"] <- rbinom(sample_size, 1, dta[, t, "prob_S"]) * 2 - 1
    dta[, t, "prob_A"] <- expit(eta_1 * 0 + eta_2 * dta[, t, "S"])
    dta[, t, "A"] <- rbinom(sample_size, 1, dta[, t, "prob_A"])
    dta[, t, "expect_Y"] <- theta_1 * (dta[, t, "S"] - dta[, t, "expect_S"]) + theta_2 * 0 +
        (dta[, t, "A"] - dta[, t, "prob_A"]) * (beta_0 + beta_1 * dta[, t, "S"])
    dta[, t, "Y"] <- dta[, t, "expect_Y"] + dta[, t, "epsilon"]
    
    # day t >= 2
    for (t in 2:total_T) {
        dta[, t, "prob_S"] <- expit(xi * dta[, t - 1, "A"])
        dta[, t, "expect_S"] <- 2 * dta[, t, "prob_S"] - 1
        dta[, t, "S"] <- rbinom(sample_size, 1, dta[, t, "prob_S"]) * 2 - 1
        dta[, t, "prob_A"] <- expit(eta_1 * dta[, t - 1, "A"] + eta_2 * dta[, t, "S"])
        dta[, t, "A"] <- rbinom(sample_size, 1, dta[, t, "prob_A"])
        dta[, t, "expect_Y"] <- theta_1 * (dta[, t, "S"] - dta[, t, "expect_S"]) + theta_2 * (dta[, t - 1, "A"] - dta[, t - 1, "prob_A"]) +
            (dta[, t, "A"] - dta[, t, "prob_A"]) * (beta_0 + beta_1 * dta[, t, "S"])
        dta[, t, "Y"] <- dta[, t, "expect_Y"] + dta[, t, "epsilon"]
    }
    
    # collapse the array so that it is now a data frame
    dta <- plyr::adply(dta, c(1,2))
    dta[, c(1,2)] <- NULL
    
    # order the data set by userid and day
    dta <- dta[order(dta$userid, dta$day), ]
    
    return(dta)
}