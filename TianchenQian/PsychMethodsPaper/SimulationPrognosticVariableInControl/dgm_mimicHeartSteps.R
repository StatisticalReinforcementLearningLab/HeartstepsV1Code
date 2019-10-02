
# A generative model that mimics HeartSteps

# For the MRT analysis paper to be submitted to Psychology Methods

# Tianchen Qian
# 2019.10.01



# read the empirical distribution of jbsteps30pre.log from HeartSteps
jbsteps30pre.log.empirical <- readRDS("jbsteps30pre.log.empirical.RDS")

dgm_mimicHeartSteps <- function(sample_size, total_T) {
    
    alpha_0 <- 1.6085 # intercept
    alpha_1 <- 0.4037 # jbsteps30pre.log (here is the covariate Z)
    alpha_2 <- 0.0655 # jbsteps30.log.lag1
    beta_0 <- 0.1229 # I(send - 0.6)
    
    df_names <- c("userid", "day", "Y", "Z", "A", "Y_lag1", "meanY_A0")
    
    dta <- data.frame(matrix(NA, nrow = sample_size * total_T, ncol = length(df_names)))
    names(dta) <- df_names
    
    dta$userid <- rep(1:sample_size, each = total_T)
    dta$day <- rep(1:total_T, times = sample_size)
    
    dta$Z <- base::sample(jbsteps30pre.log.empirical, nrow(dta), replace = TRUE)
    dta$A <- rbinom(nrow(dta), 1, 0.6)
    
    # first time point
    t <- 1
    row_index <- seq(from = t, by = total_T, length = sample_size)
    dta$meanY_A0[row_index] <- alpha_0 + alpha_1 * dta$Z[row_index] + alpha_2 * 0
    dta$Y[row_index] <- rnorm(sample_size, dta$meanY_A0[row_index] + (dta$A[row_index] - 0.6) * beta_0, 2.716)
    # standard error = 2.716 (empirical sd of the WCLS fit residuals from HeartSteps data)
    dta$Y_lag1[row_index] <- 0
    
    for (t in 2:total_T) {
        # row index for the rows corresponding to day t for every subject
        row_index <- seq(from = t, by = total_T, length = sample_size)
        row_index_pre <- seq(from = t-1, by = total_T, length = sample_size)
        dta$Y_lag1[row_index] <- dta$Y[row_index_pre]
        dta$meanY_A0[row_index] <- alpha_0 + alpha_1 * dta$Z[row_index] + alpha_2 * dta$Y_lag1[row_index]
        dta$Y[row_index] <- rnorm(sample_size, dta$meanY_A0[row_index] + (dta$A[row_index] - 0.6) * beta_0, 3)
    }
    
    return(dta)
}