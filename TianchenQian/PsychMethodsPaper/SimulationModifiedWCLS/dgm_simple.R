# For continuous outcome, paper with Mike Russell
# Comparing the efficiency between WCLS and WCLS with \tilde{p}S_t added to the control variables

# Tianchen Qian, 2019.07.17


# In this code, I constructed examples where:
# Z_t (covariate) is an exogenous AR(1) process
# p_t depends on Z_t through logistic regression
# E(Y_t+1 | H_t, A_t = 0) depends on Z_t in the form of alpha_0 + dta$Z[row_index] * alpha_1
# treatment effect depends on Z_t in the form of beta_0 + dta$Z[row_index] * beta_1

expit <- function(x){
    return(exp(x)/(1+exp(x)))
}

prob_clip <- function(vector, min, max) {
    vector[which(vector < min)] <- min
    vector[which(vector > max)] <- max
    return(vector)
}

inner_prod <- function(x, y) {
    stopifnot(class(x) == "numeric" & class(y) == "numeric")
    stopifnot(length(x) == length(y))
    return(sum(x * y))
}

dgm_simple <- function(sample_size, total_T) {
    # eta determines how prob(A_t) is affected by Z_t
    # eta = 0 means A_t independent of Z_t
    # alpha is the coefficient in the control model
    
    alpha_0 <- -1
    alpha_1 <- 1
    beta_0 <- 0.5
    beta_1 <- 1
    
    eta <- 0.5
    
    df_names <- c("userid", "day", "Y", "meanY_A0", "tx", "A", "Z", "prob_A", "Y_lag1")
    
    dta <- data.frame(matrix(NA, nrow = sample_size * total_T, ncol = length(df_names)))
    names(dta) <- df_names
    
    dta$userid <- rep(1:sample_size, each = total_T)
    dta$day <- rep(1:total_T, times = sample_size)
    
    dta$Z <- as.vector(replicate(sample_size, arima.sim(model = list(ar = 0.5), n = total_T)))
    
    # first time point
    t <- 1
    row_index <- seq(from = t, by = total_T, length = sample_size)
    
    dta$prob_A[row_index] <- prob_clip(expit(eta * dta$Z[row_index]), min = 0.2, max = 0.8)
    dta$A[row_index] <- rbinom(sample_size, 1, dta$prob_A[row_index])
    dta$meanY_A0[row_index] <- alpha_0 + dta$Z[row_index] * alpha_1
    dta$tx[row_index] <- beta_0 + dta$Z[row_index] * beta_1
    dta$Y[row_index] <- rnorm(sample_size, dta$meanY_A0[row_index] + dta$A[row_index] * dta$tx[row_index], 1)
    dta$Y_lag1[row_index] <- 0
    
    for (t in 2:total_T) {
        # row index for the rows corresponding to day t for every subject
        row_index <- seq(from = t, by = total_T, length = sample_size)
        row_index_pre <- seq(from = t-1, by = total_T, length = sample_size)
        
        dta$prob_A[row_index] <- prob_clip(expit(eta * dta$Z[row_index]), min = 0.2, max = 0.8)
        dta$A[row_index] <- rbinom(sample_size, 1, dta$prob_A[row_index])
        dta$meanY_A0[row_index] <- alpha_0 + dta$Z[row_index] * alpha_1 # + dta$Y[row_index_pre] * alpha_2
        dta$tx[row_index] <- beta_0 + dta$Z[row_index] * beta_1 # + dta$Y[row_index_pre] * beta_2
        dta$Y[row_index] <- rnorm(sample_size, dta$meanY_A0[row_index] + dta$A[row_index] * dta$tx[row_index], 1)
        dta$Y_lag1[row_index] <- dta$Y[row_index_pre]
    }
    
    return(dta)
}

## true beta
beta_0_true <- 0.5
beta_1_true <- 1

# try out the range of Y
if (0) {
    set.seed(123)
    dta <- dgm(100, 30)
    summary(dta$meanY_A0)
    summary(dta$tx)
    summary(dta$prob_A)
    hist(dta$meanY_A0)
    hist(dta$tx)
    hist(dta$prob_A, xlim = c(0,1))
}

