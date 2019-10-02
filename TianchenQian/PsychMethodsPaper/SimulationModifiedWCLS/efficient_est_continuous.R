# For continuous outcome, paper with Mike Russell
# Comparing the efficiency between WCLS and WCLS with \tilde{p}S_t added to the control variables

# Tianchen Qian, 2019.07.17

# This is the efficient estimating equation for continuous outcome,
# which is very similar to WCLS with \tilde{p}S_t added to the control variables.
# More precisely, this is equivalent to WCLS with A_t not centering in the residual part (only centered in the derivative part)



library(rootSolve) # for solver function multiroot()
library(geepack) # for fitting GEE using package

get_alpha_beta_from_multiroot_result <- function(root, p, q)
{
    if (p == 1) {
        beta_root <- root$root[q+1]
    } else {
        beta_root <- as.matrix(root$root[(q+1) : (q+p)])
    }
    if (q == 1) {
        alpha_root <- root$root[1]
    } else {
        alpha_root <- as.matrix(root$root[1:q])
    }
    return(list(alpha = alpha_root, beta = beta_root))
}

find_change_location <- function(v){
    n <- length(v)
    if (n <= 1) {
        stop("The vector need to have length > 1.")
    }
    return(c(1, 1 + which(v[1:(n-1)] != v[2:n])))
}
# examples
# v <- c("a", "a", "b", "c", "c"); find_change_location(v)
# [1] 1 3 4


efficient_est_continuous <- function(
    Y, # outcome
    A, # treatment indicator
    Zdm, # control variables
    Xdm, # moderators
    p_t, # randomization probability
    p_t_tilde = NULL, # \tilde{p}_t
    id,
    decision_time,
    avail = NULL,
    estimator_initial_value = NULL # in the order of c(alpha, beta)
)
{
    ### 1. preparation ###
    sample_size <- length(unique(id))
    total_person_decisionpoint <- length(Y)
    
    if (is.null(avail)) {
        avail <- rep(1, total_person_decisionpoint)
    }
    
    if (is.null(p_t_tilde)) {
        p_t_tilde <- p_t
    }
    
    cA <- A - p_t_tilde # centered A
    
    p <- ncol(Xdm) # dimension of beta
    q <- ncol(Zdm) # dimension of alpha
    
    ### 2. estimation via estimating equation ###
    
    estimating_equation <- function(theta) {
        alpha <- as.matrix(theta[1:q])
        beta <- as.matrix(theta[(q+1):(q+p)])
        
        residual <- Y - Zdm %*% alpha - A * (Xdm %*% beta)
        weight <- ifelse(A, p_t_tilde / p_t, (1 - p_t_tilde) / (1 - p_t))
        
        ef <- rep(NA, length(theta)) # value of estimating function
        for (i in 1:q) {
            ef[i] <- sum( weight * residual * avail * Zdm[, i])
        }
        for (i in 1:p) {
            ef[q + i] <- sum( weight * residual * avail * cA * Xdm[, i])
        }
        
        ef <- ef / sample_size
        return(ef)
    }
    
    if (is.null(estimator_initial_value)) {
        estimator_initial_value <- rep(0, length = p + q)
    }
    
    solution <- tryCatch(
        {
            multiroot(estimating_equation, estimator_initial_value)
        },
        error = function(cond) {
            message("\nCatched error in multiroot inside efficient_ee():")
            message(cond)
            return(list(root = rep(NaN, p + q), msg = cond,
                        f.root = rep(NaN, p + q)))
        })
    
    estimator <- get_alpha_beta_from_multiroot_result(solution, p, q)
    alpha_hat <- as.vector(estimator$alpha)
    beta_hat <- as.vector(estimator$beta)
    
    
    ### 3. return the result with variable names ###
    
    names(alpha_hat) <- colnames(Zdm)
    names(beta_hat) <- colnames(Xdm)
    
    return(list(beta_hat = beta_hat, alpha_hat = alpha_hat,
                dims = list(p = p, q = q),
                f.root = solution$f.root))
}


# Following is the estimator proposed by Susan, which she claims to be efficient.
# The estimator is to divde each term by \frac{1}{\tilde p_t(1-\tilde p_t).
# In this case where treatment effect is conditional on the entire history, this means
# divding each term by \frac{1}{p_t (1 - p_t).
efficient_est_continuous_Susan <- function(
    Y, # outcome
    A, # treatment indicator
    Zdm, # control variables
    Xdm, # moderators
    p_t, # randomization probability
    p_t_tilde = NULL, # \tilde{p}_t
    id,
    decision_time,
    avail = NULL,
    estimator_initial_value = NULL # in the order of c(alpha, beta)
)
{
    ### 1. preparation ###
    
    sample_size <- length(unique(id))
    total_person_decisionpoint <- length(Y)
    
    if (is.null(avail)) {
        avail <- rep(1, total_person_decisionpoint)
    }
    
    if (is.null(p_t_tilde)) {
        p_t_tilde <- p_t
    }
    
    cA <- A - p_t_tilde # centered A
    
    p <- ncol(Xdm) # dimension of beta
    q <- ncol(Zdm) # dimension of alpha
    
    ### 2. estimation via estimating equation ###
    
    estimating_equation <- function(theta) {
        alpha <- as.matrix(theta[1:q])
        beta <- as.matrix(theta[(q+1):(q+p)])
        
        residual <- Y - Zdm %*% alpha - A * (Xdm %*% beta)
        weight <- ifelse(A, p_t_tilde / p_t, (1 - p_t_tilde) / (1 - p_t)) / p_t_tilde * (1 - p_t_tilde)
        
        ef <- rep(NA, length(theta)) # value of estimating function
        for (i in 1:q) {
            ef[i] <- sum( weight * residual * avail * Zdm[, i])
        }
        for (i in 1:p) {
            ef[q + i] <- sum( weight * residual * avail * cA * Xdm[, i])
        }
        
        ef <- ef / sample_size
        return(ef)
    }
    
    if (is.null(estimator_initial_value)) {
        estimator_initial_value <- rep(0, length = p + q)
    }
    
    # browser()
    solution <- tryCatch(
        {
            multiroot(estimating_equation, estimator_initial_value)
        },
        error = function(cond) {
            message("\nCatched error in multiroot inside efficient_ee():")
            message(cond)
            return(list(root = rep(NaN, p + q), msg = cond,
                        f.root = rep(NaN, p + q)))
        })
    
    estimator <- get_alpha_beta_from_multiroot_result(solution, p, q)
    alpha_hat <- as.vector(estimator$alpha)
    beta_hat <- as.vector(estimator$beta)
    
    
    ### 3. return the result with variable names ###
    
    names(alpha_hat) <- colnames(Zdm)
    names(beta_hat) <- colnames(Xdm)
    
    return(list(beta_hat = beta_hat, alpha_hat = alpha_hat,
                dims = list(p = p, q = q),
                f.root = solution$f.root))
}