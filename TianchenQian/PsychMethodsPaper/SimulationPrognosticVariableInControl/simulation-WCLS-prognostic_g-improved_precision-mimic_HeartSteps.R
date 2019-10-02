# Simulation study to show improved precision of WCLS
# when including more prognostic variables in the control part, g()

# For the MRT analysis paper to be submitted to Psychology Methods

# Tianchen Qian
# 2019.10.01

rm(list = ls())

source("dgm_mimicHeartSteps.R")

library(geepack)
source("xgeepack.R")


sample_size <- 37
total_T <- 210

nsim <- 1000


library(foreach)
library(doMC)
library(doRNG)

max_cores <- 16
registerDoMC(min(detectCores() - 1, max_cores))



set.seed(20191001)

writeLines(c(""), "log_parallel.txt")

sink("log_parallel.txt", append = FALSE)

result <- foreach(isim = 1:nsim, .combine = "rbind") %dorng% {
    if (isim %% 10 == 0) {
        cat(paste("Starting iteration",isim,"\n"))
    }
    dta <- dgm_mimicHeartSteps(sample_size, total_T)
    
    # create variables to pass into regression
    dta$"(Intercept)" <- 1
    dta$"I(A - 0.6)" <- dta$A - 0.6
    
    fit_model1 <- geese.glm(x = as.matrix(dta[, c("(Intercept)", "Y_lag1", "Z", "I(A - 0.6)")]),
                            y = dta$Y, id = as.factor(dta$user),
                            family = gaussian(), corstr = "independence")
    estimates1 <- estimate(fit_model1)
    
    fit_model2 <- geese.glm(x = as.matrix(dta[, c("(Intercept)", "Z", "I(A - 0.6)")]),
                            y = dta$Y, id = as.factor(dta$user),
                            family = gaussian(), corstr = "independence")
    estimates2 <- estimate(fit_model2)
    
    fit_model3 <- geese.glm(x = as.matrix(dta[, c("(Intercept)", "Y_lag1", "I(A - 0.6)")]),
                            y = dta$Y, id = as.factor(dta$user),
                            family = gaussian(), corstr = "independence")
    estimates3 <- estimate(fit_model3)
    
    fit_model4 <- geese.glm(x = as.matrix(dta[, c("(Intercept)", "I(A - 0.6)")]),
                            y = dta$Y, id = as.factor(dta$user),
                            family = gaussian(), corstr = "independence")
    estimates4 <- estimate(fit_model4)
    
    
    output <- c(estimates1["I(A - 0.6)", c("Estimate", "95% LCL", "95% UCL")],
                estimates2["I(A - 0.6)", c("Estimate", "95% LCL", "95% UCL")],
                estimates3["I(A - 0.6)", c("Estimate", "95% LCL", "95% UCL")],
                estimates4["I(A - 0.6)", c("Estimate", "95% LCL", "95% UCL")])
}
sink()

beta_true <- 0.1229

result1 <- result[, 1:3] # "Y_lag1", "Z"
result2 <- result[, 4:6] # "Z"
result3 <- result[, 7:9] # "Y_lag1"
result4 <- result[, 10:12] # none

results <- list(result1, result2, result3, result4)

bias <- sapply(results, function(r) mean(r[, 1] - beta_true))
# [1] -0.0008620106 -0.0006934222 -0.0010403213 -0.0008596285
sd <- sapply(results, function(r) sd(r[, 1]))
# [1] 0.06695082 0.06713738 0.07357646 0.07379363
cp <- sapply(results, function(r) mean(r[, 2] < beta_true & r[, 3] > beta_true))
# [1] 0.967 0.969 0.958 0.957

print(bias)
print(sd)
print(cp)


saveRDS(results, file = "results_n=37,T=210,nsim=1000.RDS")

