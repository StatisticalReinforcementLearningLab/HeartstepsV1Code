library("rstan") # observe startup messages

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/Volumes/dav/HeartSteps/Walter/daily")

dfdaily = readRDS("dfdaily_v2.rds")

id = unique(dfdaily$user)
temp = sqrt(dfdaily$jbsteps.direct[is.element(dfdaily$user, id)])
y = temp
y_obs = temp[!is.na(temp)];
N = length(y);
N_obs = length(y_obs)
N_mis = N - N_obs;
ids = dfdaily$user[is.element(dfdaily$user, id)]
ii_obs <- which(!is.na(temp), arr.ind = TRUE)
ii_miss <- which(is.na(temp), arr.ind = TRUE)

A.1 = as.numeric(dfdaily$planning.today[is.element(dfdaily$user, id)] == "structured") - 1/4
A.2 = as.numeric(dfdaily$planning.today[is.element(dfdaily$user, id)] == "unstructured") - 1/4

A.1[is.na(A.1)] = 0.0; A.2[is.na(A.2)] = 0.0; 

x = cbind(as.numeric(dfdaily$weekendTrue[is.element(dfdaily$user, id)]),
          as.numeric(dfdaily$daily.precip_mean[is.element(dfdaily$user, id)])
          )

a = cbind(A.1,A.2)

x[is.nan(x)] = 0


test_dat = list("N"= N, 
                "N_obs" = N_obs,
                "N_mis" = N_mis,
                "ind_pres" = ii_obs,
                "ind_miss" = ii_miss,
                "y_obs" = y_obs,
                "x" = x,
                "xK" = ncol(x),
                "a" = a,
                "aK" = ncol(a),
                "ids" = ids,
                "num_persons" = length(unique(ids))
                )

# STAN model
mod.nomiss <- '
data {
int<lower = 0> N; // Total number of values = N_obs + N_mis
int<lower = 0> N_obs; // Number of non-missing values
int<lower = 0> N_mis; // Number of missing values
real y_obs[N_obs]; // Vector of non-missing values
int ind_pres[N_obs];     // Vector of non-missing value indices
int ind_miss[N_mis];     // Vector of missing value indices
int<lower=0> xK; // number of predictors
int<lower=0> aK; // number of predictors
matrix[N, xK] x; // predictor matrix
matrix[N, aK] a; // predictor matrix
int ids[N];
}
parameters {
real alpha;
real beta;
vector[xK] zeta;
vector[aK] treatzeta;
real y_mis[N_mis];
real<lower=0> sigma;
}
transformed parameters {
  vector[N] y;   // The "data" with interpolated missing values
  // Fill y with non-missing values 
  for(n in 1:N_obs) {
  y[ind_pres[n]] = y_obs[n];
  }
  // Fill the rest of y with missing value "parameters"
  for(n in 1:N_mis){
  y[ind_miss[n]] = y_mis[n];
  }
}
model {
sigma ~ gamma(1, 1);
y[1] ~ normal(alpha + x[1] * zeta, sigma);
for (n in 2:N) {
  if (ids[n] == ids[n-1]) {
    y[n] ~ normal(alpha + beta * y[n-1] + x[n] * zeta + a[n-1] * treatzeta, sigma);
  } else {
    y[n] ~ normal(alpha + x[n] * zeta, sigma);
  }
}

// hyperpriors
sigma ~ cauchy(0, 2);
alpha ~ cauchy(0, 2);
beta ~ cauchy(0, 2);
zeta ~ cauchy(0, 2);
}
'

fit <- stan(model_code = mod.nomiss, data = test_dat, 
            iter = 1000, chains = 4)

summary(fit, pars= c("alpha", "beta", "zeta", "treatzeta"))$summary
