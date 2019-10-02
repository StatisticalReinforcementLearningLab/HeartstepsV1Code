
# Data pre-processing to obtain regression coefficients 
# and the empirical distribution of jbsteps30pre.log

# This is used in constructing a generative model that mimics HeartSteps

# For the MRT analysis paper to be submitted to Psychology Methods

# Tianchen Qian
# 2019.10.01


# data loading and preprocessing is copied from Tianchen's code for LMM paper:
# linear_mixed_model-continuous_outcome.R

rm(list = ls())

# library(plyr)
# library(dplyr)
library(tidyverse)
library(xtable)

DIAGNOSTIC_CODE <- TRUE # run diagnostic code (plots/view data frames) throughout the code?


# Preprocessing -----------------------------------------------------------

##### load data

# loading data from mounted mBox "HeartSteps" folder
sys.var <- switch(Sys.info()["sysname"],
                  "Windows" = list(locale = "English",
                                   mbox = "Z:/HeartSteps/"),
                  "Darwin" = list(locale = "en_US",
                                  mbox = "/Volumes/dav/HeartSteps/"),
                  "Linux" = list(locale = "en_US.UTF-8",
                                 mbox = "~/mbox/HeartSteps/"))
sys.var$mbox.data <- paste0(sys.var$mbox, "Data/")
Sys.setlocale("LC_ALL", sys.var$locale)
Sys.setenv(TZ = "GMT")
load(paste0(sys.var$mbox.data, "analysis.RData"))


# Ordering the list in terms of user and then decision index
suggest <- suggest[order(suggest$user,suggest$decision.index),]

suggest$at.home.or.work <- (suggest$location.category %in% c("home", "work"))


##### Exclude 734 decision points in suggest data.frame

# This code chunk is copied from Brook's "main effects replication.R":
# git_HeartstepsV1Code/brook/maineff_replaction.Rmd

# This code by Brook code removed the following decision points from the analysis:
# (a) travel days (ntravel = 390 decision points)
# (b) study.day.nogap past 41 (npost41 = 340 decision points)
# (c) unavail_sent_slots (user deemed unavailable but a message was sent: 3 decision points)
# (d) no_message_tag (1 decision point)

unavail_sent_slots <-
    filter(suggest, !avail & !is.na(notification.message)) %>%
    select(user, study.day.nogap, slot, avail, notification.message) %>%
    mutate('Message sent' = !is.na(notification.message)) %>% select(-notification.message) %>%
    rename('User' = user, 'Non-travel study day'= study.day.nogap,
           'Decision slot' = slot, 'Available' = avail)
no_message_tag <- 
    filter(suggest, send & !travel & study.day.nogap <= 42 & is.na(send.sedentary)) %>%
    select(user, study.day.nogap, slot, 
           notification.message) %>%
    rename('User' = user,
           'Non-travel study day' = study.day.nogap,
           'Decision slot' = slot,
           'Sent message' = notification.message)

ntravel <- sum(suggest$travel)
npost41 <- with(suggest, sum(study.day.nogap > 41, na.rm=T))
nexclude <- 
    ntravel + npost41 + nrow(unavail_sent_slots) + nrow(no_message_tag)

suggest.included <- 
    suggest %>%
    filter(!travel & study.day.nogap <= 41) %>%
    anti_join(mutate(no_message_tag, no_message_tag = T),
              by=c('user'='User','study.day.nogap'='Non-travel study day',
                   'slot'='Decision slot')) %>%
    anti_join(mutate(unavail_sent_slots, unavail_sent_slots = T),
              by=c('user'='User','study.day.nogap'='Non-travel study day',
                   'slot'='Decision slot'))
suggest.analysis <-
    suggest.included %>%
    arrange(user, study.day.nogap, decision.index.nogap)

navail <- sum(suggest.included$avail)


if (DIAGNOSTIC_CODE) {
    plot(suggest.included$study.day, cex = 0.1)
    plot(suggest.included$study.day.nogap, cex = 0.1)
}


##### Exploratory analysis of missing data 

if (DIAGNOSTIC_CODE) {
    apply(suggest.included[, c("jbsteps30", "jbsteps30.log", "jbsteps30pre", "jbsteps30pre.log",
                               "avail", "connect", "intransit", "recognized.activity",
                               "study.day.nogap", "at.home.or.work")], 2, function(r) sum(is.na(r)))
    # jbsteps30 is missing for 698 time points
    # jbsteps30pre is missing for 688 time points
    
    suggest.included.avail <- filter(suggest.included, avail == 1)
    apply(suggest.included.avail[, c("jbsteps30", "jbsteps30.log", "jbsteps30pre", "jbsteps30pre.log",
                                     "avail", "connect", "intransit", "recognized.activity",
                                     "study.day.nogap", "at.home.or.work")], 2, function(r) sum(is.na(r)))
    # jbsteps30 is missing for 552 time points
    # jbsteps30pre is missing for 539 time points
}



###### Warning
# In the log-transformed version of those variables in suggest, there is no missing data, and all the missing step counts are imputed as 0 steps.
# Is this a correct imputation?
# I need to check this with whoever that created those log-transformed variables to find out their rationale for imputing with zero.


##### create variables for suggest.included

suggest.included <- as_tibble(suggest.included)

if (DIAGNOSTIC_CODE) {
    plot(suggest.included$decision.index, cex = 0.05)
    plot(suggest.included$decision.index.nogap, cex = 0.05)
}

# create lagged 30min step
suggest.included$jbsteps30.log.lag1 <- c(0, suggest.included$jbsteps30.log[1 : (nrow(suggest.included) - 1)])
suggest.included$jbsteps30.log.lag1[suggest.included$decision.index.nogap == 0] <- 0

# create variables to pass into regression
suggest.included$"(Intercept)" <- 1
suggest.included$"I(send.active - 0.3)" <- suggest.included$send.active - 0.3
suggest.included$"I(send.sedentary - 0.3)" <- suggest.included$send.sedentary - 0.3
suggest.included$"I(send - 0.6)" <- suggest.included$send - 0.6

##### check number of 0's in 30-min step count

mean(suggest.included$jbsteps30 == 0, na.rm = TRUE) # 30% are 0's






# Calculate necessary quantities for the generative model ----------------------------------------------

library(geepack)
source("xgeepack.R")

fit_model1 <- geese.glm(x = as.matrix(suggest.included[, c("(Intercept)", "jbsteps30pre.log", "jbsteps30.log.lag1", "I(send - 0.6)")]),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model1)

# Estimate 95% LCL 95% UCL      SE Hotelling p-value    
# (Intercept)          1.6085  1.3015  1.9156  0.1509    113.58 < 1e-04 ***
# jbsteps30pre.log     0.4037  0.3467  0.4606  0.0280    208.15 < 1e-04 ***
# jbsteps30.log.lag1   0.0655  0.0175  0.1136  0.0236      7.70 0.00901 ** 
# I(send - 0.6)        0.1229 -0.0126  0.2584  0.0666      3.41 0.07392 .  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

predicted_y <- as.matrix(suggest.included[, c("(Intercept)", "jbsteps30pre.log", "jbsteps30.log.lag1", "I(send - 0.6)")]) %*% coefficients(fit_model1)

residual <- suggest.included$jbsteps30.log - predicted_y


sd(residual) # 2.716


jbsteps30pre.log.empirical <- suggest.included$jbsteps30pre.log
saveRDS(jbsteps30pre.log.empirical, file = "jbsteps30pre.log.empirical.RDS")