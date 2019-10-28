
# Heartsteps data set
# WCLS analysis using xgeepack.R (Boruvka's code)
# (require to source("xgeepack.R"))

# For the MRT analysis paper to be submitted to Psychology Methods

# Tianchen Qian
# 2019.05.06


# Update on 2019.07.15
# - add analysis of location being a moderator

# Update on 2019.07.20
# - add analysis of recent dose being a moderator

# Update on 2019.09.23
# - analyze the effect of send (an activity suggestion) vs no suggestion
#   (previously was analyzing the effect of send.active (a walking suggestion) vs {anti-sendentary or no suggestion})

# Update on 2019.09.26
# - added Model 5.2, where location is formualted as 3 level (i.e., 2 indicators): home, work, or other places.

# Update on 2019.10.10
# - added Model 7, analyzing the moderation effect of send (activity suggestion) by previous day's planning

# Update on 2019.10.27
# - edited "estimate" function from xgeepack.R to output degrees-of-freedom.



# data loading and preprocessing is copied from Tianchen's code for LMM paper:
# linear_mixed_model-continuous_outcome.R

rm(list = ls())

# library(plyr)
# library(dplyr)
library(tidyverse)
library(xtable)

DIAGNOSTIC_CODE <- FALSE # run diagnostic code (plots/view data frames) throughout the code?


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


##### create the "recent responsivity" variable

# For this variable, we need to work on the original suggest data.frame

# plot the study.day and study.day.nogap variables
if (DIAGNOSTIC_CODE) {
    plot(suggest$study.day, cex = 0.1)
    plot(suggest$study.day.nogap, cex = 0.1)
}


if (DIAGNOSTIC_CODE) {
    # length of travel days (in terms of decision points)
    r <- rle(suggest$travel)
    r$lengths[r$values ==1]
    # 100  40  30  25  55  15 125
    # So it is fine to define recent_responsivity = NA in case (c) in the following function
}

add_recent_responsivity <- function(dta, window_len = 15, treatment = "send") {
    # if any of the following occurs, then recent_responsivity is NA:
    # (a) within the first window_len decision points since start of the study
    # (b) within the first window_len decision points since end of travel day
    # (c) does not have both at least 1 treatment and at least 1 no treatment for all the available decision points over the past window_len decision points 
    
    dta$jbsteps30.log.prepostdiff <- dta$jbsteps30.log - dta$jbsteps30pre.log
    
    dta$recent_resp <- rep(NA, nrow(dta))
    
    dta$recent_resp_NAreason <- rep(NA, nrow(dta))
    
    for (i_dp in 1:nrow(dta)) { # index for decision point (note that i_dp starts from 1, not from 0)
        if (i_dp <= window_len) { # NA case (a)
            dta$recent_resp[i_dp] <- NA
            dta$recent_resp_NAreason[i_dp] <- "a"
            next
        }
        
        dp_window_index <- (i_dp - window_len) : (i_dp - 1)
        if (any(dta$travel[dp_window_index])) { # NA case (b)
            dta$recent_resp[i_dp] <- NA
            dta$recent_resp_NAreason[i_dp] <- "b"
            next
        }
        
        treatment_index <- which(dta$avail[dp_window_index] & (dta[dp_window_index, treatment] == 1) )
        notreatment_index <- which(dta$avail[dp_window_index] & (dta[dp_window_index, treatment] == 0) )
        
        if (length(treatment_index) == 0 | length(notreatment_index) == 0) { # NA case (c)
            dta$recent_resp[i_dp] <- NA
            dta$recent_resp_NAreason[i_dp] <- "c"
            next
        }
        
        dta$recent_resp[i_dp] <- 
            mean(dta$jbsteps30.log.prepostdiff[dp_window_index[treatment_index]]) - 
            mean(dta$jbsteps30.log.prepostdiff[dp_window_index[notreatment_index]])
        
    }
    return(dta)
}

if (DIAGNOSTIC_CODE) {
    # test run on user 1's data
    
    user1 <- filter(suggest, user == 1)
    user1 <- add_recent_responsivity(user1)
    
    View(user1[, c("jbsteps30.log", "jbsteps30pre.log", "travel", "recent_resp", "avail", "send.active", "send.sedentary", "send")])
}

suggest <- suggest %>%
    split(.$user) %>%
    purrr::map_dfr(add_recent_responsivity)

suggest$recent_resp_binary <- (suggest$recent_resp > 0)

if (DIAGNOSTIC_CODE) {
    
    all(is.na(suggest$recent_resp_binary) == is.na(suggest$recent_resp))
    
    plot(suggest$recent_resp)
    
    # see how much of the NA recent_resp is due to travel
    plot(is.na(suggest$recent_resp), col = rgb(0,0,0, 0.1), cex = 0.2)
    points(0.5 * suggest$travel, col = rgb(0,1,0, 0.1), cex = 0.2) # green: travel dates
    points(0.9 * (suggest$recent_resp_NAreason == "a"), col = rgb(1, 0, 0, 0.1), cex = 0.3) # red: NA reason (a)
    points(0.8 * (suggest$recent_resp_NAreason == "b"), col = rgb(0, 1, 0, 0.1), cex = 0.3) # green: NA reason (b)
    points(0.7 * (suggest$recent_resp_NAreason == "c"), col = rgb(0, 0, 1, 0.1), cex = 0.3) # blue: NA reason (c)
}



##### create the "recent dose" variable

# For this variable, we need to work on the original suggest data.frame

# recent_dose = 1 if send = TRUE at previous decision point 
add_recent_dose <- function(dta, treatment = "send") {
    
    dta$recent_dose <- NA
    dta$recent_dose[1] <- 0
    dta$recent_dose[2:nrow(dta)] <- dta[1:(nrow(dta) - 1), treatment]
    if (any(is.na(dta$recent_dose))) {
        warning("Missing ", sum(is.na(dta$recent_dose)), " recent_dose, filled with 0.")
        dta$recent_dose[which(is.na(dta$recent_dose))] <- 0
    }
    
    return(dta)
}

suggest <- suggest %>%
    split(.$user) %>%
    purrr::map_dfr(add_recent_dose)

# recent_dose2 = 1 if send = TRUE at both of the previous two decision points
add_recent_dose2 <- function(dta, treatment = "send") {
    
    treatment_lag1 <- c(0, dta[1:(nrow(dta) - 1), treatment])
    treatment_lag2 <- c(0, 0, dta[1:(nrow(dta) - 2), treatment])
    dta$recent_dose2 <- as.numeric(treatment_lag1 + treatment_lag2 == 2)
    if (any(is.na(dta$recent_dose2))) {
        warning("Missing ", sum(is.na(dta$recent_dose2)), " recent_dose2, filled with 0.")
        dta$recent_dose2[which(is.na(dta$recent_dose2))] <- 0
    }
    
    return(dta)
}

suggest <- suggest %>%
    split(.$user) %>%
    purrr::map_dfr(add_recent_dose2)

# yesterday_sametime_dose = 1 if send = TRUE at 5 decision points ago (i.e., yesterday_sametime)
add_yesterday_sametime_dose <- function(dta, treatment = "send") {
    
    dta$yesterday_sametime_dose <- NA
    dta$yesterday_sametime_dose[1:5] <- 0
    dta$yesterday_sametime_dose[6:nrow(dta)] <- dta[1:(nrow(dta) - 5), treatment]
    if (any(is.na(dta$yesterday_sametime_dose))) {
        warning("Missing ", sum(is.na(dta$yesterday_sametime_dose)), " yesterday_sametime_dose, filled with 0.")
        dta$yesterday_sametime_dose[which(is.na(dta$yesterday_sametime_dose))] <- 0
    }
    
    return(dta)
}

suggest <- suggest %>%
    split(.$user) %>%
    purrr::map_dfr(add_yesterday_sametime_dose)



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



##### Add indicator of planning on the previous day to the suggest data set

# summary(suggest.included$study.date)
# summary(daily$study.date)

for (i in 1:nrow(suggest.included)) {
    if (i %% 100 == 0) {
        cat(i, "")
    }
    row_index <- which((daily$study.date == suggest.included$study.date[i] - 1) & (daily$user == suggest.included$user[i]))
    if (length(row_index) == 1) {
        if (daily$planning[row_index] %in% c("structured", "unstructured")) {
            suggest.included$planning_previousday[i] <- 1
        } else {
            suggest.included$planning_previousday[i] <- 0
        }
    } else if (length(row_index) == 0) { # planning indicator of previous day not found, likely because it is the first day of a user
        suggest.included$planning_previousday[i] <- 0
    } else {
        stop("Multiple instances found in daily.")
    }
}



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

# Fit WCLS ----------------------------------------------------------------


library(geepack)
source("xgeepack.R")

# The following "estimate" function is copied from xgeepack.R,
# and I added the degrees of freedom in the output table.
estimate <- function(x, combos = NULL, omnibus = FALSE, null = 0,
                     small = TRUE, conf.int = 0.95, normal = FALSE, ...) {
    if (is.null(combos)) {
        combos <- diag(length(coef(x)))
        rownames(combos) <- names(coef(x))
        omnibus <- FALSE
    }
    est <- combos %*% coef(x)
    if (nrow(est) != length(null)) null <- rep(null[1], nrow(est))
    ## apply Mancl and DeRouen's (2001) small sample correction
    if (is.logical(small)) small <- small * 50
    n <- cluster.number(x, overall = FALSE)
    d1 <- if (omnibus) nrow(combos)
    else apply(combos != 0, 1, sum)
    d2 <- n - length(coef(x))
    ## apply Hotelling's T-squared test, following Liao et al. (2016)
    if (n <= small & !normal) {
        type <- "Hotelling"
        adj <- d1 * (d1 + d2 - 1) / d2
        qfun <- function(p) mapply(qf, p = p, df1 = d1, df2 = d2) / adj
        pfun <- function(q) 1 - mapply(pf, q = q * adj, df1 = d1, df2 = d2)
    }
    else {
        type <- "Wald"
        qfun <- if (normal) function(p) qnorm((1 + p) / 2)
        else function(p) mapply(qf, p = p, df1 = d1, df2 = d2)
        pfun <- if (normal) function(q) 1 - mapply(pchisq, q = q, df = d1)
        else function(q) 1 - mapply(pf, q = q, df1 = d1, df2 = d2)
    }
    var.est <- combos %*% vcov(x, small = small, ...) %*% t(combos)
    se.est <- sqrt(diag(var.est))
    crit <- sqrt(qfun(conf.int))
    lcl <- est - se.est * crit
    ucl <- est + se.est * crit
    stat <- if (omnibus) rep(t(est - null) %*% solve(var.est) %*% (est - null), d1)
    else (est - null)^2 / diag(var.est)
    pvalue <- pfun(stat)
    out <- cbind(est, lcl, ucl, se.est, stat, d1, d2, pvalue)
    rownames(out) <- rownames(combos)
    colnames(out) <- c("Estimate",
                       paste0(round(conf.int * 100), "% ", c("LCL", "UCL")),
                       "SE", type, "df1", "df2", "p-value")
    class(out) <- c("estimate", "matrix")
    out
}


##### Model 1. marginal effect #####

# Question 1: What is the effect of delivering activity suggestions on individuals’ subsequent 30-minute step counts?

fit_model1 <- geese.glm(x = as.matrix(suggest.included[, c("(Intercept)", "jbsteps30pre.log", "I(send - 0.6)")]),
                 y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                 family = gaussian(), corstr = "independence")
estimate(fit_model1)

output <- estimate(fit_model1)
write.csv(round(output, 3), file = "output.csv")

# > output
#                   Estimate   95% LCL   95% UCL        SE Hotelling       df1 df2 p-value    
# (Intercept)        1.78314   1.53732   2.02896   0.12096 217.31012   1.00000  34  <1e-04 ***
# jbsteps30pre.log   0.41360   0.35116   0.47604   0.03072 181.21118   1.00000  34  <1e-04 ***
# I(send - 0.6)      0.13120  -0.00576   0.26815   0.06739   3.78979   1.00000  34  0.0599 .  

if (DIAGNOSTIC_CODE) {
    # chech the treatment effect towards the end of the trial
    print(suggest.included %>%
              group_by(user) %>%
              summarise(max = max(decision.index.nogap)),
          n = Inf)
    
    last_dp_index <- c()
    
    for (iuser in unique(suggest.included$user)) {
        tmp <- filter(suggest.included, user == iuser)
        max_dp <- max(suggest.included$decision.index.nogap)
        last_dp_index <- c(last_dp_index,
                           which(suggest.included$user == iuser & suggest.included$decision.index.nogap > max_dp - 170))
    }
    
    fit_last20dp <- geese.glm(x = as.matrix(suggest.included[last_dp_index, c("(Intercept)", "jbsteps30pre.log", "I(send.active - 0.3)")]),
                              y = suggest.included$jbsteps30.log[last_dp_index], w = suggest.included$avail[last_dp_index], 
                              id = as.factor(suggest.included$user[last_dp_index]),
                              family = gaussian(), corstr = "independence")
    estimate(fit_last20dp)
}



##### Model 2. effect change over time #####

# Question 2: How does the effect of activity activity suggestions change with each additional day in the study?

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "study.day.nogap" = .$"study.day.nogap",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):study.day.nogap" = .$"I(send - 0.6)" * .$"study.day.nogap")

fit_model2 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model2)

output <- estimate(fit_model2)
write.csv(round(output, 3), file = "output.csv")


beta_index <- 4:5

beta_hat <- coef(fit_model2)[beta_index]
vcov <- fit_model2$geese$vbeta[beta_index, beta_index]

newdta <- df_tx <- data.frame(Intercept = 1, study.day.nogap = 0:41)
df_tx$treatment_effect <- as.matrix(newdta) %*% beta_hat
df_tx$tx_se <- NA
for (i in 1:nrow(df_tx)) {
    f_t <- as.numeric(newdta[i, ]) # feature
    df_tx$tx_se[i] <- sqrt(t(f_t) %*% vcov %*% f_t)
}
df_tx$left_ci <- df_tx$treatment_effect - 1.96 * df_tx$tx_se
df_tx$right_ci <- df_tx$treatment_effect + 1.96 * df_tx$tx_se


p <- ggplot(df_tx) + 
    geom_line(aes(x = study.day.nogap, y = treatment_effect), color = "blue") +
    geom_ribbon(aes(ymin = left_ci, ymax = right_ci, x = study.day.nogap), alpha = 0.3, fill = "blue") +
    geom_hline(yintercept = 0, color = "black") +
    xlab(label = "day in the study") +
    ylab(label = "treatment effect\n(on log step count)") +
    ggtitle(paste0("Effect of activity suggestion over time")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

dir.create("plot", showWarnings = FALSE, recursive = "TRUE")
ggsave(p, filename = paste0("plot/model-2.png"), width = 5, height = 3)

# > output
#                                Estimate   95% LCL   95% UCL        SE Hotelling       df1 df2 p-value    
# (Intercept)                     2.00283   1.76518   2.24048   0.11667 294.69069   1.00000  32 < 1e-04 ***
# jbsteps30pre.log                0.41200   0.35104   0.47295   0.02992 189.56395   1.00000  32 < 1e-04 ***
# study.day.nogap                -0.01058  -0.02013  -0.00103   0.00469   5.09091   1.00000  32 0.03102 *  
# I(send - 0.6)                   0.50744   0.20086   0.81402   0.15051  11.36666   1.00000  32 0.00197 ** 
# I(send - 0.6):study.day.nogap  -0.01848  -0.03090  -0.00607   0.00610   9.19229   1.00000  32 0.00479 ** 


## sensitivity analysis, by estimating a nonparametric curve of treatment effect for each day ##

# > table(suggest.included$study.day.nogap)
# 
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37 
# 102 185 185 185 185 185 185 185 185 185 185 184 184 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 185 181 173 169 
#  38  39  40  41 
# 161 160 156 150 

days <- 0:41
tx <- rep(NA, length(days))
tx_3day <- rep(NA, length(days))
for (iday in 1:length(days)) {
    day <- days[iday]
    # called xymat instead of xmat because both x and y are from this matrix
    xymat <- suggest.included %>%
        filter(study.day.nogap == day) %>%
        transmute("(Intercept)" = .$"(Intercept)",
                  "jbsteps30pre.log" = .$"jbsteps30pre.log",
                  "I(send - 0.6)" = .$"I(send - 0.6)",
                  "jbsteps30.log" = .$"jbsteps30.log",
                  "avail" = .$"avail",
                  "user" = .$"user")
    xymat <- as.data.frame(xymat)
    fit_model_day <- geese.glm(x = as.matrix(xymat[, 1:3]),
                                y = as.numeric(xymat[, 4]), w = as.numeric(xymat[, 5]), id = as.factor(as.vector(xymat[, 6])),
                                family = gaussian(), corstr = "independence")
    tx[iday] <- coef(fit_model_day)[3]
    
    if (!(iday %in% c(1, length(days)))) {
        # calculate treatment effect averaged over consecutive 3 days
        xymat <- suggest.included %>%
            filter(study.day.nogap %in% c(day-1, day, day+1)) %>%
            transmute("(Intercept)" = .$"(Intercept)",
                      "jbsteps30pre.log" = .$"jbsteps30pre.log",
                      "I(send - 0.6)" = .$"I(send - 0.6)",
                      "jbsteps30.log" = .$"jbsteps30.log",
                      "avail" = .$"avail",
                      "user" = .$"user")
        xymat <- as.data.frame(xymat)
        fit_model_day <- geese.glm(x = as.matrix(xymat[, 1:3]),
                                   y = as.numeric(xymat[, 4]), w = as.numeric(xymat[, 5]), id = as.factor(as.vector(xymat[, 6])),
                                   family = gaussian(), corstr = "independence")
        tx_3day[iday] <- coef(fit_model_day)[3]
    }
}
plot(tx, type = "l")
plot(tx_3day, type = "l")


##### Model 3. effect change with recent responsivity #####

# Question 3: Does an individual’s recent responsivity to activity suggestions moderate 
# the effect of the activity activity suggestion, either by enhancing or dampening the effect?

# This is not used in the paper as of 2019.09.23

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "recent_resp_binary" = .$"recent_resp_binary",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):recent_resp_binary" = .$"I(send - 0.6)" * .$"recent_resp_binary")

recent_resp_binary_notNA <- !is.na(suggest.included$recent_resp_binary)

fit_model3 <- geese.glm(x = as.matrix(xmat[recent_resp_binary_notNA, ]),
                        y = suggest.included$jbsteps30.log[recent_resp_binary_notNA],
                        w = suggest.included$avail[recent_resp_binary_notNA],
                        id = as.factor(suggest.included$user[recent_resp_binary_notNA]),
                        family = gaussian(), corstr = "independence")
estimate(fit_model3)

output <- estimate(fit_model3)
write.csv(round(output, 3), file = "output.csv")


# > output
#                                  Estimate 95% LCL 95% UCL      SE Hotelling p-value    
# (Intercept)                        1.6312  1.3129  1.9496  0.1563   108.964  <1e-04 ***
# jbsteps30pre.log                   0.4147  0.3517  0.4776  0.0309   179.989  <1e-04 ***
# recent_resp_binary                 0.2158 -0.0338  0.4654  0.1225     3.101  0.0878 .  
# I(send - 0.6)                     -0.0869 -0.3050  0.1312  0.1071     0.659  0.4229    
# I(send - 0.6):recent_resp_binary   0.3831  0.0577  0.7085  0.1598     5.751  0.0225 *  



##### Model 4. effect change with recent responsivity * day in study #####

# Question 4: It is possible that the recent responsivity moderates the treatment effect, 
# but with different direction towards the beginning and towards the end of the study?

# This is not used in the paper as of 2019.09.23

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "I(study.day.nogap - 3)" = .$"study.day.nogap" - 3,
              "recent_resp_binary" = .$"recent_resp_binary",
              "recent_resp_binary:I(study.day.nogap - 3)" = .$"recent_resp_binary" * (.$"study.day.nogap" - 3),
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):I(study.day.nogap - 3)" = .$"I(send - 0.6)" * (.$"study.day.nogap" - 3),
              "I(send - 0.6):recent_resp_binary" = .$"I(send - 0.6)" * .$"recent_resp_binary",
              "I(send - 0.6):recent_resp_binary:I(study.day.nogap - 3)" = .$"I(send - 0.6)" * .$"recent_resp_binary" * (.$"study.day.nogap" - 3))

recent_resp_binary_notNA <- !is.na(suggest.included$recent_resp_binary)

fit_model4 <- geese.glm(x = as.matrix(xmat[recent_resp_binary_notNA, ]),
                        y = suggest.included$jbsteps30.log[recent_resp_binary_notNA],
                        w = suggest.included$avail[recent_resp_binary_notNA],
                        id = as.factor(suggest.included$user[recent_resp_binary_notNA]),
                        family = gaussian(), corstr = "independence")
estimate(fit_model4)

output <- estimate(fit_model4)
write.csv(round(output, 3), file = "output.csv")

# > output
#                                                         Estimate  95% LCL  95% UCL       SE Hotelling p-value    
# (Intercept)                                              1.72598  1.42790  2.02406  0.14552   140.681  <1e-04 ***
# jbsteps30pre.log                                         0.41428  0.35153  0.47703  0.03063   182.890  <1e-04 ***
# I(study.day.nogap - 3)                                  -0.00473 -0.01948  0.01002  0.00720     0.432  0.5165    
# recent_resp_binary                                       0.28856 -0.04928  0.62640  0.16493     3.061  0.0911 .  
# recent_resp_binary:I(study.day.nogap - 3)               -0.00443 -0.02083  0.01198  0.00801     0.305  0.5849    
# I(send - 0.6)                                            0.16506 -0.36805  0.69818  0.26026     0.402  0.5311    
# I(send - 0.6):I(study.day.nogap - 3)                    -0.01259 -0.03431  0.00913  0.01060     1.409  0.2451    
# I(send - 0.6):recent_resp_binary                         0.59925  0.04069  1.15782  0.27268     4.830  0.0364 *  
# I(send - 0.6):recent_resp_binary:I(study.day.nogap - 3) -0.01332 -0.03796  0.01131  0.01203     1.228  0.2773    


# plot of effect moderation over time

beta_index <- 8:9

beta_hat <- coef(fit_model4)[beta_index]
vcov <- fit_model4$geese$vbeta[beta_index, beta_index]

newdta <- df_tx <- data.frame(Intercept = 1, study.day.nogap = 0:(41 - 3))
df_tx$treatment_effect <- as.matrix(newdta) %*% beta_hat
df_tx$tx_se <- NA
for (i in 1:nrow(df_tx)) {
    f_t <- as.numeric(newdta[i, ]) # feature
    df_tx$tx_se[i] <- sqrt(t(f_t) %*% vcov %*% f_t)
}
df_tx$left_ci <- df_tx$treatment_effect - 1.96 * df_tx$tx_se
df_tx$right_ci <- df_tx$treatment_effect + 1.96 * df_tx$tx_se


p <- ggplot(df_tx) + 
    geom_line(aes(x = study.day.nogap + 3, y = treatment_effect), color = "blue") +
    geom_ribbon(aes(ymin = left_ci, ymax = right_ci, x = study.day.nogap + 3), alpha = 0.3, fill = "blue") +
    geom_hline(yintercept = 0, color = "black") +
    xlab(label = "day in the study") +
    ylab(label = "effect moderation\n(on log step count)") +
    ggtitle(paste0("Effect moderation by recent responsivity over time")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

dir.create("plot", showWarnings = FALSE, recursive = "TRUE")
ggsave(p, filename = paste0("plot/model-4-1.png"), width = 5, height = 3)



# plot of treatment effect over time, separately for recent_resp_binary = 1 and recent_resp_binary = 0


beta_index <- 6:9

beta_hat <- coef(fit_model4)[beta_index]
vcov <- fit_model4$geese$vbeta[beta_index, beta_index]

newdta <- df_tx <- data.frame(Intercept = 1, study.day.nogap = rep(0:(41 - 3), 2),
                              recent_resp_binary = rep(c(0,1), each = 39))
newdta$"recent_resp_binary:study.day.nogap" <- df_tx$"recent_resp_binary:study.day.nogap" <- 
    df_tx$recent_resp_binary * df_tx$study.day.nogap
df_tx$treatment_effect <- as.matrix(newdta) %*% beta_hat
df_tx$tx_se <- NA
for (i in 1:nrow(df_tx)) {
    f_t <- as.numeric(newdta[i, ]) # feature
    df_tx$tx_se[i] <- sqrt(t(f_t) %*% vcov %*% f_t)
}
df_tx$left_ci <- df_tx$treatment_effect - 1.96 * df_tx$tx_se
df_tx$right_ci <- df_tx$treatment_effect + 1.96 * df_tx$tx_se


df_tx$recent_resp_binary_text <- ifelse(df_tx$recent_resp_binary, "recently responsive", "not recently responsive")


p <- ggplot(df_tx) + 
    geom_line(aes(x = study.day.nogap + 3, y = treatment_effect, color = as.factor(recent_resp_binary_text))) +
    geom_ribbon(aes(ymin = left_ci, ymax = right_ci, x = study.day.nogap + 3, fill = as.factor(recent_resp_binary_text)), alpha = 0.3) +
    geom_hline(yintercept = 0, color = "black") +
    xlab(label = "day in the study") +
    ylab(label = "treatment effect\n(on log step count)") +
    ggtitle(paste0("Effect of activity suggestion over time")) +
    theme_bw() +
    theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

dir.create("plot", showWarnings = FALSE, recursive = "TRUE")
ggsave(p, filename = paste0("plot/model-4-2.png"), width = 6, height = 3)




##### Model 5. effect moderated by location (home/work vs. other) #####

# Question 5: How does the effect of walking activity suggestions depend on the location of the individual?

suggest.included$location.other <- !(suggest.included$location.category %in% c("home", "work"))

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "location.other" = .$"location.other",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):location.other" = .$"I(send - 0.6)" * .$"location.other")

fit_model5 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model5)

output <- estimate(fit_model5)
write.csv(round(output, 3), file = "output.csv")

# > output
#                              Estimate 95% LCL 95% UCL      SE Hotelling p-value    
# (Intercept)                    1.8696  1.5756  2.1636  0.1443   167.765  <1e-04 ***
# jbsteps30pre.log               0.4144  0.3517  0.4772  0.0308   180.838  <1e-04 ***
# location.other                -0.1582 -0.3839  0.0674  0.1108     2.040   0.163    
# I(send - 0.6)                  0.2109 -0.0471  0.4689  0.1267     2.772   0.106    
# I(send - 0.6):location.other  -0.1427 -0.4772  0.1919  0.1642     0.754   0.392   


estimate(fit_model5, combos = rbind(c(0, 0, 0, 1, 0), c(0, 0, 0, 1, 1)))

# 1st row: treatment effect with location.other = 0
# 2nd row: treatment effect with location.other = 1
#
#      Estimate 95% LCL 95% UCL      SE Hotelling p-value
# [1,]   0.2109 -0.0471  0.4689  0.1267      2.77   0.106
# [2,]   0.0682 -0.0413  0.1777  0.0867      0.62   0.292



##### Model 5.2 effect moderated by location (home, work, or other) (3 levels) #####

# Question 5: How does the effect of walking activity suggestions depend on the location of the individual?

suggest.included$location.home <- (suggest.included$location.category == "home")
suggest.included$location.work <- (suggest.included$location.category == "work")
suggest.included$location.home[is.na(suggest.included$location.home)] <- 0
suggest.included$location.work[is.na(suggest.included$location.work)] <- 0

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "location.home" = .$"location.home",
              "location.work" = .$"location.work",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):location.home" = .$"I(send - 0.6)" * .$"location.home",
              "I(send - 0.6):location.work" = .$"I(send - 0.6)" * .$"location.work")

fit_model5.2 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model5.2)

output <- estimate(fit_model5.2)
write.csv(round(output, 3), file = "output.csv")

# > output
#
#                             Estimate  95% LCL  95% UCL       SE Hotelling      df1 df2 p-value    
# (Intercept)                   1.7103   1.4571   1.9635   0.1240  190.3298   1.0000  30  <1e-04 ***
# jbsteps30pre.log              0.4148   0.3522   0.4775   0.0307  182.7510   1.0000  30  <1e-04 ***
# location.home                 0.0871  -0.2017   0.3760   0.1414    0.3795   1.0000  30  0.5425    
# location.work                 0.2815   0.0187   0.5442   0.1286    4.7868   1.0000  30  0.0366 *  
# I(send - 0.6)                 0.0683  -0.1087   0.2452   0.0866    0.6206   1.0000  30  0.4370    
# I(send - 0.6):location.home   0.1642  -0.1398   0.4683   0.1489    1.2167   1.0000  30  0.2788    
# I(send - 0.6):location.work   0.0893  -0.5720   0.7507   0.3238    0.0761   1.0000  30  0.7846  


estimate(fit_model5.2, combos = rbind(c(0, 0, 0, 0, 1, 0, 0),
                                    c(0, 0, 0, 0, 1, 1, 0),
                                    c(0, 0, 0, 0, 1, 0, 1)))

# 1st row: treatment effect at other location
# 2nd row: treatment effect at home
# 3rd row: treatment effect at work
#
#      Estimate 95% LCL 95% UCL      SE Hotelling p-value    
# [1,]   0.0683 -0.1087  0.2452  0.0866     0.621 0.43702    
# [2,]   0.2325  0.0906  0.3743  0.1120     4.310 0.00092 ***
# [3,]   0.1576 -0.2261  0.5413  0.3029     0.271 0.57739    





##### Model 6. effect moderated by recent treatment #####

# None of the following three analyses has a significant moderator effect

# Question 6: How does the effect of the activity suggestion depend on whether the individual received a message at a recent time?

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "recent_dose" = .$"recent_dose",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):recent_dose" = .$"I(send - 0.6)" * .$"recent_dose")

fit_model6 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model6)

output <- estimate(fit_model6)
write.csv(round(output, 3), file = "output.csv")

# > output
# 
#                           Estimate 95% LCL 95% UCL      SE Hotelling p-value    
# (Intercept)                 1.8073  1.5507  2.0638  0.1260   205.878  <1e-04 ***
# jbsteps30pre.log            0.4134  0.3508  0.4761  0.0308   180.631  <1e-04 ***
# recent_dose                -0.0475 -0.1607  0.0657  0.0556     0.730   0.399    
# I(send - 0.6)               0.1860 -0.0596  0.4316  0.1206     2.380   0.133    
# I(send - 0.6):recent_dose  -0.1115 -0.4833  0.2604  0.1826     0.373   0.546    


# Question 7: How does the effect of the walking suggestion depend on whether the individual received a message same time yesterday?

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "yesterday_sametime_dose" = .$"yesterday_sametime_dose",
              "I(send.active - 0.3)" = .$"I(send.active - 0.3)",
              "I(send.active - 0.3):yesterday_sametime_dose" = .$"I(send.active - 0.3)" * .$"yesterday_sametime_dose")

fit_model7 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model7)

output <- estimate(fit_model7)
write.csv(round(output, 3), file = "output.csv")


# Question 8: How does the effect of the walking suggestion depend on whether the individual received 2 messages in a row previously?

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "recent_dose2" = .$"recent_dose2",
              "I(send.active - 0.3)" = .$"I(send.active - 0.3)",
              "I(send.active - 0.3):recent_dose2" = .$"I(send.active - 0.3)" * .$"recent_dose2")

fit_model8 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model8)

output <- estimate(fit_model8)
write.csv(round(output, 3), file = "output.csv")



##### Model 7. effect moderated by planning on previous day #####

# None of the following three analyses has a significant moderator effect

# Question 9: How does the effect of the activity suggestion depend on whether the individual received a planning prompt on the previous day?

xmat <- suggest.included %>%
    transmute("(Intercept)" = .$"(Intercept)",
              "jbsteps30pre.log" = .$"jbsteps30pre.log",
              "planning_previousday" = .$"planning_previousday",
              "I(send - 0.6)" = .$"I(send - 0.6)",
              "I(send - 0.6):planning_previousday" = .$"I(send - 0.6)" * .$"planning_previousday")

fit_model9 <- geese.glm(x = as.matrix(xmat),
                        y = suggest.included$jbsteps30.log, w = suggest.included$avail, id = as.factor(suggest.included$user),
                        family = gaussian(), corstr = "independence")
estimate(fit_model9)

output <- estimate(fit_model9)
write.csv(round(output, 3), file = "output.csv")

# > output
#
#                                    Estimate  95% LCL  95% UCL       SE Hotelling      df1 df2 p-value    
# (Intercept)                          1.7642   1.5109   2.0174   0.1243  201.3204   1.0000  32  <1e-04 ***
# jbsteps30pre.log                     0.4137   0.3509   0.4764   0.0308  180.5027   1.0000  32  <1e-04 ***
# planning_previousday                 0.0499  -0.1055   0.2053   0.0763    0.4273   1.0000  32   0.518    
# I(send - 0.6)                        0.1133  -0.0348   0.2614   0.0727    2.4290   1.0000  32   0.129    
# I(send - 0.6):planning_previousday   0.0460  -0.2275   0.3196   0.1343    0.1175   1.0000  32   0.734    
