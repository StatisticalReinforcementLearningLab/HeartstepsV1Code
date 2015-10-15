wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

load("heartsteps.RData")

## check for duplicates
checkdup(decision, suggestid, "checks/dup_decision.csv")
checkdup(response, decisionid, "checks/dup_response.csv")

## count number of decisions per day

## step counts
## - merge unique (potential) notification times with jawbone table,
##   so user-specific notification counts => treatment occasion index
## - for corresponding occasion, calculate duration between (potential)
##   notification time and activity end time => aggregate of counts meeting
##   a specified duration threshold gives the proximal step count
suggest <- merge(subset(decision, select = c(userid, utime_stamp, is_prefetch)),
                 jawbone, by.x = c("userid", "utime_stamp"),
                 by.y = c("userid", "end_utime"), all = TRUE)
    
