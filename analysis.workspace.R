wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

load("heartsteps.RData")

## questions
q <- sort(unique(emaresponse$question))
q

## questions with "select any" responses
q.check <- q[sapply(q, function(x)
  with(emaresponse, any(grepl("@", unique(response[question == x])))))]

## questions with the option to select "other"
q.other <- with(emaresponse, unique(question[other != ""]))

## questions referring to a previously-received activity suggestion
q.msg <- with(emaresponse, unique(question[message != "N/A"]))

f <- function(x) {
  s <- subset(emaresponse, question == x, select = c(contextid, response))
  ## FIXME: add variables more useful for analysis to s
  d <- data.frame(contextid = context.ids)
  d <- cbind(d, matrix(NA, ncol(s) - 1))
  d[d$contextid %in% s$contextid, -1] <- s[, -1]
  names(d)[-1] <- paste(names(s)[-1], x, sep = "_")
  d
}

ema <- Reduce(function(...) merge(..., by = "contextid", all = TRUE),
              sapply(q, f, simplify = FALSE))

wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

load("heartsteps.RData")

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
    
