wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

load("heartsteps.RData")

## print any duplicates
checkdup(complete, contextid, "checks/dup_ema_complete.csv")
checkdup(notify, contextid, "checks/dup_ema_notified.csv")
checkdup(engage, engageid, "checks/dup_ema_engaged.csv")
checkdup(emaresponse, questionid, "checks/dup_ema_response.csv")
intersect(struc$contextid, unstruc$contextid)
checkdup(struc, contextid, "checks/dup_structured.csv")
checkdup(unstruc, contextid, "checks/dup_unstructured.csv")

unstruc$list_of_options <- NA
plan <- rbind(struc, unstruc[names(struc)])
plan <- plan[with(plan, order(userid, utime_finished)), ]

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
