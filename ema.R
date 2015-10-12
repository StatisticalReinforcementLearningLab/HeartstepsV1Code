source("readcsv.R", echo = TRUE)

## keep last-submitted completion data
subset(complete, contextID %in% contextID[duplicated(contextID)],
       select = c(contextID, time_stamp, completed))
complete$submit_ltime <- char2ltime(complete$time_stamp, complete$tz)
complete <- complete[with(complete,
                          order(contextID, -as.numeric(submit_ltime))), ]
complete <- complete[!duplicated(complete$contextID), ]

subset(notify, contextID %in% contextID[duplicated(contextID)],
       select = c(contextID, notified_time))
notify$notify_ltime <- char2ltime(notify$notified_time, notify$tz)

## keep latest planning data
intersect(struc$contextID, unstruc$contextID)
unstruc$list_of_options <- NA
plan <- rbind(struc, unstruc[names(struc)])
subset(plan, contextID %in% contextID[duplicated(contextID)],
       select = c(contextID, time_started, time_finished, response))
plan$start_ltime <- char2ltime(plan$time_started, plan$tz)
plan$finish_ltime <- char2ltime(plan$time_finished, plan$tz)
plan <- plan[with(plan, order(contextID, -as.numeric(finish_ltime))), ]
plan <- plan[!duplicated(plan$contextID), ]
plan$structured <- !is.na(plan$list_of_options)
summary(plan$duration / 60)

engage$plan_ltime <- copy(engage, plan, "finish_ltime", "contextID")
engage$engage_ltime <- char2ltime(engage$engaged_time, engage$tz)
engage$plan_prox <- with(engage, as.numeric(difftime(plan_ltime, engage_ltime,
                                                     units = "secs")))
subset(engage, contextID %in% contextID[duplicated(contextID)],
       select = c(contextID, engaged_time, valid, plan_prox))
engage$valid <- engage$valid == "true"
engage <- engage[with(engage, order(contextID, -valid, abs(plan_prox))), ]

## drop redundant responses
table(emaresponse$drop <- duplicated(subset(emaresponse,
                                            select = c(contextID, question))))
table(duplicated(subset(emaresponse,
                        select = c(contextID, question, response))))
emaresponse <- emaresponse[!emaresponse$drop, -ncol(emaresponse)]

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
  s <- subset(emaresponse, question == x, select = c(contextID, response))
  ## FIXME: add variables more useful for analysis to s
  d <- data.frame(contextID = context.ids)
  d <- cbind(d, matrix(NA, ncol(s) - 1))
  d[d$contextID %in% s$contextID, -1] <- s[, -1]
  names(d)[-1] <- paste(names(s)[-1], x, sep = "_")
  d
}

ema <- Reduce(function(...) merge(..., by = "contextID", all = TRUE),
              sapply(q, f, simplify = FALSE))

ema$submit_ltime <- copy(ema, complete, "submit_ltime", "contextID")
ema$notify_ltime <- copy(ema, notify, "notify_ltime", "contextID")
ema$engage_ltime <- copy(ema, engage, "engage_ltime", "contextID")
ema$plan_prox <- copy(ema, engage, "plan_prox", "contextID")
ema$plan_structured <- copy(ema, plan, "structured", "contextID")

subset(ema, is.na(notify_ltime) & is.na(engage_ltime) & !is.na(plan_structured))

ema$userID <- unlist(lapply(strsplit(ema$contextID, "_"), function(x) x[1]))
ema$date <- with(ema, as.Date(pmin(submit_ltime, notify_ltime, engage_ltime,
                                   na.rm = TRUE)))

sapply(unique(ema$userID),
       function(x) with(subset(ema, userID == x), table(date)))
