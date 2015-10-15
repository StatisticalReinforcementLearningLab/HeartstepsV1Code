source("readcsv.R", echo = TRUE)

## print any duplicates
subset(complete, contextid %in% contextid[duplicated(contextid)],
       select = c(user, context, ltime_stamp, completed))

subset(notify, contextid %in% contextid[duplicated(contextid)],
       select = c(user, context, notified_ltime))

intersect(struc$contextid, unstruc$contextid)
unstruc$list_of_options <- NA
plan <- rbind(struc, unstruc[names(struc)])
plan <- plan[with(plan, order(userid, utime_finished)), ]
subset(plan, contextid %in% contextid[duplicated(contextid)],
       select = c(user, context, ltime_started, ltime_finished, response))

engage$plan_ltime <- copy(engage, plan, "finish_ltime", "contextid")
engage$engage_ltime <- char2ltime(engage$engaged_time, engage$tz)
engage$plan_prox <- with(engage, as.numeric(difftime(plan_ltime, engage_ltime,
                                                     units = "secs")))
subset(engage, contextid %in% contextid[duplicated(contextid)],
       select = c(contextid, engaged_time, valid, plan_prox))
engage$valid <- engage$valid == "true"
engage <- engage[with(engage, order(contextid, -valid, abs(plan_prox))), ]

## drop redundant responses
table(emaresponse$drop <- duplicated(subset(emaresponse,
                                            select = c(contextid, question))))
table(duplicated(subset(emaresponse,
                        select = c(contextid, question, response))))
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

ema$submit_ltime <- copy(ema, complete, "submit_ltime", "contextid")
ema$notify_ltime <- copy(ema, notify, "notify_ltime", "contextid")
ema$engage_ltime <- copy(ema, engage, "engage_ltime", "contextid")
ema$plan_prox <- copy(ema, engage, "plan_prox", "contextid")
ema$plan_structured <- copy(ema, plan, "structured", "contextid")

subset(ema, is.na(notify_ltime) & is.na(engage_ltime) & !is.na(plan_structured))

ema$userid <- unlist(lapply(strsplit(ema$contextid, "_"), function(x) x[1]))
ema$date <- with(ema, as.Date(pmin(submit_ltime, notify_ltime, engage_ltime,
                                   na.rm = TRUE)))

sapply(unique(ema$userid),
       function(x) with(subset(ema, userid == x), table(date)))
