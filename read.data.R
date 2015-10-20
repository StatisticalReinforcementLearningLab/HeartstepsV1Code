## read, check and revise data files

source("init.R")

wd <- getwd()
setwd(mbox)

## -- read CSV files into data frames

read.data <- function(file, order.by = NULL, ...) {
  d <- sapply(file, read.csv, header = TRUE, strip.white = TRUE, ...,
              simplify = FALSE)
  if (length(d) > 1) {
    l <- d
    n <- unique(unlist(lapply(d, names)))
    d <- lapply(l, function(x) data.frame(matrix(NA, nrow(x), length(n),
                                                 dimnames = list(NULL, n))))
    sapply(1:length(l), function(i) d[[i]][, match(names(l[[i]]), n)] <<- l[[i]])
  }
  d <- do.call("rbind", d)
  names(d) <- tolower(names(d))
  names(d) <- gsub("_", ".", names(d))
  names(d)[names(d) == "user"] <- "userid"
  d <- d[, names(d) != "key", drop = FALSE]
  ## keep only pilot users
  if ("userid" %in% names(d))
    d <- subset(d, grepl("heartsteps.test[0-9]+@", userid, perl = TRUE))
  ## add offset in seconds from GMT/UTC, time zone identifier
  l <- grepl("(^time\\.(fin|sta|up)|\\.(date|)time$)", names(d), perl = TRUE)
  if (any(l)) {
    d$gmtoff <- 0
    d$tz <- if (is.null(d$timezone)) ""
            else d$timezone
    if (!is.null(d$utc.to.local.delta)) {
      d$gmtoff <- 60 * d$utc.to.local.delta
      d$tz <- paste("Etc/GMT", c("-", "+")[pmax(1, sign(d$gmtoff) + 1)],
                    formatC(abs(d$gmtoff) / 60^2), sep = "")
    }
    ## all Unix times, POSIXlt elements
    u <- do.call("data.frame",
                 mapply(char2utime, x = d[, l, drop = FALSE],
                        offset = d[, ncol(d) - 1, drop = FALSE],
                        SIMPLIFY = FALSE))
    names(u) <- gsub("(date|)time", "utime", names(u), perl = TRUE)
    p <- do.call("data.frame",
                 mapply(char2calendar, x = d[, l, drop = FALSE],
                        tz = d[, ncol(d), drop = FALSE], SIMPLIFY = FALSE))
    d <- cbind(d, u, p)
  }
  ## sort by given list of variables
  order.by <- substitute(order.by)
  order.by <- eval(order.by, d)
  if (!is.null(order.by))
    d <- d[do.call("order", order.by), ]
  row.names(d) <- NULL
  d
}

## EMA completion status
complete <- read.data("EMA_Completed.csv", list(contextid, utime.stamp))

## context in which the EMA notification was sent
notify <- read.data("EMA_Context_Notified.csv", list(contextid, notified.utime))

## context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv", list(contextid, engaged.utime))

## EMA responses
## nb: time zone data are unavailable
ema <- read.data("EMA_Response.csv", list(contextid, question, utime.stamp))
ema$message <- strip.white(ema$message)

## planning
plan <- read.data(c("Structured_Planning_Response.csv",
                    "Unstructured_Planning_Response.csv"),
                  list(contextid, utime.finished))

## suggestion messages
messages <- read.data("Reviewed_Heartsteps_Messages.csv", NULL, skip = 1)
messages$message <- strip.white(messages$message)

## suggestions
## nb: prefetch data in a prefetch/non-prefetch dual are irrelvant,
##     but decisionid sometimes persists into next timeslot
decision <- read.data("Momentary_Decision.csv", list(decisionid, is.prefetch))
decision$returned.message <- strip.white(decision$returned.message)
decision$is.prefetch <- decision$is.prefetch == "true"
decision$notify <- decision$notify == "True"
decision$msgid <- with(decision, paste(decisionid, time.slot, sep = "_"))
decision$drop <- duplicated(decision$msgid)

## response to suggestions
## FIXME: merge on message text and proximity in time, check why decisions
##        with notify = FALSE appear in response
response <- read.data("Response.csv", list(decisionid, responded.utime))
response$notification.message <- strip.white(response$notification.message)
response <- merge(response, subset(decision, !drop,
                               select = c(decisionid, msgid, time.slot,
                                          is.prefetch, notify,
                                          returned.message)),
              by.x = c("decisionid", "notification.message"),
              by.y = c("decisionid", "returned.message"), all.x = TRUE)

## physical activity
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data("jawbone_step_count_data.csv", list(userid, end.utime))

## application usage
usage <- read.data("Heartsteps_Usage_History.csv", list(userid, end.utime))

## snooze enabled or disabled
snooze <- read.data("Snoozed_FromInApp.csv")

## home and work locations
## nb: time zone data are unavailable
address <- read.data("User_Addresses.csv", list(userid, time.updated))

## calendars
## nb: time zone data are unavailable
calendar <- read.data("User_Calendars.csv", list(userid, time.updated))

## suggestion and EMA timeslots
timeslot <- read.data("User_Decision_Times.csv", list(userid, utime.updated))

## daily weather by city
weather <- read.data("Weather_History.csv", list(date))
weather$date <- char2date(weather$date, "%Y:%m:%d")

## -- check overlap and duplicates

## keep unique or first-recorded completions
check.dup(complete, "checks/dup_ema_complete.csv", contextid)
complete$keep <- !duplicated(complete$contextid)

## keep unique or first-recorded notifications
check.dup(notify, "checks/dup_ema_notified.csv", contextid)
notify$keep <- !duplicated(notify$contextid)

## keep unique or classified activity engagements
dup.engage <- check.dup(engage, "checks/dup_ema_engaged.csv", contextid,
                        engaged.utime)
engage <- engage[with(engage, order(contextid, engaged.utime,
                                    recognized.activity %in% c(NA, "N/A"))), ]
engage$keep <- !duplicated(subset(engage, select = c(contextid, engaged.utime)))

## duplicates due to answer revisions
## keep unique or latest same-day answers to EMA questions
dup.ema <- check.dup(ema, "checks/dup_ema_response.csv", contextid, question)
ema <- ema[with(ema, order(contextid, question,
                           message.time.mday != time.stamp.mday)), ]
ema$keep <- !duplicated(subset(ema, select = c(contextid, question)))

## duplicates due to answer revisions
## keep unique or latest same-day plans
dup.plan <- check.dup(plan, "checks/dup_planning.csv", contextid)
plan <- plan[with(plan, order(contextid,
                              time.started.mday != time.finished.mday)), ]
plan$keep <- !duplicated(plan$contextid)

check.dup(subset(decision, !drop), "checks/dup_decision.csv",
          decisionid, time.slot)

check.dup(response, "checks/dup_response.csv", decisionid, time.slot)

check.dup(jawbone, "checks/dup_jawbone.csv", userid, end.utime)

## FIXME: add more checks

## -- sample information

length(user.ids <- get.ids("userid", complete, notify, engage, ema, plan,
                           decision, response))

length(context.ids <- get.ids("contextid", complete, notify, engage, ema, plan))

length(decision.ids <- get.ids("msgid", decision, response))

save.image("heartsteps.RData", safe = FALSE)

setwd(wd)
