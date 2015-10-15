source("datafun.R")

wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))
options(stringsAsFactors = FALSE)

## read and revise data files
read.data <- function(file, id = userid, time = utime_stamp) {
  d <- read.csv(file, header = TRUE)
  ## down-case all variable names
  names(d) <- tolower(names(d))
  ## make identifier variable names consistent
  names(d)[names(d) == "user"] <- "userid"
  ## short identifiers for printing
  if ("userid" %in% names(d))
    d$user <- gsub("(heartsteps\\.test|@gmail.*$)", "", d$userid, perl = TRUE)
  if ("contextid" %in% names(d))
    d$context <- gsub("^.*_", "", d$contextid, perl = TRUE)
  if ("decisionid" %in% names(d))
    d$decision <- gsub("^.*_", "", d$decisionid, perl = TRUE)
  ## keep only pilot users
  if ("userid" %in% names(d))
    d <- subset(d, grepl("heartsteps.test[0-9]+@", userid, perl = TRUE))
  ## drop extraneous variables
  if ("key" %in% names(d))
    d <- subset(d, select = -key)
  iz <- names(d) == "timezone"
  if (!is.null(d$utc_to_local_delta)) {
    ## create timezone identifier that accounts for DST
    ## nb: timezone identifiers are platform-dependent
    d$tz <- paste("Etc/GMT",
                  c("-", "+")[pmax(1, sign(d$utc_to_local_delta) + 1)],
                  formatC(abs(d$utc_to_local_delta) / 60), sep = "")
    d$tz[is.na(d$utc_to_local_delta)] <- NA
    d$tz_valid <- d$tz %in% OlsonNames()
    if (any(!d$tz_valid)) {
      with(d, print(table(timezone, tz, useNA = "ifany")))
      cat("Users with invalid timezone data:\n")
      with(d, print(sort(unique(userid[!tz_valid]))))
    }
    iz <- names(d) == "tz"
  }
  it <- grepl("(^time_(fin|sta|up)|_(date|)time$)", names(d), perl = TRUE)
  if (any(it) & any(iz)) {
    ## POSIXlt times
    l <- do.call("data.frame", mapply(char2ltime, x = d[, it, drop = FALSE],
                                      tz = d[, iz, drop = FALSE]))
    ## Unix times
    u <- data.frame(apply(l, 2, ltime2utime))
    names(l) <- gsub("(date|)time", "ltime", names(l), perl = TRUE)
    names(u) <- gsub("(date|)time", "utime", names(u), perl = TRUE)
    d <- cbind(d, l, u)
  }
  ## sort by provided identifier and time variables
  time <- substitute(time)
  time <- eval(time, d)
  if (!is.null(time))
    d <- d[order(time), ]
  id <- substitute(id)
  id <- eval(id, d)
  if (!is.null(id))
    d <- d[order(id), ]
  d
}

## EMAs
complete <- read.data("EMA_Completed.csv", contextid)
engage <- read.data("EMA_Context_Engaged.csv", contextid, engaged_utime)
notify <- read.data("EMA_Context_Notified.csv", contextid, notified_utime)
## FIXME: all timezone data are missing
emaresponse <- read.data("EMA_Response.csv")

## planning
struc <- read.data("Structured_Planning_Response.csv", contextid, utime_started)
unstruc <- read.data("Unstructured_Planning_Response.csv", contextid,
                     utime_started)

## suggestions
decision <- read.data("Momentary_Decision.csv", decisionid)
response <- read.data("Response.csv", decisionid, responded_utime)

## physical activity
jawbone <- read.data("jawbone_step_count_data.csv", time = end_utime)

## application usage
usage <- read.data("Heartsteps_Usage_History.csv", time = end_utime)

## snooze enabled or disabled
snooze <- read.data("Snoozed_FromInApp.csv")

## home and work locations
## FIXME: all timezone data are missing
address <- read.data("User_Addresses.csv", time = time_updated)

## calendars
## FIXME: all timezone data are missing
calendar <- read.data("User_Calendars.csv", time = time_updated)

## suggestion and EMA timeslots
timeslots <- read.data("User_Decision_Times.csv", time = utime_updated)

## daily weather by city
weather <- read.data("Weather_History.csv", id = NULL, time = date)
weather$date <- char2date(weather$date, "%Y:%m:%d")

## all unique identifier values
get.ids <- function(idname, ...)
  sort(unique(unlist(lapply(list(...), function(x) x[[idname]]))))

length(user.ids <- get.ids("userid", complete, notify, engage, emaresponse,
                           struc, unstruc, decision, response))

length(context.ids <- get.ids("contextid", complete, notify, engage,
                              emaresponse, struc, unstruc))

length(decision.ids <- get.ids("decisionid", decision, response))

emaresponse$message <- omit.space(emaresponse$message)

setwd(wd)
