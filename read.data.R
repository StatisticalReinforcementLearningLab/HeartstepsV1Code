source("datafun.R")

wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
#Sys.setenv(TZ = "UTC")

## read and revise data files
read.data <- function(file, id = userid, time = utime_stamp) {
  d <- read.csv(file, header = TRUE)
  ## down-case all variable names
  names(d) <- tolower(names(d))
  ## make identifier variable names consistent
  names(d)[names(d) == "user"] <- "userid"
  ## keep only pilot users
  if ("userid" %in% names(d))
    d <- subset(d, grepl("heartsteps.test[0-9]+@", userid, perl = TRUE))
  ## drop extraneous variables
  if ("key" %in% names(d))
    d <- subset(d, select = -key)
  iz <- names(d) == "timezone"
  if (!is.null(d$utc_to_local_delta)) {
    ## create timezone identifier that accounts for DST
    d$tz <- paste("Etc/GMT",
                  c("+", "-")[pmax(1, sign(d$utc_to_local_delta) + 1)],
                  formatC(abs(d$utc_to_local_delta) / 60), sep = "")
    d$tz[is.na(d$utc_to_local_delta)] <- NA
    iz <- names(d) == "tz"
  }
  it <- grepl("(^time_(fin|sta|up)|_(date|)time$)", names(d), perl = TRUE)
  if (any(it) & any(iz)) {
    ## Unix times
    u <- do.call("data.frame", mapply(char2utime, x = d[, it, drop = FALSE],
                                      tz = d[, iz, drop = FALSE]))
    names(u) <- gsub("(date|)time", "utime", names(u), perl = TRUE)
    d <- cbind(d, u)
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

## EMA completion status
complete <- read.data("EMA_Completed.csv", contextid)

## context in which the EMA notification was sent
notify <- read.data("EMA_Context_Notified.csv", contextid, notified_utime)

## context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv", contextid, engaged_utime)
engage$engageid <- with(engage, paste(contextid, engaged_utime, sep = "_"))

## EMA responses
## FIXME: all timezone data are missing
## fiddle with this
emaresponse <- read.data("EMA_Response.csv")
emaresponse$message <- omit.space(emaresponse$message)
emaresponse$questionid <-
  with(emaresponse, paste(contextid, question, sep = "_"))

## planning
struc <- read.data("Structured_Planning_Response.csv", contextid, utime_started)
unstruc <- read.data("Unstructured_Planning_Response.csv", contextid,
                     utime_started)
struc$response <- omit.space(struc$response)
unstruc$response <- omit.space(unstruc$response)

## suggestions
## FIXME: suggestion type (sedentary vs active)? see Excel files on box
## FIXME: ignore prefetch when... (another record with same id, slot, etc)?
## FIXME: time of notification, had notify = true?
## time for treatment occasion is decision table time stamp
decision <- read.data("Momentary_Decision.csv", decisionid)
decision$suggestid <- with(decision, paste(decisionid, is_prefetch, sep = "_"))
## context for suggestion? in decision table, if two take prefetch = false
## if just one and is prefetch = true, context will be outdated (30 min)
response <- read.data("Response.csv", decisionid, responded_utime)

## physical activity
## one minute windows, but might be more granular depending on server load
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

save.image("heartsteps.RData")

setwd(wd)
