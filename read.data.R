source("functions.R")

wd <- getwd()
setwd(paste(mbox, "HeartSteps/Data", sep = "/"))

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## read and revise data files
read.data <- function(file, id = userid, time = utime.stamp) {
  l <- sapply(file, read.csv, header = TRUE, simplify = FALSE)
  if (length(l) > 1) {
    n <- unique(unlist(lapply(l, names)))
    d <- lapply(l, function(x) data.frame(matrix(NA, nrow(x), length(n),
                                                 dimnames = list(NULL, n))))
    sapply(1:length(l), function(i) d[[i]][, match(names(l[[i]]), n)] <<- l[[i]])
  }
  d <- do.call("rbind", d)
  names(d) <- tolower(names(d))
  names(d) <- gsub("_", ".", names(d))
  names(d)[names(d) == "user"] <- "userid"
  ## keep only pilot users
  if ("userid" %in% names(d))
    d <- subset(d, grepl("heartsteps.test[0-9]+@", userid, perl = TRUE),
                select = -key)
  ## add offset in seconds from GMT/UTC, time zone identifier
  if (!is.null(d$timezone)) {
    d$gmtoff <- NA
    d$tz <- d$timezone
    l <- d$timezone %in% c("GMT", "UTC")
    d$gmtoff[l] <- 0
    if (!is.null(d$utc.to.local.delta) & !all(l)) {
      d$gmtoff[l] <- 0
      d$gmtoff[!l] <- 60 * d$utc.to.local.delta
      d$tz[!l] <- paste("Etc/GMT", c("-", "+")[pmax(1, sign(d$gmtoff) + 1)],
                        formatC(abs(d$gmtoff) / 60^2), sep = "")
      d$tz[is.na(d$gmtoff)] <- NA
    }
    l <- grepl("(^time\\.(fin|sta|up)|\\.(date|)time$)", names(d), perl = TRUE)
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
  ## sort by provided identifier and time variables
  time <- substitute(time)
  time <- eval(time, d)
  if (!is.null(time))
    d <- d[order(time), ]
  id <- substitute(id)
  id <- eval(id, d)
  if (!is.null(id))
    d <- d[order(id), ]
  row.names(d) <- NULL
  d
}

## EMA completion status
complete <- read.data("EMA_Completed.csv", contextid)

## context in which the EMA notification was sent
notified <- read.data("EMA_Context_Notified.csv", contextid, notified.utime)

## context in which the user engaged with the EMA
engaged <- read.data("EMA_Context_Engaged.csv", contextid, engaged.utime)

## EMA responses
## nb: all time zone data are missing
ema <- read.data("EMA_Response.csv", contextid)

## planning
plan <- read.data(c("Structured_Planning_Response.csv",
                    "Unstructured_Planning_Response.csv"),
                  contextid, utime.finished)

## suggestions
## FIXME: suggestion type (sedentary vs active)? see Excel files on box
## FIXME: ignore prefetch when... (another record with same id, slot, etc)?
## FIXME: time of notification, had notify = true?
## time for treatment occasion is decision table time stamp
decision <- read.data("Momentary_Decision.csv", decisionid)
decision$suggestid <- with(decision, paste(decisionid, is_prefetch, sep = "."))
## context for suggestion? in decision table, if two take prefetch = false
## if just one and is prefetch = true, context will be outdated (30 min)
response <- read.data("Response.csv", decisionid, responded.utime)

## physical activity
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data("jawbone_step_count_data.csv", time = end.utime)

## application usage
usage <- read.data("Heartsteps_Usage_History.csv", time = end.utime)

## snooze enabled or disabled
snooze <- read.data("Snoozed_FromInApp.csv")

## home and work locations
## FIXME: all timezone data are missing
address <- read.data("User_Addresses.csv", time = time.updated)

## calendars
## FIXME: all timezone data are missing
calendar <- read.data("User_Calendars.csv", time = time.updated)

## suggestion and EMA timeslots
timeslot <- read.data("User_Decision_Times.csv", time = utime.updated)

## daily weather by city
weather <- read.data("Weather_History.csv", id = NULL, time = date)
weather$date <- char2date(weather$date, "%Y:%m:%d")

## all unique identifier values
get.ids <- function(idname, ...)
  sort(unique(unlist(lapply(list(...), function(x) x[[idname]]))))

length(user.ids <- get.ids("userid", complete, notify, engage, ema,
                           plans, planu, decision, response))

length(context.ids <- get.ids("contextid", complete, notify, engage,
                              ema, plans, planu))

length(decision.ids <- get.ids("decisionid", decision, response))

save.image("heartsteps.RData")

setwd(wd)
