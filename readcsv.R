wd <- getwd()
setwd(paste(mbox, "HeartSteps/Tables", sep = "/"))
options(stringsAsFactors = FALSE)

## read and revise exported files
get.data <- function(file, id = contextID, time = time_stamp) {
  d <- subset(read.csv(file, header = TRUE),
              grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
  ## timezone field gives the name, which is not an identifier
  ## FIXME: verify that delta combines UTC offset and DST
  ## FIXME: timezone identifiers are platform-dependent
  if (!is.null(d$utc_to_local_delta)) {
    d$tz <- paste("Etc/GMT",
                  c("-", "+")[pmax(1, sign(d$utc_to_local_delta) + 1)],
                  formatC(abs(d$utc_to_local_delta) / 60), sep = "")
    d$tz[is.na(d$utc_to_local_delta)] <- NA
    d$tz_valid <- d$tz %in% OlsonNames()
    if (any(!d$tz_valid)) {
      print(table(d$tz[!d$tz_valid]))
      print(sort(unique(d$userID[!d$tz_valid])))
    }
  }
  id <- substitute(id)
  id <- eval(id, d)
  time <- substitute(time)
  time <- eval(time, d)
  d[order(id, time), ]
}

## EMAs
complete <- get.data("EMA_Completed.csv")
notify <- get.data("EMA_Context_Notified.csv", time = notified_time)
engage <- get.data("EMA_Context_Engaged.csv", time = engaged_time)
emaresponse <- get.data("EMA_Response.csv")

## planning
struc <- get.data("Structured_Planning_Response.csv", time = time_started)
unstruc <- get.data("Unstructured_Planning_Response.csv", time = time_started)

## suggestions
decision <- get.data("Momentary_Decision.csv", decisionID)
response <- get.data("Response.csv", decisionID, responded_time)

## physical activity
jawbone <- read.csv("jawbone_step_count_data.csv", header = TRUE)

## all unique identifier values
get.ids <- function(idname, ...)
  sort(unique(unlist(lapply(list(...), function(x) x[[idname]]))))

length(user.ids <- get.ids("userID", complete, notify, engage, emaresponse,
                           struc, unstruc, decision, response))

length(context.ids <- get.ids("contextID", complete, notify, engage, emaresponse,
                              struc, unstruc))

length(decision.ids <- get.ids("decisionID", decision, response))

## strip whitespace
omit.space <- function(x) {
  x <- gsub("\\n", "", x, perl = TRUE)
  x <- gsub("^ +", "", x, perl = TRUE)
  x <- gsub(" +$", "", x, perl = TRUE)
  gsub(" +$", " ", x, perl = TRUE)
}

emaresponse$message <- omit.space(emaresponse$message)

## date-time-related functions
json2list <- function(x) {
  x <- x[-c(1, length(x))]
  x <- sapply(x, gsub, pattern = "[{[]$", replacement = "list(", perl = TRUE)
  x <- sapply(x, gsub, pattern = "[}]],$", replacement = "),", perl = TRUE)
  x <- sapply(x, gsub, pattern = "\" +: +", replacement = "=", perl = TRUE)
  x <- sapply(x, gsub, pattern = " +\"", replacement = "", perl = TRUE)
  eval(parse(text = paste(c("list(", x, ")"), collapse = "")))
}

## 
## nb: rate and number of daily queries are limited
## https://developers.google.com/maps/documentation/timezone/usage-limits
gps2timezone <- function(x) {
  u <- url(paste("https://maps.googleapis.com/maps/api/timezone/json?location=",
                 x[1], ",", x[2], "&timestamp=0&sensor=false", sep = "",
                 collapse = ""))
  on.exit(close(u))
  l <- json2list(readLines(u))
  l$timeZoneName
}

## convert character string to Date, assuming the given format
char2date <- function(x, format = "%Y-%m-%d")
  as.Date(paste(x), format = format)

## convert character string to POSIXlt, assuming the given format
char2ltime <- function(x, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  do.call("c", mapply(strptime, x = paste(x), format = format, tz = tz,
                      SIMPLIFY = FALSE))

## convert POSIXlt to Unix time (seconds since 1970-01-01 00:00.00 UTC)
## nb: this accounts for different time zones
ltime2secs <- function(x)
  unclass(as.POSIXct(x))

## convert POSIX[cl]t to time of day
timeofday <- function(x) {
  x <- as.numeric(format(x, "%H"))
  cut(x, c(min(x, na.rm = TRUE) - 1, 12, 17, max(x, na.rm = TRUE) + 1),
      labels = c("Morning", "Afternoon", "Evening"),
      right = FALSE)
}

## copy variable from y to x, taking first matches in an identifier
copy <- function(x, y, varname, idname)
  y[match(x[[idname]], y[[idname]]), names(y) == varname]

setwd(wd)
