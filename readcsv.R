source("datafun.R")

wd <- getwd()
setwd(paste(mbox, "HeartSteps/Tables", sep = "/"))
options(stringsAsFactors = FALSE)

## read and revise exported files
get.data <- function(file, id = contextID, time = utime_stamp) {
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
    i <- grepl("(^time_sta|_time$)", names(d), perl = TRUE)
    ## POSIXlt times
    l <- do.call("data.frame", apply(d[, i, drop = FALSE], 2, char2ltime))
    ## Unix times
    u <- data.frame(apply(l, 2, ltime2utime))
    names(l) <- gsub("time", "ltime", names(l))
    names(u) <- gsub("time", "utime", names(u))
    d <- cbind(d, l, u)
  }
  id <- substitute(id)
  id <- eval(id, d)
  time <- substitute(time)
  time <- eval(time, d)
  d[order(id, time), ]
}

## EMAs
complete <- get.data("EMA_Completed.csv")
notify <- get.data("EMA_Context_Notified.csv", time = notified_utime)
engage <- get.data("EMA_Context_Engaged.csv", time = engaged_utime)
## FIXME: all timezone data are missing
emaresponse <- get.data("EMA_Response.csv")

## planning
struc <- get.data("Structured_Planning_Response.csv", time = utime_started)
unstruc <- get.data("Unstructured_Planning_Response.csv", time = utime_started)

## suggestions
decision <- get.data("Momentary_Decision.csv", decisionID)
response <- get.data("Response.csv", decisionID, responded_utime)

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

emaresponse$message <- omit.space(emaresponse$message)

setwd(wd)
