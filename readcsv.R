wd <- getwd()
setwd("~/mbox/HeartSteps/Tables")
options(stringsAsFactors = FALSE)

## todo
# exporter make "utc to local" "local to utc"
# here multiply by -1

# check where timezone is missing, send list of users with this to Andy

## read exported files, omit testers
get.data <- function(file, id = contextID, time = time_stamp) {
  d <- subset(read.csv(file, header = TRUE),
              grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
  id <- substitute(id)
  id <- eval(id, d)
  time <- substitute(time)
  time <- eval(time, d)
  d[order(id, time), ]
}

complete <- get.data("EMA_Completed.csv")
notify <- get.data("EMA_Context_Notified.csv", time = notified_time)
engage <- get.data("EMA_Context_Engaged.csv", time = engaged_time)
answer <- get.data("EMA_Response.csv")
struc <- get.data("Structured_Planning_Response.csv", time = time_started)
unstruc <- get.data("Unstructured_Planning_Response.csv", time = time_started)
decision <- get.data("Momentary_Decision.csv", decisionID)
response <- get.data("Response.csv", decisionID, responded_time)

table(complete$timezone, useNA = "ifany")
table(notify$timezone, useNA = "ifany")
table(engage$timezone, useNA = "ifany")
table(answer$timezone, useNA = "ifany")
table(struc$timezone, useNA = "ifany")
table(unstruc$timezone, useNA = "ifany")
table(decision$timezone, useNA = "ifany")
table(response$timezone, useNA = "ifany")

subset(complete, timezone %in% c(NA, "????????"))$gps
subset(notify, timezone %in% c(NA, "????????"))$gps
subset(engage, timezone %in% c(NA, "????????"))$gps
subset(answer, timezone %in% c(NA, "????????"))$gps
subset(struc, timezone %in% c(NA, "????????"))$gps
subset(unstruc, timezone %in% c(NA, "????????"))$gps
subset(decision, timezone %in% c(NA, "????????"))$gps
subset(response, timezone %in% c(NA, "????????"))$gps

with(subset(complete, timezone %in% c(NA, "????????")), table(userID))
with(subset(notify, timezone %in% c(NA, "????????")), table(userID))
with(subset(engage, timezone %in% c(NA, "????????")), table(userID))
with(subset(answer, timezone %in% c(NA, "????????")), table(userID))
with(subset(struc, timezone %in% c(NA, "????????")), table(userID))
with(subset(unstruc, timezone %in% c(NA, "????????")), table(userID))
with(subset(decision, timezone %in% c(NA, "????????")), table(userID))
with(subset(response, timezone %in% c(NA, "????????")), table(userID))

## all unique identifier values
get.ids <- function(idname, ...)
  sort(unique(unlist(lapply(list(...), function(x) x[[idname]]))))

length(user.ids <- get.ids("userID", complete, notify, engage, answer,
                           struc, unstruc, decision, response))

length(context.ids <- get.ids("contextID", complete, notify, engage, answer,
                              struc, unstruc))

length(decision.ids <- get.ids("decisionID", decision, response))

## strip whitespace
omit.space <- function(x) {
  x <- gsub("\\n", "", x, perl = TRUE)
  x <- gsub("^ +", "", x, perl = TRUE)
  x <- gsub(" +$", "", x, perl = TRUE)
  gsub(" +$", " ", x, perl = TRUE)
}

answer$message <- omit.space(answer$message)

## date-time conversion
char2date <- function(x, format = "%Y-%m-%d")
  as.Date(paste(x), format = format)

char2ltime <- function(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  strptime(paste(x), format = format, tz = tz)

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
