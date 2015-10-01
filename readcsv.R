wd <- getwd()
setwd(mbox)
options(stringsAsFactors = FALSE)

## read exported files, omit testers
complete <- subset(read.csv("EMA_Completed.csv", header = TRUE),
                   grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
complete <- complete[with(complete, order(contextID, time_stamp)), ]

notify <- subset(read.csv("EMA_Context_Notified.csv", header = TRUE),
                 grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
notify <- notify[with(notify, order(contextID, notified_time)), ]

engage <- subset(read.csv("EMA_Context_Engaged.csv", header = TRUE),
                 grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
engage <- engage[with(engage, order(contextID, engaged_time)), ]

answer <- subset(read.csv("EMA_Response.csv", header = TRUE),
                 grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
answer <- answer[with(answer, order(contextID, time_stamp)), ]

struc <- subset(read.csv("Structured_Planning_Response.csv", header = TRUE),
                grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
struc <- struc[with(struc, order(contextID, time_started)), ]

unstruc <- subset(read.csv("Unstructured_Planning_Response.csv", header = TRUE),
                  grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
unstruc <- unstruc[with(unstruc, order(contextID, time_started)), ]

decision <- subset(read.csv("Momentary_Decision.csv", header = TRUE),
                   grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
decision <- decision[with(decision, order(decisionID, time_stamp)), ]

response <- subset(read.csv("Response.csv", header = TRUE),
                   grepl("heartsteps.test[0-9]+@", userID, perl = TRUE))
response <- response[with(response, order(decisionID, responded_time)), ]

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

char2datetime <- function(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
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
