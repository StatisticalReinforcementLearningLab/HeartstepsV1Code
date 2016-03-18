## load exported CSV files into data frames, apply fixes,
## save as an R workspace (csv.RData file)

source("init.R")
setwd(sys.var$mbox.data)

## --- administrative and interview data

## participant/user list
participants <- read.data("HeartSteps Participant Directory.csv", list(user))
participants$intake.interview.date <-
  char2date(participants$intake.interview.date, "%m/%d/%Y")
participants$exit.interview.date <-
  char2date(participants$exit.interview.date, "%m/%d/%Y")
participants$dropout.date <- char2date(participants$dropout.date, "%m/%d/%Y") 
participants$travel.start <- char2date(participants$travel.start, "%m/%d/%Y") 
participants$travel.end   <- char2date(participants$travel.end,   "%m/%d/%Y") 

## intake interviews
## FIXME: IPAQ allows "unsure" answers only for minutes of activity;
##        check missing responses for number of active days in past week
## FIXME: consider incorporating instrument/item names into the variable
##        names (e.g. ipaq.vigor.days, ipaq.vigor.hours, ipaq.vigor.mins, etc)
intake <- read.data("Survey_Intake.csv", list(user), skip = 3, na.strings = "X")
intake$startdate <- char2date(intake$startdate, "%m/%d/%Y")

## exit interviews
exit <- read.data("Survey_Exit.csv", list(user), skip = 3, na.strings = "X")
exit$exitdate <- char2date(exit$exitdate, "%m/%d/%Y")

## --- HeartSteps application data

## application usage
usage <- read.data("Heartsteps_Usage_History.csv", list(user, end.utime))
## dispense with data time-stamped pre-study
temp <- usage$start.date < as.Date("2015-07-01")
write.data(usage[temp, ], "checks/usage_prestudy.csv")
usage <- usage[!temp, ]

## snooze enabled or disabled
snooze <- read.data("Snoozed_FromInApp.csv", list(user, utime.stamp))

## home and work locations
## nb: time zone data are unavailable
address <- read.data("User_Addresses.csv", list(user, time.updated))

## calendars
## nb: Google calendar API data are unavailable
## nb: time zone data are unavailable
calendar <- read.data("User_Calendars.csv", list(user, time.updated))

## suggestion and EMA timeslots
timeslot <- read.data("User_Decision_Times.csv", list(user, utime.updated))
## drop redundant timeslot updates
timeslot <- subset(timeslot,
                   !duplicated(cbind(user, date.updated, tz,
                                     morning, lunch, dinner, evening, ema)))
## time intervals for user-designated times
temp <- do.call("cbind",
                lapply(timeslot[, match(slots, names(timeslot))],
                       function(x) as.difftime(x, "%H:%M", units = "hours")))
colnames(temp) <- paste(slots, "hours", sep = ".")
timeslot <- cbind(timeslot, temp)
## discard obviously invalid slot selections
timeslot <- timeslot[valid.slots(timeslot), ]
## discard recurrent slots by time
dup.timeslot <- check.dup(timeslot, "checks/dup_timeslot.csv",
                          user, utime.updated)
timeslot <- timeslot[!dup.timeslot$is.dup, ]
## slot corresponding to the update time
timeslot$slot.updated <- ltime2slot(time.updated.hour, time.updated.min,
                                    timeslot)

## daily weather by city
weather <- read.data("Weather_History.csv", list(date))
weather$date <- char2date(weather$date, "%Y:%m:%d")

## --- planning and evening questionnaire (EMA)

## context in which the EMA notification was sent
## nb: planning and EMA questions administered must be inferred from responses
notify <- read.data("EMA_Context_Notified.csv", list(user, notified.utime))
notify$ema.set.today <- gsub(",ema_finish", "", notify$ema.set.today)
notify <- merge.last(notify,
                     subset(timeslot, select = c(user, utime.updated,
                                                 morning.hours:ema.hours)),
                     id = "user", var.x = "notified.utime",
                     var.y = "utime.updated")
notify$notified.time.slot <-
  ltime2slot(notified.time.hour, notified.time.min, notify)

## any recurrent contextIDs?
with(notify, table(duplicated(contextid),
                   duplicated(cbind(user, notified.date, tz))))

## context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv",
                    list(user, engaged.utime, valid == "false"))
## keep unique or classified-activity engagements
dup.engage <- check.dup(engage, "checks/dup_ema_engaged.csv",
                        user, engaged.utime)
engage <- engage[!dup.engage$is.dup, ]

## planning
plan <- read.data(c("Structured_Planning_Response.csv",
                    "Unstructured_Planning_Response.csv"),
                  list(user, date.started,
                       time.started.yday != time.finished.yday,
                       -as.numeric(utime.finished)))
plan$response <- normalize.text(plan$response)
plan$list.of.options <- normalize.text(plan$list.of.options)
plan$planning <- c("structured", "unstructured")[1 + is.na(plan$list.of.options)]
## keep unique or latest same-day plans
dup.plan <- check.dup(plan, "checks/dup_planning.csv", user, date.started, tz)
plan <- plan[!dup.plan$is.dup, ]

## EMA completion status
## nb: we look to EMA response for completion status instead
complete <- read.data("EMA_Completed.csv",
                      list(user, date.stamp, completed != "true",
                           -as.numeric(utime.stamp)))
complete$completed <- complete$completed == "true"

## start assembling user time zones
set.unames <- function(x, n) {
  n <- intersect(c("user", "timezone", "tz", "gmtoff", n,
                   gsub("time", "utime", n[length(n)])), names(x))
  setNames(x[, match(n, names(x))], c(n[-(-1:0 + length(n))], "time", "utime"))
}
timezone <- rbind(set.unames(notify, c("contextid", "notified.time")),
                  set.unames(plan, c("contextid", "time.started")),
                  set.unames(engage, c("contextid", "engaged.time")),
                  set.unames(complete, c("contextid", "time.stamp")))

## EMA response
ema <- read.data("EMA_Response.csv", list(contextid))
ema$message <- normalize.text(ema$message)
## fix question numbering - need EMA questions 1-7, research 1-4
ema$question[ema$question == "6"] <- "5"
ema$question[ema$question == "7"] <- "6"
ema$question[ema$question == "8"] <- "7"

## resolve missing time zone in EMA response from other EMA tables
## nb: this presumes that contextID can distinguish between different-day EMAs
##     and each contextID is associated with at most one time zone
temp <- subset(timezone, !duplicated(cbind(contextid, tz)), select = -utime)
## any contextIDs associated with more than one time zone?
any(duplicated(temp$contextid))
nrow(ema <- merge(ema, temp[, -1], by = "contextid", all.x = TRUE))

## add notification time, given by message.time unless question is 6 or 7
temp <- with(subset(subset(ema, !(question %in% paste(6:7))),
                    !duplicated(contextid)),
             data.frame(contextid, notified.time = message.time))
nrow(ema <- merge(ema, temp, by = "contextid", all.x = TRUE))

## calculate date-time elements as 'read.data' with now-available time zone
ema$notified.date <- as.Date(ema$notified.time)
ema$notified.utime <- with(ema, char2utime(notified.time, gmtoff))
ema$message.utime <- with(ema, char2utime(message.time, gmtoff))
ema$utime.stamp <- with(ema, char2utime(time.stamp, gmtoff))
ema <-
  cbind(ema,
        with(ema, char2calendar(notified.time, tz, prefix = "notified.time")),
        with(ema, char2calendar(message.time, tz, prefix = "message.time")),
        with(ema, char2calendar(time.stamp, tz, prefix = "time.stamp")))

## avoid associating EMA records with the wrong day, primarily due to
## a late time slot and the time lag for activity recognition or user response
with(notify, table(notified.time.hour, tz))
with(engage, table(engaged.time.hour, tz))
with(plan, table(time.started.hour, tz))
with(ema, table(notified.time.hour, tz))
## cut-off of 18:00 is somewhat arbitrary
notify$ema.date <- with(notify, notified.date - (notified.time.hour < 18))
engage$ema.date <- with(engage, engaged.date - (engaged.time.hour < 18))
plan$ema.date <- with(plan, date.started - (time.started.hour < 18))
ema$ema.date <- with(ema, notified.date - (notified.time.hour < 18))

## any planning responses recur by EMA day?
with(plan, table(duplicated(contextid), duplicated(cbind(user, ema.date))))

## every EMA response has a corresponding notification?
with(ema, table(contextid %in% notify$contextid,
                paste(user, ema.date) %in% with(notify, paste(user, ema.date))))

## every EMA engagement has a corresponding notification?
with(engage,
     table(contextid %in% notify$contextid,
           paste(user, ema.date) %in% with(notify, paste(user, ema.date))))

## if each EMA response is associated with a single contextID,
## the off-diagonal should be zero
with(ema, table(duplicated(cbind(contextid, order)),
                duplicated(cbind(user, ema.date, order))))

## keep unique EMA responses
ema <- ema[with(ema, order(user, order, question, notified.utime,
                           -as.numeric(utime.stamp))), ]
dup.ema <- check.dup(ema, "checks/dup_ema_response.csv",
                     user, ema.date, order, question, response)
ema <- ema[!dup.ema$is.dup, ]

## form EMA question set, akin to 'ema.set.today' in EMA notifications
ema <- merge(ema,
             with(ema, aggregate(question, by = list(user, notified.utime),
                                 function(x) paste(unique(x), collapse = ","))),
             by.x = c("user", "notified.utime"),
             by.y = paste("Group", 1:2, sep = "."))
nrow(ema)
names(ema)[ncol(ema)] <- "ema.set"
ema$ema.set.length <- unlist(lapply(strsplit(ema$ema.set, ","), length))

## keep the longest and latest EMA set in the same day
ema <- ema[with(ema, order(user, notified.utime, order, -ema.set.length,
                           -as.numeric(utime.stamp))), ]
dup.ema <- check.dup(ema, "checks/dup_ema_multiset.csv", user, ema.date, order)
ema <- ema[!dup.ema$is.dup, ]

## keep unique, most-responded, or latest EMA notifications
notify <-
  merge(notify,
        subset(ema, !duplicated(cbind(user, notified.utime)),
               select = c(user, ema.date, notified.time, tz, notified.utime,
                          ema.set, ema.set.length)),
        by = c("user", "ema.date"), all.x = TRUE, suffixes = c("", ".ema"))
nrow(notify)
notify <- notify[with(notify, order(user, ema.date, is.na(ema.set),
                                    -ema.set.length,
                                    abs(notified.utime - notified.utime.ema),
                                    -as.numeric(notified.utime))), ]
dup.notify <- check.dup(notify, "checks/dup_notify.csv", user, ema.date)
notify <- notify[!dup.notify$is.dup, ]

## convert numeric and parse @-delimited EMA responses 
ema$hectic <- with(ema, as.numeric(ifelse(question == "1", response, NA)))
ema$stressful <- with(ema, as.numeric(ifelse(question == "2", response, NA)))
ema$typical <- with(ema, as.numeric(ifelse(question == "3", response, NA)))
ema <- cbind(ema, match.option(ema4, ema$response,
                               ema$question == "4", "active", FALSE))
ema$follow <- with(ema, ifelse(question == "5", response, NA))
ema <- cbind(ema, match.option(ema6, ema$response, ema$question == "6", "down"))
ema$down.msg <- with(ema, ifelse(question == "6", message, NA))
ema <- cbind(ema, match.option(ema7, ema$response, ema$question == "7", "up"))
ema$up.msg <- with(ema, ifelse(question == "7", message, NA))
ema <- cbind(ema, match.option(research1, ema$response,
                               ema$question == "research1", "barrier"),
             match.option(research2, ema$response,
                          ema$question == "research2", "enabler"))
ema$energetic <- with(ema, as.numeric(ifelse(question == "research3",
                                             response, NA)))
ema$urge <- with(ema, as.numeric(ifelse(question == "research4", response, NA)))

## --- activity suggestions and responses

## suggestion message tags
## nb: messages were not saved along with their relevant tags,
##     so we use this table to apply them after the fact
messages <- read.data("Reviewed_Heartsteps_Messages.csv", NULL, skip = 1)
messages$message <- normalize.text(messages$message)
## replace recurrent tag variable with tag indicators
tags <- sort(unique(unlist(subset(messages, select = tag.1:tag.14))))
tags <- tags[!(tags %in% c("", "other", "outdoor_snow"))]
## no tags imply that all tags apply
temp <- messages$tag.1 == ""
messages <- data.frame(message = messages$message,
                       t(apply(subset(messages, select = tag.1:tag.14), 1,
                               function(x) sapply(tags, function(tag)
                                 any(grepl(tag, x))))))
names(messages)[-1] <- paste("tag", tags, sep = ".")
messages[temp, -1] <- TRUE
## combine recurrent messages
## nb: recurrences distinguish context, which we can't recover post-hoc
messages <- aggregate(. ~ message, data = messages, any)

## momentary decision (to send suggestion or not)
## nb: suggestion could be prefetched 30 minutes prior to initial notify time
decision <- read.data("Momentary_Decision.csv",
                      list(user, utime.stamp, is.prefetch, notify != "True",
                           valid != "valid"))
decision$returned.message <- normalize.text(decision$returned.message)
decision$is.prefetch <- decision$is.prefetch == "true"
decision$notify <- decision$notify == "True"
decision$is.randomized <- decision$is.randomized == "true"
decision$valid <- decision$valid == "valid"
decision$snooze.status <- decision$snooze.status == "true"
decision$slot <- match(decision$time.slot, slots)

## missing day of week (just record in case this affects contextualization)
write.data(subset(decision, day.of.week == ""), "checks/decision_nowkday.csv")

## any prefetch singletons?
with(decision, table(!(decisionid %in% decisionid[duplicated(decisionid)])
                     & is.prefetch))

## persistence of identifiers over time in momentary decision
with(decision, table(duplicated(decisionid),
                     duplicated(cbind(decisionid, is.prefetch))))
with(subset(decision, !is.prefetch),
     table(duplicated(decisionid), duplicated(cbind(decisionid, time.slot))))
with(decision, table(duplicated(cbind(user, utime.stamp)),
                     duplicated(cbind(user, utime.stamp, is.prefetch))))

## response to suggestions
## nb: user has 30 minutes from the initial notification to respond
response <- read.data("Response.csv", list(user, notified.utime))
response$notification.message <- normalize.text(response$notification.message)

## finish assembling user time zones
timezone <- rbind(subset(timezone, select = -contextid),
                  set.unames(usage, "start.time"),
                  set.unames(timeslot, "time.updated"),
                  set.unames(decision, "time.stamp"),
                  set.unames(response, "responded.time"))
timezone <- subset(timezone, !duplicated(cbind(user, utime, timezone)))
timezone <- timezone[with(timezone, order(user, utime)), ]
rownames(timezone) <- NULL
timezone$ltime <- with(timezone, utime + gmtoff)
## unknown time zone indicative of non-English device locale
timezone$en.locale <- !grepl("^\\?+$", timezone$timezone)
with(timezone, length(unique(user[!en.locale])))

## save users' time zones, which can point to a variety of issues, such as...
temp <- subset(timezone, !duplicated(cbind(user, timezone)),
               select = -c(ltime, utime))
temp <- temp[with(temp, order(user)), ]
write.data(temp, "checks/user_timezones.csv")
## ... multiple time zones subject to the time zone bugs
with(temp, table(duplicated(user)))
## ... invalid time zone to problems that arise with non-English locale
with(subset(temp, grepl("^\\?+", timezone)), length(unique(user)))

## persistence of identifiers over time in suggestion response
with(response, table(duplicated(decisionid)))

## persistence the same between momentary decision and response?
table(with(response, decisionid[duplicated(decisionid)]) %in%
      with(decision, decisionid[duplicated(cbind(decisionid, is.prefetch))]))

## prefetch data always overridden if non-prefetch decision is available?
with(merge(response, subset(decision, !is.prefetch & notify),
           by = "decisionid", all.x = TRUE),
     table(notification.message == returned.message))

## add indication of linkage to response by date, message and delta
## between send time by the server and notification time on the device
temp <- merge(decision,
              subset(response, select = c(user, notified.date, notified.utime,
                                          notification.message, response)),
              by.x = c("user", "date.stamp", "returned.message"),
              by.y = c("user", "notified.date", "notification.message"),
              all.x = TRUE)
temp$delta <- with(temp, abs(as.numeric(utime.stamp - notified.utime)))
temp <- merge(temp,
              aggregate(cbind(min.delta = delta)
                        ~ user + notified.utime + returned.message,
                        function(x) min(x, Inf), data = temp),
              by = c("user", "notified.utime", "returned.message"), all.x = TRUE)
decision <-
  merge(decision,
        aggregate(cbind(link = ifelse(is.na(response), 0, delta == min.delta))
                  ~ user + is.prefetch + utime.stamp + returned.message,
                  max, data = temp),
        by = c("user", "is.prefetch", "utime.stamp", "returned.message"))
nrow(decision)

## among decisions made at the same time: discard if unlinked or prefetch
decision <- decision[with(decision,
                          order(user, utime.stamp, -link, is.prefetch)), ]
dup.decision <- check.dup(decision, "checks/dup_decision.csv", user, utime.stamp)
decision <- decision[!dup.decision$is.dup, ]

## add user-designated time slots to momentary decision
decision <-
  merge.last(decision,
             subset(timeslot,
                    select = c(user, utime.updated, morning.hours:ema.hours)),
             id = "user", var.x = "utime.stamp", var.y = "utime.updated")
## designated hours for the intended time slot
decision$slot.hours <- apply(subset(decision,
                                    select = c(slot, morning.hours:ema.hours)),
                             1, function(x) x[-1][x[1]])
## (last preceeding) slot to which the decision time stamp actually belongs
decision$time.stamp.slot <-
  ltime2slot(time.stamp.hour, time.stamp.min + 30 * is.prefetch, decision)
with(decision, table(slot != time.stamp.slot, user))

## add the *intended* time slots to suggestion response
## nb: we use intended versus user-designated to ensure that correctly associate
##     each response to its corresponding decision
response <-
  merge.last(response,
             subset(decision,
                    select = c(user, utime.stamp, date.stamp, time.stamp, tz,
                               time.slot, slot, slot.hours, time.stamp.slot,
                               notify, is.randomized, is.prefetch,
                               returned.message, link)),
             id = "user", var.x = "notified.utime", var.y = "utime.stamp",
             suffixes = c("", ".decision"))
## match between linked messages?
with(response, table(notification.message == returned.message,
                     link, useNA = "ifany"))

## among suggestion responses in the same intended slot,
## keep rated/snoozed or earliest responses
response <- response[with(response,
                          order(user, date.stamp, slot,
                                response == "no_response", responded.utime)), ]
dup.response <- check.dup(response, "checks/dup_response.csv",
                          user, date.stamp, slot)
response <- response[!dup.response$is.dup, ]

## update link status
decision <- merge(decision,
                  subset(response, select = c(user, date.stamp, slot, response)),
                  by = c("user", "date.stamp", "slot"), all.x = TRUE)
nrow(decision)
decision$response[!decision$link] <- NA

## among momentary decisions in the same intended slot,
## select one from among duplicates, with preference for linked, responded,
## lower-discrepancy, non-prefetch, earlier decisions
decision <- decision[with(decision,
                          order(user, date.stamp, slot, !link,
                                response %in% c(NA, "no_response"),
                                abs(slot - time.stamp.slot), is.prefetch,
                                utime.stamp)), ]
dup.decision <- check.dup(decision, "checks/dup_decision.csv",
                          user, date.stamp, slot)
decision <- decision[!dup.decision$is.dup, ]

## remaining time slot mismatches
write.data(subset(decision, slot != time.stamp.slot),
           "checks/decision_outsideslot.csv")

## add message tags
nrow(decision <- merge(decision, messages,
                       by.x = "returned.message", by.y = "message",
                       all.x = TRUE, sort = FALSE))
decision <- decision[with(decision, order(user, date.stamp, slot)), ]
## missing tags
write.data(subset(decision, notify & is.na(tag.active)),
           "checks/decision_notags.csv")

## --- application usage tracker

## pre-process tracker files (if not previously done)
mbox.tracker <- paste0(sys.var$mbox, "Interviews/Exit/App_Usage_Tracker_Data/")
tracker.files <- list.files(mbox.tracker)
tracker.files <- setdiff(tracker.files,
                         intersect(tracker.files, list.files("tracker")))
if (length(tracker.files)) {
  ## routine to normalize date format
  normalize.format <- function(x) {
    ## tracker files indicate date-time format in column name
    format <- gsub(".+\\((.+)\\)", "\\1", strsplit(x[1], ",")[[1]][2])
    if (format == "MM-dd-yyyy HH:mm:ss") # %m-%d-%Y
      gsub("(^[^,]+),([0-9]+)-([0-9]+)-([0-9]+)", "\\1,\\4-\\2-\\3", x)
    else if (format == "dd-MM-yyyy HH:mm:ss")  # %d-%m-%Y
      gsub("(^[^,]+),([0-9]+)-([0-9]+)-([0-9]+)", "\\1,\\4-\\3-\\2", x)
    else { # %d/%b/%Y with alphabetic month part %b dependent on locale
      x <- gsub("(^[^,]+),([0-9]+)/([0-9A-Za-z]+).*/([0-9]+)",
                "\\1,\\4-\\3-\\2", x) # toss out Chinese characters
      x <- gsub("-Jan-", "-01-", x, fixed = TRUE)
      x <- gsub("-Feb-", "-02-", x, fixed = TRUE)
      x <- gsub("-Mar-", "-03-", x, fixed = TRUE)
      x <- gsub("-Apr-", "-04-", x, fixed = TRUE)
      x <- gsub("-May-", "-05-", x, fixed = TRUE)
      x <- gsub("-Jun-", "-06-", x, fixed = TRUE)
      x <- gsub("-Jul-", "-07-", x, fixed = TRUE)
      x <- gsub("-Aug-", "-08-", x, fixed = TRUE)
      x <- gsub("-Sep-", "-09-", x, fixed = TRUE)
      x <- gsub("-Oct-", "-10-", x, fixed = TRUE)
      x <- gsub("-Nov-", "-11-", x, fixed = TRUE)
      gsub("-Dec-", "-12-", x, fixed = TRUE)
    }
  }
  temp <- lapply(tracker.files,
                 function(x) readLines(paste0(mbox.tracker, x), warn = FALSE))
  ## omit non-breaking spaces and hyphens
  temp <- lapply(temp, gsub, pattern = " ", replacement = " ", fixed = TRUE)
  temp <- lapply(temp, gsub, pattern = "‑", replacement = "-", fixed = TRUE)
  ## strip commas with trailing spaces
  temp <- lapply(temp, gsub, pattern = ", ", replacement = " ", fixed = TRUE)
  temp <- lapply(temp, normalize.format)
  mapply(function(x, y) writeLines(x, paste0("tracker/", y)),
         temp, tracker.files, SIMPLIFY = FALSE)
}

tracker <- read.data(list.files("tracker", full.names = TRUE),
                     list(user), add.user = TRUE,
                     col.names = c("app", "start.datetime", "duration",
                                   "duration.secs"))
tracker$order <- unlist(sapply(table(tracker$user), seq, from = 1, by = 1))
## omit "total" rows, found in the last line of the tracker files
nrow(tracker <- subset(tracker, !grepl("^Total", start.datetime)))
## any dates not parsed correctly?
write.data(subset(tracker, is.na(start.date)), "checks/tracker_nadate.csv")

## break ties in time by adding fractional seconds
## nb: presumes tracker records in chronological order
print(temp <- with(tracker, max(table(user, start.datetime))))
tracker$rank <-
  unlist(lapply(unique(tracker$user),
                function(x) with(subset(tracker, user == x),
                                 rank(start.datetime, ties.method = "min"))))
tracker$fsecs <-
  unlist(lapply(lapply(unique(tracker$user),
                       function(x) with(subset(tracker, user == x),
                                        table(rank)[match(unique(rank),
                                                          sort(unique(rank)))])),
                function(y) lapply(y, seq, from = 1, by = 1))) - 1
tracker$start.datetime <- 
  with(tracker, paste(start.datetime,
                      formatC(fsecs * round(10/temp),
                              width = ceiling(log(temp, 10)), flag = "0"),
                      sep = "."))
tracker$start.ltime <- char2utime(tracker$start.datetime)
## any ties remaining?
with(tracker, any(duplicated(cbind(user, start.ltime))))

## tracker records local date-times but no time zone,
## so presume users start in Eastern time zone and LOCF-impute time zone
temp <- with(subset(tracker, !duplicated(user)),
             as.POSIXlt(char2utime(start.datetime), tz = "US/Eastern"))
temp <- do.call("data.frame", lapply(temp, unlist))
temp <- with(subset(tracker, !duplicated(user)),
             data.frame(user, timezone = "Eastern Standard Time",
                        tz = paste0("Etc/GMT", temp$gmtoff / 60^2),
                        gmtoff = temp$gmtoff, ltime = start.ltime,
                        utime = start.ltime + temp$gmtoff,
                        en.locale = TRUE))
tracker <- merge.last(tracker, rbind(temp, subset(timezone, select = -time)),
                      id = "user", var.x = "start.ltime", var.y = "ltime")
## any unlinked tracker records?
write.data(subset(tracker, is.na(gmtoff)), "checks/tracker_notz.csv")
tracker$start.utime <- with(tracker, start.ltime - gmtoff)
tracker <- tracker[with(tracker, order(user, start.utime)), ]
tracker$utime <- tracker$ltime <- rownames(tracker) <- NULL

## application list, excluding devices with non-English locale
write.data(with(subset(tracker, !(user %in% user[!en.locale])),
                data.frame(app = sort(unique(app)))), "checks/tracker_apps.csv")

## --- physical activity

## Jawbone
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data(c("jawbone_step_count_data_07-15.csv",
                       "jawbone_step_count_data_08-15.csv",
                       "jawbone_step_count_data_09-15.csv",
                       "jawbone_step_count_data_10-15.csv",
                       "jawbone_step_count_data_11-15.csv",
                       "jawbone_step_count_data_12-15.csv",
                       "jawbone_step_count_data_01-16.csv",
                       "jawbone_step_count_data_02-16.csv"),
                     list(user, end.utime))
dup.jawbone <- check.dup(jawbone, "checks/dup_jawbone.csv", user, end.utime)
jawbone <- jawbone[!dup.jawbone$is.dup, ]

## Google Fit
## nb: degree of fractional seconds varies over time
## nb: step counts provided over time intervals of continuous physical activity
googlefit <- read.data(c("google_fit_data_07-15.csv",
                         "google_fit_data_08-15.csv",
                         "google_fit_data_09-15.csv",
                         "google_fit_data_10-15.csv",
                         "google_fit_data_11-15.csv",
                         "google_fit_data_12-15.csv",
                         "google_fit_data_01-16.csv",
                         "google_fit_data_02-16.csv"),
                       list(user, end.utime))
dup.googlefit <- check.dup(googlefit, "checks/dup_googlefit.csv",
                           user, end.utime)
googlefit <- googlefit[!dup.googlefit$is.dup, ]

## don't save temporary objects, system-specific variables or functions
rm(temp)
rm(sys.var)
rm(list = lsf.str(all.names = TRUE))
ls()

save.image("csv.RData", safe = FALSE)
