## load exported CSV files into data frames, apply fixes,
## save as an R workspace (csv.RData file)

source("init.R")
setwd(sys.var$mbox)

## --- user-level data

## participant/user list
participants <- read.data("HeartSteps Participant Directory.csv", list(user))
participants$intake.date <- char2date(participants$intake.interview.date,
                                      "%m/%d/%Y")
participants$exit.date <- char2date(participants$exit.interview.date, "%m/%d/%Y")

## intake interviews
intake <- read.data("Survey_Intake.csv", list(user), skip = 3, na.strings = "X")
intake$startdate <- char2date(intake$startdate, "%m/%d/%Y")

## exit interviews
exit <- read.data("Survey_Exit.csv", list(user), skip = 3, na.strings = "X")
exit$exitdate <- char2date(exit$exitdate, "%m/%d/%Y")

## --- HeartSteps application data

## application usage
usage <- read.data("Heartsteps_Usage_History.csv", list(user, end.utime))

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
timeslot <- subset(timeslot, !duplicated(cbind(user, date.updated, tz, morning,
                                               lunch, dinner, evening, ema)))
## time intervals for user-designated times
temp <- do.call("cbind",
                lapply(timeslot[, match(slots, names(timeslot))],
                       function(x) as.difftime(x, "%H:%M", units = "hours")))
colnames(temp) <- paste(slots, "hours", sep = ".")
timeslot <- cbind(timeslot, temp)
## discard obviously invalid slot selections
timeslot <- timeslot[valid.slots(timeslot), ]

## daily weather by city
weather <- read.data("Weather_History.csv", list(date))
weather$date <- char2date(weather$date, "%Y:%m:%d")

## --- planning intervention and evening questionnaire (EMA)

## context in which the EMA notification was sent
## nb: planning and EMA questions administered must be inferred from responses
notify <- read.data("EMA_Context_Notified.csv", list(user, notified.utime))
notify$ema.set.today <- gsub(",ema_finish", "", notify$ema.set.today)
notify$ema.set.today.length <-
  unlist(lapply(strsplit(notify$ema.set.today, ","), length))
notify$days.since <-
  with(notify, change(user, notified.utime, notified.utime)) / (24 * 60^2)
notify <- merge.last(notify,
                     subset(timeslot, select = c(user, utime.updated,
                                                 morning.hours:ema.hours)),
                       "user", var.x = "notified.utime", var.y = "utime.updated")
notify$notified.time.slot <-
  ltime2slot(notified.time.hour, notified.time.min, notify)

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
with(plan, any(duplicated(cbind(user, date.started))))

## EMA completion status
## nb: we look to EMA response for completion status instead
complete <- read.data("EMA_Completed.csv",
                      list(user, date.stamp, completed != "true",
                           -as.numeric(utime.stamp)))
complete$completed <- complete$completed == "true"

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
temp <- rbind(subset(notify, select = c(contextid, timezone, tz, gmtoff)),
              subset(plan, select = c(contextid, timezone, tz, gmtoff)),
              subset(engage, select = c(contextid, timezone, tz, gmtoff)),
              subset(complete, select = c(contextid, timezone, tz, gmtoff)))
temp <- subset(temp, !duplicated(cbind(contextid, tz)))
## any contextIDs associated with more than one time zone?
any(duplicated(temp$contextid))
ema <- merge(ema, temp, by = "contextid", all.x = TRUE)

## add notification time, given by message.time unless question is 6 or 7
temp <- with(subset(subset(ema, !(question %in% paste(6:7))),
                    !duplicated(contextid)),
             data.frame(contextid, notified.time = message.time))
ema <- merge(ema, temp, by = "contextid", all.x = TRUE)

## calculate date-time elements as 'read.data' with the now-available
## time zone information
ema$notified.date <- as.Date(ema$notified.time)
ema$notified.utime <- with(ema, char2utime(notified.time, gmtoff))
ema$message.utime <- with(ema, char2utime(message.time, gmtoff))
ema$utime.stamp <- with(ema, char2utime(time.stamp, gmtoff))
ema <-
  cbind(ema,
        with(ema, char2calendar(notified.time, tz, prefix = "notified.time")),
        with(ema, char2calendar(message.time, tz, prefix = "message.time")),
        with(ema, char2calendar(time.stamp, tz, prefix = "time.stamp")))

## keep unique EMA responses
ema <- ema[with(ema, order(user, order, question, notified.utime,
                           -as.numeric(utime.stamp))), ]
dup.ema <- check.dup(ema, "checks/dup_ema_response.csv",
                     user, notified.date, notified.time.hour, tz, order,
                     question, response)
ema <- ema[!dup.ema$is.dup, ]

## form EMA question set, akin to 'ema.set.today' in 'notify'
ema <- merge(ema,
             with(ema, aggregate(question, by = list(user, notified.utime),
                                 function(x) paste(unique(x), collapse = ","))),
             by.x = c("user", "notified.utime"),
             by.y = paste("Group", 1:2, sep = "."))
names(ema)[ncol(ema)] <- "ema.set"
ema$ema.set.length <- unlist(lapply(strsplit(ema$ema.set, ","), length))

## keep the longest and latest EMA set in the same day
ema <- ema[with(ema, order(user, notified.utime, order, -ema.set.length,
                           -as.numeric(utime.stamp))), ]
dup.ema <- check.dup(ema, "checks/dup_ema_multiset.csv",
                     user, notified.date, notified.time.hour, tz, order)
ema <- ema[!dup.ema$is.dup, ]

## keep unique, responded, or latest EMA notifications
notify <- merge(notify,
                subset(ema, !duplicated(cbind(user, notified.utime)),
                       select = c(user, notified.date, notified.time.hour, tz,
                                  notified.utime, ema.set)),
                by = c("user", "notified.date", "notified.time.hour", "tz"),
                all.x = TRUE, suffixes = c("", ".ema"))
notify <- notify[with(notify, order(user, notified.date, is.na(ema.set),
                                    abs(notified.utime - notified.utime.ema),
                                    -as.numeric(notified.utime))), ]
dup.notify <- check.dup(notify, "checks/dup_notify.csv", user, notified.date, tz)
notify <- notify[!dup.notify$is.dup, ]

## avoid associating EMA records with the wrong day, presumably due to
## a late time slot plus the time lag for activity recognition or user response
with(notify, table(notified.time.hour, tz))
with(engage, table(engaged.time.hour, tz))
with(plan, table(time.started.hour, tz))
with(ema, table(notified.time.hour, tz))
notify$ema.date <- with(notify, notified.date - (notified.time.hour < 18))
engage$ema.date <- with(engage, engaged.date - (engaged.time.hour < 18))
plan$ema.date <- with(plan, date.started - (time.started.hour < 18))
ema$ema.date <- with(ema, notified.date - (notified.time.hour < 18))
with(notify, any(duplicated(cbind(user, ema.date))))
with(plan, any(duplicated(cbind(user, ema.date))))
with(ema, any(duplicated(cbind(user, ema.date, order))))

## convert numeric EMA responses, parse @-delimited EMA responses 
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

## --- activity suggestion interventions

## suggestion message tags
## nb: messages were not tagged in GAE tables, so we apply them after the fact
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

## momentary decision (send suggestion or not)
## nb: we look to the response for the provided suggestion
decision <- read.data("Momentary_Decision.csv",
                      list(user, date.stamp, tz, time.slot,
                           is.prefetch == "true", valid != "valid", utime.stamp))
decision$returned.message <- normalize.text(decision$returned.message)
decision$is.prefetch <- decision$is.prefetch == "true"
decision$notify <- decision$notify == "True"
decision$is.randomized <- decision$is.randomized == "true"
decision$valid <- decision$valid == "valid"
decision$snooze.status <- decision$snooze.status == "true"
decision$slot <- match(decision$time.slot, slots)
## dispense with extraneous prefetch data
decision <- subset(decision, !(is.prefetch &
                               duplicated(cbind(user, date.stamp, tz, slot))))
## user-designated time slot
decision <- merge.last(decision,
                       subset(timeslot, select = c(user, utime.updated,
                                                   morning.hours:ema.hours)),
                       "user", var.x = "utime.stamp", var.y = "utime.updated")
decision$time.stamp.slot <- ltime2slot(time.stamp.hour,
                                       time.stamp.min + 30 * is.prefetch,
                                       decision)
with(decision, table(slot != time.stamp.slot, user))
## keep unique, "send" or within-slot decisions
decision <- decision[with(decision,
                          order(user, date.stamp, tz, slot, !valid,
                                !notify, slot != time.stamp.slot,
                                utime.stamp)), ]
dup.decision <- check.dup(decision, "checks/dup_decision.csv",
                          user, date.stamp, slot)
decision <- decision[!dup.decision$is.dup, ]
## missing day of week
write.data(subset(decision, day.of.week == ""), "checks/decision_nowkday.csv")
## time slot mismatch
write.data(subset(decision, slot != time.stamp.slot),
           "checks/decision_outsideslot.csv")
## add message tags
decision <- merge(decision, messages,
                  by.x = "returned.message", by.y = "message",
                  all.x = TRUE, sort = FALSE)
decision <- decision[with(decision, order(user, utime.stamp)), ]
## missing tags
write.data(subset(decision, notify & is.na(tag.active)),
           "checks/decision_notags.csv")

## response to suggestions
## suggestion could be prefetched 30 minutes prior to initial notify time
## user has 30 minutes from the initial notification to respond
response <- read.data("Response.csv", list(user, notified.utime))
response$notification.message <- normalize.text(response$notification.message)
response <- merge(response,
                  subset(decision,
                         select = c(user, date.stamp, time.stamp.hour, tz,
                                    time.slot, slot, time.stamp.slot, time.stamp)),
                  by.x = c("user", "notified.date", "notified.time.hour", "tz"),
                  by.y = c("user", "date.stamp", "time.stamp.hour", "tz"),
                  all.x = TRUE)
check.dup(response, "checks/dup_response.csv", user, notified.date, slot)
## add message tags
response <- merge(response, messages,
                  by.x = "notification.message", by.y = "message",
                  all.x = TRUE, sort = FALSE)
response <- response[with(response, order(user, notified.utime)), ]
## missing tags
write.data(subset(response, is.na(tag.active)), "checks/response_notags.csv")

## --- physical activity

## Jawbone
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data(c("jawbone_step_count_data_07-15.csv",
                       "jawbone_step_count_data_08-15.csv",
                       "jawbone_step_count_data_09-15.csv",
                       "jawbone_step_count_data_10-15.csv",
                       "jawbone_step_count_data_11-15.csv"),
                     list(user, end.utime))
check.dup(jawbone, "checks/dup_jawbone.csv", user, end.utime)

## Google Fit
## nb: degree of fractional seconds varies over time
## nb: step counts provided over time intervals of continuous physical activity
googlefit <- read.data(c("google_fit_data_07-15.csv",
                         "google_fit_data_08-15.csv",
                         "google_fit_data_09-15.csv",
                         "google_fit_data_10-15.csv",
                         "google_fit_data_11-15.csv"),
                       list(user, end.utime))
check.dup(googlefit, "checks/dup_googlefit.csv", user, end.utime)

rm(temp)
rm(sys.var)
save.image("csv.RData", safe = FALSE)
