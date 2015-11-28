## load exported CSV files into data frames, apply fixes,
## save as an R workspace (csv.RData file)

source("init.R")
setwd(sys.var$mbox)

## --- user-level data

## participant/user list
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
                       "user", var.x = "notified.utime", var.y = "utime.updated")
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
temp <-
  rbind(subset(notify, select = c(user, contextid, timezone, tz, gmtoff)),
        subset(plan, select = c(user, contextid, timezone, tz, gmtoff)),
        subset(engage, select = c(user, contextid, timezone, tz, gmtoff)),
        subset(complete, select = c(user, contextid, timezone, tz, gmtoff)))
temp <- subset(temp, !duplicated(cbind(contextid, tz)))
## any contextIDs associated with more than one time zone?
any(duplicated(temp$contextid))
nrow(ema <- merge(ema, temp[, -1], by = "contextid", all.x = TRUE))

## save users' time zones, which can point to a variety of issues:
## multiple time zones subject to the time zone bugs,
## invalid time zone to problems that arise with non-English locale
temp <- subset(temp, !duplicated(cbind(user, timezone)), select = -contextid)
with(temp, table(duplicated(user)))
with(subset(temp, grepl("^\\?+", timezone)), length(unique(user)))
write.data(temp, "checks/user_timezones.csv")

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

## every EMA response has a corresponding notification?
with(ema, table(contextid %in% notify$contextid,
                paste(user, ema.date) %in% with(notify, paste(user, ema.date))))

## every EMA engagement has a corresponding notification?
with(engage,
     table(contextid %in% notify$contextid,
           paste(user, ema.date) %in% with(notify, paste(user, ema.date))))

## if every EMA response associated with a single contextID,
## the off-diagonal should be zero
with(ema, table(duplicated(contextid, order),
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

## persistency of identifiers over time in momentary decision
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

## persistency of identifiers over time in suggestion response
with(response, table(duplicated(decisionid)))

## persistency the same between momentary decision and response?
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
             "user", var.x = "utime.stamp", var.y = "utime.updated")
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
             "user", var.x = "notified.utime", var.y = "utime.stamp",
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

## updated linked status
decision <- merge(decision,
                  subset(response, select = c(user, date.stamp, slot, response)),
                  by = c("user", "date.stamp", "slot"), all.x = TRUE)
nrow(decision)

## among momentary decisions in the same intended slot, keep linked decisions
decision <- decision[with(decision, order(user, date.stamp, slot,
                                          !link | is.na(response))), ]
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
