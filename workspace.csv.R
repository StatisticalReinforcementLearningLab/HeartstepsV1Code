## load exported CSV files into data frames, apply fixes,
## save as an R workspace (csv.RData file)

source("init.R")
setwd(sys.var$mbox)

## --- participant-level data
## FIXME: check with Shawna about time "off-study"
## FIXME: finalize interview spreadsheet format

## user list
user <- read.data("HeartSteps Participant Directory.csv", list(user))
user$intake.date <- char2date(user$intake.interview.date, "%m/%d/%Y")
user$exit.date <- char2date(user$exit.interview.date, "%m/%d/%Y")

## intake interviews
intake <- read.data("Survey_Intake.csv", list(user), skip = 3,
                    na.strings = "X")
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
## hour and minutes for user-designated times
temp <- do.call("cbind", lapply(lapply(timeslot[, match(slots, names(timeslot))],
                                       strsplit, split = ":"),
                                do.call, what = "rbind"))
colnames(temp) <- paste(rep(slots, each = 2), c("hour", "min"), sep = ".")
mode(temp) <- "numeric"
timeslot <- cbind(timeslot, temp)

## daily weather by city
weather <- read.data("Weather_History.csv", list(date))
weather$date <- char2date(weather$date, "%Y:%m:%d")

## --- evening questionnaire (EMA) and planning intervention

## EMA completion status
## nb: we look to EMA response for completion status instead
complete <- read.data("EMA_Completed.csv",
                      list(user, date.stamp, completed != "true",
                           -as.numeric(utime.stamp)))
complete$completed <- complete$completed == "true"
dup.complete <- check.dup(complete, "checks/dup_complete.csv",
                          user, date.stamp, tz)

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

## context in which the EMA notification was sent
## nb: we look to EMA response for the EMA question set instead
notify <- read.data("EMA_Context_Notified.csv",
                    list(user, notified.date, tz, -as.numeric(notified.utime)))
notify$ema.set.today <- gsub(",ema_finish", "", notify$ema.set.today)
notify$ema.set.today.length <- unlist(lapply(strsplit(notify$ema.set.today, ","),
                                             length))
## keep unique or latest notification-EMA sets
dup.notify <- check.dup(notify, "checks/dup_ema_notify.csv",
                        user, notified.date, tz, ema.set.today)
notify <- notify[!dup.notify$is.dup, ]

## context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv",
                    list(user, engaged.utime, recognized.activity == "N/A"))
## keep unique or classified-activity engagements
dup.engage <- check.dup(engage, "checks/dup_ema_engaged.csv",
                        user, engaged.utime)
engage <- engage[!dup.engage$is.dup, ]

## EMA response
## user has 1 hour from initial notification to complete questionnaire
## nb: time zone data are unavailable
ema <- read.data("EMA_Response.csv", list(contextid))
ema$response <- normalize.text(ema$response)
ema$message <- normalize.text(ema$message)
## EMA questions 1-7, research 1-4
ema$question[ema$question == "6"] <- "5"
ema$question[ema$question == "7"] <- "6"
ema$question[ema$question == "8"] <- "7"
## convert numeric responses, parse @-delimited responses 
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
## assemble time zone information from other EMA tables
temp <- rbind(subset(notify, select = c(contextid, timezone, tz, gmtoff)),
              subset(plan, select = c(contextid, timezone, tz, gmtoff)),
              subset(engage, select = c(contextid, timezone, tz, gmtoff)),
              subset(complete, select = c(contextid, timezone, tz, gmtoff)))
temp <- subset(temp, !duplicated(cbind(contextid, tz)))
## any contextIDs associated with more than one time zone?
any(duplicated(temp$contextid))
## infer time zone of EMA response
## nb: this presumes that contextID can distinguish between different-day EMAs
##     and each contextID is associated with at most one time zone
ema <- merge(ema, temp, by = "contextid", all.x = TRUE)
## calculate date-time elements
ema$message.utime <- with(ema, char2utime(message.time, gmtoff))
ema$utime.stamp <- with(ema, char2utime(time.stamp, gmtoff))
ema <- cbind(ema,
             with(ema, char2calendar(message.time, tz, prefix = "message.time")),
             with(ema, char2calendar(time.stamp, tz, prefix = "time.stamp")))
## form list of completed EMA question sets, akin to 'ema.set.today' in 'notify'
## nb: this presumes that 'contextid' can separate same-day EMAs
ema <- ema[with(ema, order(user, message.date, contextid, order)), ]
ema <- merge(ema,
             with(ema, aggregate(question,
                                 by = list(user, message.date, contextid),
                                 function(x) paste(unique(x), collapse = ","))),
             by.x = c("user", "message.date", "contextid"),
             by.y = paste("Group", 1:3, sep = "."))
names(ema)[ncol(ema)] <- "ema.set"
ema$ema.set.length <- unlist(lapply(strsplit(ema$ema.set, ","), length))
## keep unique or latest same-day EMA sets
dup.ema <- check.dup(ema, "checks/dup_ema_response.csv",
                     user, message.date, tz, order, question, ema.set)
ema <- ema[!dup.ema$is.dup, ]

## EMA notification duplicates by user-day, but different EMA question set
## keep EMA notifications that either link to EMA response or occur later
notify <- merge(notify,
                subset(ema, order == 1,
                       select = c(user, message.date, tz, message.time.hour,
                                  message.time.min, message.time, ema.set,
                                  ema.set.length)),
                by.x = c("user", "notified.date", "tz", "notified.time.hour",
                         "notified.time.min"),
                by.y = c("user", "message.date", "tz", "message.time.hour",
                         "message.time.min"),
                all.x = TRUE)
notify <- notify[with(notify, order(user, notified.date, is.na(ema.set),
                                    -as.numeric(notified.utime))), ]
dup.notify <- check.dup(notify, "checks/dup_ema_notify_multiset.csv",
                        user, notified.date, tz)
notify <- notify[!dup.notify$is.dup, ]

## EMA response duplicates by user-day, but different EMA question set
## keep EMAs that either link to EMA notification or were completed later
ema <- merge(ema,
             subset(notify,
                    select = c(user, notified.date, tz, notified.time.hour,
                               notified.time.min, notified.time)),
             by.x = c("user", "message.date", "tz", "message.time.hour",
                      "message.time.min"),
             by.y = c("user", "notified.date", "tz", "notified.time.hour",
                      "notified.time.min"),
             all.x = TRUE)
ema <- ema[with(ema, order(user, message.date, order, is.na(notified.time),
                           -as.numeric(message.utime))), ]
dup.ema <- check.dup(ema, "checks/dup_ema_response_multiset.csv",
                     user, message.date, tz, order)
ema <- ema[!dup.ema$is.dup, ]

## assess link between planning and EMA notification via contextID
temp <- merge(plan, notify, by = "contextid", all.x = TRUE,
              suffixes = c("", ".notify"))
## linked to the same user, date and planning status?
temp$link.date <- with(temp, !is.na(user.notify) & user == user.notify
                       & date.started == notified.date
                       & tz == tz.notify)
temp$link.plan <- with(temp, link.date & planning == planning.today)
with(temp, table(link.date, link.plan))
write.data(subset(temp, select = c(contextid, link.date, link.plan)),
           "checks/link_contextid_plan_notify.csv")

## assess link between EMA response and notification via contextID
temp <- merge(subset(ema, order == 1), notify, by = "contextid",
              all.x = TRUE, suffixes = c("", ".notify"))
## linked to the same user, date and leading question?
temp$link.date <- with(temp, user == user.notify & message.date == notified.date
                       & tz == tz.notify)
temp$link.set <- with(temp, link.date
                      & question == unlist(lapply(strsplit(ema.set.today, ","),
                                                  function(x) x[1])))
with(temp, table(link.date, link.set))
write.data(subset(temp, order == 1, select = c(contextid, link.date, link.set)),
           "checks/link_contextid_ema_notify.csv")

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
                                                   morning.hour:ema.min)),
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
                          user, date.stamp, tz, slot)
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
                  subset(decision, select = c(user, date.stamp, time.stamp.hour,
                                              time.slot, slot, time.stamp.slot)),
                  by.x = c("user", "notified.date", "notified.time.hour"),
                  by.y = c("user", "date.stamp", "time.stamp.hour"),
                  all.x = TRUE)
dup.response <- check.dup(response, "checks/dup_response.csv",
                          user, notified.date, tz, slot, time.stamp.slot)
response <- response[!dup.response$is.dup, ]

## assess link via decisionID
temp <- merge(response, decision, by = "decisionid", all.x = TRUE,
              suffixes = c("", ".decision"))
## linked to the same user, day, message and roughly the same time?
temp$link <- with(temp, user == user.decision & notified.date == date.stamp
                  & notify & returned.message == notification.message
                  & notified.utime > utime.stamp
                  & (notified.utime - utime.stamp) < 60^2)
table(temp$link)
write.data(subset(temp, select = c(decisionid, link)),
           "checks/link_decisionid.csv")

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
## periods of inactivity longer than one day
jawbone$duration <- with(jawbone, end.utime - start.utime)
jawbone$days.since <- with(jawbone, change(user, start.utime, end.utime)
                           - duration) / (60^2 * 24)
jawbone$days.since[jawbone$days.since == 0] <- NA
write.data(subset(jawbone, days.since > 1), "checks/inactivity_jbone_gt1.csv")

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
## periods of inactivity longer than one day
googlefit$duration <- with(googlefit, end.utime - start.utime)
googlefit$days.since <- with(googlefit, change(user, start.utime, end.utime)
                             - duration) / (60^2 * 24)
googlefit$days.since[googlefit$days.since == 0] <- NA
write.data(subset(googlefit, days.since > 1), "checks/inactivity_gfit_gt1.csv")

rm(temp)
rm(wd)
save.image("csv.RData", safe = FALSE)
