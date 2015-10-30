## load exported CSV files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
file <- "csv.RData"

## --- user list, intake and exit interviews
## FIXME: try with CSV file exported by Nick (Office Libre works fine)
## FIXME: apply more meaningful variable names on the Excel end
## FIXME: check missing values
user <- read.data("HeartSteps Participant Directory.csv", list(user))
user$intake.date <- char2date(user$intake.interview.date, "%m/%d/%Y")
user$exit.date <- char2date(user$exit.interview.date, "%m/%d/%Y")

intake <- read.data("Survey_Intake.csv", list(user), skip = 3)
intake$date1 <- char2date(intake$date1, "%m/%d/%Y")

exit <- read.data("Survey_Exit.csv", list(user), skip = 3)
exit$date2 <- char2date(exit$date2, "%m/%d/%Y")

## reconstruct linkage ID
## user + udate
## udate = local date of notification + UTC offset

## notify -> next 

## complete - use only to infer time zone for ema
## - for each EMA, take time zone of next available complete

## --- EMA completion status
complete <- read.data("EMA_Completed.csv", list(user, utime.stamp))
complete$completed <- complete$completed == "true"

## completion status might not be confirmed until X hours after EMA time slot,
## so if hour < earliest time slot, move date back one day
## FIXME: check that this makes sense
check.dup(complete, "checks/dup_ema_complete.csv", user, date.stamp)
complete$date.stamp <- with(complete, date.stamp
                            - (!completed & time.stamp.hour < 20))
dup <- check.dup(complete, "checks/dup_ema_complete.csv", user, date.stamp)

## --- context in which the EMA notification was sent
notify <- read.data("EMA_Context_Notified.csv", list(user, notified.utime))

## keep unique or first-recorded notifications
dup <- check.dup(notify, "checks/dupid_ema_notified.csv", contextid)
notify <- notify[!dup, ]

## 
dup <- check.dup(notify, "checks/dup_ema_notify.csv", user, notified.date)

dup <- check.dup(notify, "checks/dup_ema_notify.csv", user, date.stamp)

## --- context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv",
                    list(user, engaged.utime,
                         recognized.activity %in% c(NA, "N/A")))

## keep unique or classified activity engagements
dup <- check.dup(engage, "checks/dup_ema_engaged.csv", contextid, engaged.utime)
engage <- engage[!dup, ]

## --- planning
plan <- read.data(c("Structured_Planning_Response.csv",
                    "Unstructured_Planning_Response.csv"),
                  list(user, contextid, time.started.yday != time.finished.yday))

## duplicates due to answer revisions
## keep unique or latest same-day plans
dup <- check.dup(plan, "checks/dupid_planning.csv", contextid)
plan <- subset(plan, !dup)
plan <- plan[with(plan, order(user, utime.finished)), ]

## remaining duplicate occurs in the central time zone;
## this is likely due to the pre 10/28 time zone bug, so discard the earlier
## FIXME: check this
dup <- check.dup(plan, "checks/dup_planning.csv", user, date.finished)
plan <- plan[!dup, ]

## --- EMA responses
## nb: time zone data are unavailable
ema <- read.data("EMA_Response.csv",
                 list(user, contextid, question, time.stamp), utime = FALSE)
ema$response <- strip.white(ema$response)
ema$message <- strip.white(ema$message)

## duplicates due to answer revisions
## keep unique or latest same-day answers to EMA questions
ema <- ema[with(ema, order(contextid, question,
                           message.time.mday != time.stamp.mday)), ]
dup <- check.dup(ema, "checks/dup_ema_response.csv", contextid, question)
ema <- subset(ema, !dup, select = -(gmtoff:time.stamp.yday))

## infer EMA response time zone from other EMA tables
temp <-
  Reduce(function(x, y) merge(x, y, all = TRUE, by = "contextid"),
         list(with(notify,
                   data.frame(contextid, notified.timezone = timezone,
                              notified.tz = tz, notified.gmtoff = gmtoff)),
              with(subset(engage, !duplicated(contextid)),
                   data.frame(contextid, engaged.timezone = timezone,
                              engaged.tz = tz, engaged.gmtoff = gmtoff)),
              with(plan,
                   data.frame(contextid, plan.timezone = timezone,
                              plan.tz = tz, plan.gmtoff = gmtoff)),
              with(complete,
                   data.frame(contextid, complete.timezone = timezone,
                              complete.tz = tz, complete.gmtoff = gmtoff))))
temp$min.gmtoff <- with(temp, pmin(notified.gmtoff, engaged.gmtoff, plan.gmtoff,
                                   complete.gmtoff, na.rm = TRUE))
temp$max.gmtoff <- with(temp, pmax(notified.gmtoff, engaged.gmtoff, plan.gmtoff,
                                   complete.gmtoff, na.rm = TRUE))

## no EMA context
write.data(subset(temp, is.na(notified.tz) & is.na(engaged.tz)),
           file = "checks/missing_ema_context.csv")

## EMA saddling different time zones or DST settings
write.data(subset(temp, min.gmtoff != max.gmtoff),
           file = "checks/ema_multiple_tzdst.csv")

## zero rows returned above => just take time zone from complete
ema <- merge(ema, subset(complete, select = c(contextid, timezone, tz, gmtoff)),
             by = "contextid", all.x = TRUE)

## recalculate Unix time and POSIXlt elements
ema$message.utime <- with(ema, char2utime(message.time, gmtoff))
ema$utime.stamp <- with(ema, char2utime(time.stamp, gmtoff))
ema <- cbind(ema,
             with(ema, char2calendar(message.time, tz, prefix = "message.time")),
             with(ema, char2calendar(time.stamp, tz, prefix = "time.stamp")))

## --- suggestion messages
## FIXME: typo variants are added to source file
## FIXME: clarify meaning of tags; for example,
##        suggestions tagged neither active nor sedentary - what does this mean?
messages <- read.data("Reviewed_Heartsteps_Messages.csv", NULL, skip = 1)
messages$message <- strip.white(messages$message)
## replace recurrent tag variable with tag indicators
tags <- sort(unique(unlist(subset(messages, select = tag.1:tag.14))))
tags <- tags[!(tags %in% c("", "other", "outdoor_snow"))]
messages <- data.frame(message = messages$message,
                       t(apply(subset(messages, select = tag.1:tag.14), 1,
                               function(x) sapply(tags, function(tag)
                                 any(grepl(tag, x))))))
names(messages)[-1] <- paste("tag", tags, sep = ".")
## combine recurrent messages
## FIXME: check that this makes sense
messages <- aggregate(. ~ message, data = messages, any)

## --- momentary decision (send suggestion or not)
decision <- read.data("Momentary_Decision.csv",
                      list(user, decisionid, is.prefetch))

decision$returned.message <- strip.white(decision$returned.message)
decision$is.prefetch <- decision$is.prefetch == "true"
decision$notify <- decision$notify == "True"
decision$msgid <- with(decision, paste(decisionid, time.slot, sep = "_"))

## recurrent decisionid, but non-prefetch
write.data(subset(decision, duplicated(msgid) & !is.prefetch),
           "check/recur_nonprefetch.csv")

## drop prefetch data in a prefetch/non-prefetch duo
decision <- subset(decision, !duplicated(msgid))

## decisionid persisting into next time slot
write.data(subset(decision, decisionid %in% decisionid[duplicated(decisionid)]),
           "checks/persist_decisionid.csv")

## missing day of week
## FIXME: would this affect the message selection?
write.data(subset(decision, day.of.week == ""), "checks/decision_nowkday.csv")

## add message tags
decision <- merge(decision, messages,
                  by.x = "returned.message", by.y = "message",
                  all.x = TRUE, sort = FALSE)
## missing tags
write.data(subset(decision, notify & is.na(tag.active)),
           "checks/decision_notags.csv")

dup <- check.dup(decision, "checks/dup_decision.csv", decisionid, time.slot)
decision <- subset(decision, !dup)

## --- response to suggestions
## FIXME: merge on message text and proximity in time
## FIXME: parse question options using doc from Andy
response <- read.data("Response.csv", list(user, decisionid, responded.utime))

response$notification.message <- strip.white(response$notification.message)
response <- merge(response,
                  subset(decision,
                         select = c(decisionid, msgid, time.slot, is.prefetch,
                                    notify, returned.message)),
                  by.x = c("decisionid", "notification.message"),
                  by.y = c("decisionid", "returned.message"), all.x = TRUE)

## responses for decision = 'do not notify'
write.data(subset(response, !notify), "checks/donotnotify_response.csv")

dup <- check.dup(response, "checks/dup_response.csv", decisionid, time.slot)
response <- subset(response, !dup)

## --- Jawbone
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data(c("jawbone_step_count_data_07-15.csv",
                       "jawbone_step_count_data_08-15.csv",
                       "jawbone_step_count_data_09-15.csv",
                       "jawbone_step_count_data_10-15.csv"),
                     list(user, end.utime), ptime = FALSE)

## periods of inactivity longer than one day
jawbone$duration <- with(jawbone, end.utime - start.utime)
jawbone$days.since <- with(jawbone, change(user, start.utime, end.utime)
                           - duration) / (60^2 * 24)
jawbone$days.since[jawbone$days.since == 0] <- NA
write.data(subset(jawbone, days.since > 1), "checks/inactivity_jbone_gt1.csv")
check.dup(jawbone, "checks/dup_jawbone.csv", user, end.utime)

## --- Google Fit
## nb: step counts provided over time intervals of continuous physical activity
googlefit <- read.data(c("google_fit_data_07-15.csv",
                         "google_fit_data_08-15.csv",
                         "google_fit_data_09-15.csv",
                         "google_fit_data_10-15.csv"),
                       list(user, end.utime), ptime = FALSE)

## periods of inactivity longer than one day
googlefit$duration <- with(googlefit, end.utime - start.utime)
googlefit$days.since <- with(googlefit, change(user, start.utime, end.utime)
                             - duration) / (60^2 * 24)
googlefit$days.since[googlefit$days.since == 0] <- NA
write.data(subset(googlefit, days.since > 1), "checks/inactivity_gfit_gt1.csv")

## --- application usage
usage <- read.data("Heartsteps_Usage_History.csv", list(user, end.utime))

## --- snooze enabled or disabled
snooze <- read.data("Snoozed_FromInApp.csv")

## --- home and work locations
## nb: time zone data are unavailable
address <- read.data("User_Addresses.csv", list(user, time.updated),
                     utime = FALSE, ptime = FALSE)

## --- calendars
## nb: time zone data are unavailable
calendar <- read.data("User_Calendars.csv", list(user, time.updated),
                      utime = FALSE, ptime = FALSE)

## --- suggestion and EMA timeslots
timeslot <- read.data("User_Decision_Times.csv", list(user, utime.updated))

## drop redundant timeslots
timeslot <- subset(timeslot, !duplicated(paste(user, morning, lunch, dinner,
                                               evening, ema, sep = "_")))

## --- daily weather by city
weather <- read.data("Weather_History.csv", list(date))
weather$date <- char2date(weather$date, "%Y:%m:%d")

## --- check timezones
## nb: notification are sent according to the time slots of the local time zone
##     at which HeartSteps was installed or the last instance where the phone
##     was restarted (powered on)
user.tz <- get.values(c("user", "tz", "timezone"), complete, notify, engage,
                      plan, decision, response, usage, snooze)
write.data(subset(user.tz, !(tz %in% OlsonNames())
                  | grepl("?", timezone, fixed = TRUE)),
           "checks/invalid_timezone.csv")
write.data(subset(user.tz, !(timezone %in% "Eastern Standard Time")),
           "checks/outside_est_timezone.csv")

rm(temp)
save.image(file, safe = FALSE)
