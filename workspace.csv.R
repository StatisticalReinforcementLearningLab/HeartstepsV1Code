## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
file <- "csv.RData"

## --- EMA completion status
complete <- read.data("EMA_Completed.csv", list(user, contextid, utime.stamp))

## keep unique or first-recorded completions
dup <- check.dup(complete, "checks/dup_ema_complete.csv", contextid)
complete <- subset(complete, !dup)

## --- context in which the EMA notification was sent
notify <- read.data("EMA_Context_Notified.csv",
                    list(user, contextid, notified.utime))

## keep unique or first-recorded notifications
dup <- check.dup(notify, "checks/dup_ema_notified.csv", contextid)
notify <- subset(notify, !dup)

## --- context in which the user engaged with the EMA
engage <- read.data("EMA_Context_Engaged.csv",
                    list(user, contextid, engaged.utime))

## keep unique or classified activity engagements
engage <- engage[with(engage, order(contextid, engaged.utime,
                                    recognized.activity %in% c(NA, "N/A"))), ]
dup <- check.dup(engage, "checks/dup_ema_engaged.csv", contextid, engaged.utime)
engage <- subset(engage, !dup)

## --- EMA responses
## nb: time zone data are unavailable
ema <- read.data("EMA_Response.csv",
                 list(user, contextid, question, utime.stamp))

ema$response <- strip.white(ema$response)
ema$message <- strip.white(ema$message)

## duplicates due to answer revisions
## keep unique or latest same-day answers to EMA questions
ema <- ema[with(ema, order(contextid, question,
                           message.time.mday != time.stamp.mday)), ]
dup <- check.dup(ema, "checks/dup_ema_response.csv", contextid, question)
ema <- subset(ema, !dup, select = -(gmtoff:time.stamp.yday))

## --- planning
plan <- read.data(c("Structured_Planning_Response.csv",
                    "Unstructured_Planning_Response.csv"),
                  list(user, contextid, utime.finished))

## duplicates due to answer revisions
## keep unique or latest same-day plans
plan <- plan[with(plan, order(contextid,
                              time.started.mday != time.finished.mday)), ]
dup <- check.dup(plan, "checks/dup_planning.csv", contextid)
plan <- subset(plan, !dup)

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

## drop prefetch data in a prefetch/non-prefetch dual
decision <- subset(decision, !duplicated(msgid))
## decisionid persisting into next time slot
write.data(subset(decision, decisionid %in% decisionid[duplicated(decisionid)]),
           "checks/persist_decisionid.csv")

## missing day of week
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

## --- physical activity - Jawbone
## nb: step counts provided in one minute windows for now;
##     might eventually be more granular depending on server load
jawbone <- read.data("jawbone_step_count_data.csv", list(user, end.utime))

## periods of inactivity longer than one day
jawbone$duration <- with(jawbone, end.utime - start.utime)
jawbone$days.since <- with(jawbone, change(user, start.utime, end.utime)
                           - duration) / (60^2 * 24)
jawbone$days.since[jawbone$days.since == 0] <- NA
write.data(subset(jawbone, days.since > 1), "checks/inactivity_jbone_gt1.csv")
check.dup(jawbone, "checks/dup_jawbone.csv", user, end.utime)

## --- physical activity - Google Fit
googlefit <- read.data("google_fit_data.csv", list(user, end.utime))

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
address <- read.data("User_Addresses.csv", list(user, time.updated))

## --- calendars
## nb: time zone data are unavailable
calendar <- read.data("User_Calendars.csv", list(user, time.updated))

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

length(users <- get.values("user", complete, notify, engage, ema, plan,
                           decision, response, jawbone))
length(contexts <- get.values("contextid", complete, notify, engage, ema, plan))
length(decisions <- get.values("msgid", decision, response))

save.image(file, safe = FALSE)
