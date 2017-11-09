# need to have suggest data frame in workspace
library(dplyr)
library(reshape2)
library(zoo)
library(forecast)

## Exclude decision points according to primary analysis
unavail_sent_slots <-
  filter(suggest, !avail & !is.na(notification.message))%>%
  select(user, study.day.nogap, slot, avail, notification.message) %>%
  mutate('Message sent' = !is.na(notification.message)) %>% select(-notification.message) %>%
  rename('User' = user, 'Non-travel study day'= study.day.nogap,
         'Decision slot' = slot, 'Available' = avail)
no_message_tag <- 
  filter(suggest, send & !travel & study.day.nogap <= 42 & is.na(send.sedentary)) %>%
  select(user, study.day.nogap, slot, 
         notification.message) %>%
  rename('User' = user,
         'Non-travel study day' = study.day.nogap,
         'Decision slot' = slot,
         'Sent mesasge' = notification.message)

ntravel <- sum(suggest$travel)
npost41 <- with(suggest, sum(study.day.nogap > 41, na.rm=T))
nexclude <- 
  ntravel + npost41 + nrow(unavail_sent_slots) + nrow(no_message_tag)

suggest.included <- 
  suggest %>%
  filter(!travel & study.day.nogap <= 41) %>%
  anti_join(mutate(no_message_tag, no_message_tag = T),
            by=c('user'='User','study.day.nogap'='Non-travel study day',
                 'slot'='Decision slot')) %>%
  anti_join(mutate(unavail_sent_slots, unavail_sent_slots = T),
            by=c('user'='User','study.day.nogap'='Non-travel study day',
                 'slot'='Decision slot'))
suggest.analysis <-
  suggest.included %>%
  arrange(user, study.day.nogap, decision.index.nogap)

navail <- sum(suggest.included$avail)
suggest.analysis$weekendTrue <-
  with(suggest.analysis, strftime(study.date,'%u') %in% c(6,7))

### Self efficacy 
user_selfeff <- 
  select(users, user, starts_with('selfeff')) %>%
  select(-ends_with('exit')) %>%
  select(-matches('selfeff.intake')) %>%
  melt(id='user') %>% group_by(user) %>%
  summarise(selfeff_sum = sum(value))
suggest.analysis <-
  suggest.analysis %>%
  left_join(user_selfeff, by='user')

### IPAQ Scores
user_ipaq_minimal <- 
  select(users, user, starts_with('ipaq.minimal')) %>%
  select(-ends_with('exit')) %>%
  melt(id='user') %>% group_by(user) %>%
  summarise(ipaq_minimal = value)
suggest.analysis <-
  suggest.analysis %>%
  left_join(user_ipaq_minimal, by='user')
user_ipaq_hepa <- 
  select(users, user, starts_with('ipaq.hepa')) %>%
  select(-ends_with('exit')) %>%
  melt(id='user') %>% group_by(user) %>%
  summarise(ipaq_hepa = value)
suggest.analysis <-
  suggest.analysis %>%
  left_join(user_ipaq_hepa, by='user')

## Conscientiousness
conc_item_names <- 
  c("detail","prepared","carryplans", "startwork", "wastetime", "duties", "makeplans" )
user_conc <-
  select(users,one_of(c('user',conc_item_names))) %>%
  melt(id='user') %>% group_by(user) %>%
  summarise(conc_sum = sum(value))
suggest.analysis <-
  suggest.analysis %>%
  left_join(user_conc, by='user')

## Add indicators for non-missing response and up/down
suggest.analysis <-
  suggest.analysis %>%
  mutate(have_thumbs = !is.na(response),
         thumbs_up_or_down = ifelse(is.na(response), NA,
                                    response %in% c("good","bad")),
         location.cat3 = ifelse(location.category=='work', 'work',
                                ifelse(location.category=='home','home','other')),
         pcip.numeric = as.numeric(precipitation.chance),
         weather.outdoor1 = ifelse(is.na(pcip.numeric) | pcip.numeric < 0, NA,
                                 ifelse(pcip.numeric < 70 & temperature > 0 & temperature < 33,
                                        1, 0))) 
suggest.analysis$location.cat3[which(is.na(suggest.analysis$location.category))] <- 'other'
suggest.analysis$loc.is.home <-
  with(suggest.analysis, location.cat3=='home')
suggest.analysis$loc.is.work <-
  with(suggest.analysis, location.cat3=='work')
suggest.analysis$loc.is.other <-
  with(suggest.analysis, location.cat3=='other')

suggest.analysis.wdays <- 
  filter(suggest.analysis, !weekendTrue) %>%
  left_join(
    suggest.analysis %>% filter(!weekendTrue) %>%
      group_by(user, study.day.nogap) %>% summarise(npoints=n())%>%
      group_by(user)%>% mutate(nwdays = n())%>% 
      mutate(study.weekday.nogap = 0:(max(nwdays)-1)) %>%
      select(user,study.day.nogap,study.weekday.nogap),
    by=c('user'='user','study.day.nogap'='study.day.nogap'))

## Count types of response to sent suggestion messages 
suggest.analysis <-
  suggest.analysis %>%
  group_by(user) %>%
  mutate(n_prev_sent_have_response =
           cumsum(c(0,(send & have_thumbs)[-n()])), # exclude current decision point
         n_prev_sent_have_response_exp85 =
           as.numeric(fitted.values(ses(c(0, as.numeric(send & have_thumbs))[-n()],
                                      initial='simple', alpha=0.85))),
         n_prev_active_have_response = 
           cumsum(c(0,(send.active & have_thumbs)[-n()])),
         n_prev_sent =
           cumsum(c(0,(send)[-n()])), # exclude current decision point
         n_prev_active = 
           cumsum(c(0,(send.active)[-n()])),
         n_prev_thumbs_updown =  # sedentary AND active messages
           cumsum(c(0,(send & have_thumbs & thumbs_up_or_down)[-n()])),
         n_prev_thumbs_updown_exp85 =
           as.numeric(fitted.values(ses(c(0, as.numeric(send&have_thumbs&thumbs_up_or_down))[-n()],
                                        initial='simple',alpha=0.85))),
         n_prev_active_thumbs_updown = 
           cumsum(c(0,(send.active & have_thumbs & thumbs_up_or_down)[-n()])),
         prop_prev_thumbs_updown = 
           ifelse(n_prev_sent_have_response==0, 0, # Setting 0/0 = 0
                  n_prev_thumbs_updown / n_prev_sent_have_response),
         prop_prev_active_thumbs_updown = 
           ifelse(n_prev_active_have_response==0, 0, # Setting 0/0 = 0
                  n_prev_active_thumbs_updown / n_prev_active_have_response)
         ) %>%
  ungroup

## Count types of response to sent suggestion messages (WEEKDAYS)
suggest.analysis.wdays <-
  suggest.analysis.wdays %>%
  group_by(user) %>%
  mutate(n_prev_sent_have_response =
           cumsum(c(0,(send & have_thumbs)[-n()])),
         n_prev_active_have_response = 
           cumsum(c(0,(send.active & have_thumbs)[-n()])),
         n_prev_sent =
           cumsum(c(0,(send)[-n()])),
         n_prev_active = 
           cumsum(c(0,(send.active)[-n()])),
         n_prev_thumbs_updown =  # sedentary AND active messages
           cumsum(c(0,(send & have_thumbs & thumbs_up_or_down)[-n()])),
         n_prev_active_thumbs_updown = 
           cumsum(c(0, (send.active & have_thumbs & thumbs_up_or_down)[-n()])),
         prop_prev_thumbs_updown = 
           ifelse(n_prev_sent_have_response==0, 0, # Setting 0/0 = 0
                  n_prev_thumbs_updown / n_prev_sent_have_response),
         prop_prev_active_thumbs_updown = 
           ifelse(n_prev_active_have_response==0, 0, # Setting 0/0 = 0
                  n_prev_active_thumbs_updown / n_prev_active_have_response)
  ) %>%
  ungroup

# Use a sliding window for the proportion of previous messages 
# that were given either thumbs up or thumbs down
ndays_window_thumbs <- 7 # the window in DAYS
ndays_window_thumbs_wdays <- 5 # Use a 5-day window for the weekday-only data frame

# Get the cumulative number of sent messages with a response from ndays_window_thumbs
# days before (take the count at the end of the day, so the window slides on the day scale) 
suggest.analysis <-
  suggest.analysis %>%
  left_join( 
    suggest.analysis %>%
      filter(slot==1) %>%
      mutate(study.day.plus6 = study.day.nogap + ndays_window_thumbs - 1,
             n_out7_sent_have_response = n_prev_sent_have_response,
             n_out7_sent = n_prev_sent,
             n_out7_active = n_prev_active,
             n_out7_thumbs_updown = n_prev_thumbs_updown,
             n_out7_active_have_response = n_prev_active_have_response,
             n_out7_active_thumbs_updown = n_prev_active_thumbs_updown) %>%
      select(user, study.day.plus6, n_out7_sent_have_response, n_out7_sent,n_out7_active,
             n_out7_thumbs_updown, n_out7_active_have_response, n_out7_active_thumbs_updown),
    by=c('user'='user','study.day.nogap'='study.day.plus6')
  ) %>%
  mutate(n_window_sent_have_response = 
           n_prev_sent_have_response - ifelse(is.na(n_out7_sent_have_response),
                                              0,
                                              n_out7_sent_have_response),
         n_window_sent = 
           n_prev_sent - ifelse(is.na(n_out7_sent), 0,
                                n_out7_sent),
         n_window_thumbs_updown =
           n_prev_thumbs_updown - ifelse(is.na(n_out7_thumbs_updown),
                                         0,
                                         n_out7_thumbs_updown),
         prop_window_thumbs_updown =
           ifelse(n_window_sent_have_response==0, 0,
                  n_window_thumbs_updown / n_window_sent_have_response),
         n_window_active_have_response = 
           n_prev_active_have_response - ifelse(is.na(n_out7_active_have_response),
                                              0,
                                              n_out7_active_have_response),
         n_window_active = 
           n_prev_active - ifelse(is.na(n_out7_active), 0,
                                  n_out7_active),
         n_window_active_thumbs_updown =
           n_prev_active_thumbs_updown - ifelse(is.na(n_out7_active_thumbs_updown),
                                         0,
                                         n_out7_active_thumbs_updown),
         prop_window_active_thumbs_updown =
           ifelse(n_window_active_have_response==0, 0,
                  n_window_active_thumbs_updown / n_window_active_have_response))

## Exponential weighting of proportion
suggest.analysis <- 
  suggest.analysis %>%
  group_by(user) %>%
  mutate(dose_sent_10points = 
           rollapply(data=c(0, send[-n()]), width=10, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_15points = 
           rollapply(data=c(0, send[-n()]), width=15, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_21points = 
           rollapply(data=c(0, send[-n()]), width=21, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_5points = 
           rollapply(data=c(0, send[-n()]), width=5, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_20points = 
           rollapply(data=c(0, send[-n()]), width=20, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_25points = 
           rollapply(data=c(0, send[-n()]), width=25, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_30points = 
           rollapply(data=c(0, send[-n()]), width=30, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_35points = 
           rollapply(data=c(0, send[-n()]), width=35, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_40points = 
           rollapply(data=c(0, send[-n()]), width=40, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_45points = 
           rollapply(data=c(0, send[-n()]), width=45, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_50points = 
           rollapply(data=c(0, send[-n()]), width=50, FUN=sum,
                     align='right', fill=0, partial=TRUE),
         dose_sent_100points = 
           rollapply(data=c(0, send[-n()]), width=100, FUN=sum,
                     align='right', fill=0, partial=TRUE)
         ) %>% ungroup

# Total step count from previous 7 days, excluding current day
# NA daily step counts add 0 to the moving-window step count
suggest.analysis <-
  suggest.analysis %>%
  left_join(
    daily %>%
      filter(!is.na(study.day.nogap))%>%
      mutate(jbsteps.direct.NA0 =ifelse(is.na(jbsteps.direct),0,jbsteps.direct)) %>%
      group_by(user) %>%
      mutate(cumsteps = cumsum(jbsteps.direct.NA0),
             steps.window7 = cumsteps - lag(cumsteps, 7, default=0),
             steps.yesterday = jbsteps.direct.NA0,
             study.day.plus1 = study.day.nogap + 1) %>% ungroup %>%
      select(user, steps.window7, study.day.plus1, steps.yesterday),
    by=c('user'='user','study.day.nogap'='study.day.plus1') 
  ) %>%
  left_join(
    daily %>% 
      select(user,study.day.nogap,daily.jbsteps.direct=jbsteps.direct) %>%
      mutate(daily.jbsteps.direct.NA0 = 
               ifelse(is.na(daily.jbsteps.direct), 0, daily.jbsteps.direct)) %>% 
      filter(!is.na(study.day.nogap))%>%
      group_by(user) %>%
      mutate(daily.csteps.direct.NA0 = cumsum(daily.jbsteps.direct.NA0),
             daily.steps.exp4 = 
               c(0,as.numeric(fitted.values(ses(daily.jbsteps.direct.NA0,
                                            initial='simple',alpha=0.4)))[-n()]),
             daily.sqrt.steps.exp4 = 
               c(0,as.numeric(fitted.values(ses(sqrt(daily.jbsteps.direct.NA0),
                                                initial='simple',alpha=0.4)))[-n()]),
             daily.sqrt.steps.exp8 = 
               c(0,as.numeric(fitted.values(ses(sqrt(daily.jbsteps.direct.NA0),
                                                initial='simple',alpha=0.8)))[-n()])) %>% ungroup,
    by=c('user'='user','study.day.nogap'='study.day.nogap'))
suggest.analysis$steps.window7[suggest.analysis$study.day.nogap==0] <- 0
suggest.analysis$steps.yesterday[suggest.analysis$study.day.nogap==0] <- 0
suggest.analysis$steps.yesterday.sqrt <- sqrt(suggest.analysis$steps.yesterday)
suggest.analysis$steps.window7.avg <- 
  with(suggest.analysis, steps.window7 / ifelse(study.day.nogap < 7, study.day.nogap, 7))
suggest.analysis$steps.window7.avg[suggest.analysis$study.day.nogap==0] <- 0
suggest.analysis$steps.window7.log <- with(suggest.analysis, log(steps.window7 +0.5))
suggest.analysis$steps.window7.log.avg <- with(suggest.analysis, log(steps.window7.avg + 0.5))
suggest.analysis$steps.window7.sqrt.avg <- with(suggest.analysis, sqrt(steps.window7.avg))
suggest.analysis$sqrt.steps.window7 <- with(suggest.analysis, sqrt(steps.window7))

## Variance of 1-hour centered window from previous 7 days
suggest.analysis <- 
  suggest.analysis %>% 
  mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
         slot.steps60.prepost.log = log(slot.steps60.prepost.zero + 0.5)) %>%
  left_join(
    suggest.analysis %>%
      mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
             slot.steps60.prepost.log = log(slot.steps60.prepost.zero + 0.5)) %>%
      group_by(user, slot) %>%
      mutate(window7.steps60.var = rollapply(slot.steps60.prepost.zero,
                                             width=7, FUN=var, align='right',fill=NA),
             window7.steps60.log.var = rollapply(slot.steps60.prepost.log,
                                                 width=7, FUN=var, align='right',fill=NA),
             study.day.nogap.plus1 = study.day.nogap + 1) %>%
      select(user, study.day.nogap.plus1, slot, window7.steps60.var,
             window7.steps60.log.var) %>%
      ungroup,
    by=c('user'='user','study.day.nogap'='study.day.nogap.plus1',
         'slot'='slot')
  ) %>%
  mutate(window7.steps60.sd = sqrt(window7.steps60.var),
         window7.steps60.log.sd = sqrt(window7.steps60.log.var))



###
### WEEKDAY DATA FRAME
###

daily$weekendTrue <- with(daily, strftime(study.date, '%u') %in% c(6,7))

# Steps total from previous 5 weekdays
#suggest.analysis.wdays <-
#  suggest.analysis.wdays %>%
#  left_join(
#    daily %>%
#      filter(!is.na(study.day.nogap), !weekendTrue) %>%
#      mutate(jbsteps.direct.NA0 = ifelse(is.na(jbsteps.direct),0,jbsteps.direct)) %>%
#      group_by(user) %>%
#      mutate(study.weekday.nogap = 0:(n()-1)) %>% 
#      mutate(cumsteps = cumsum(jbsteps.direct.NA0),
#             steps.window5 = cumsteps - lag(cumsteps, 5, default=0),
#             study.weekday.plus1 = study.weekday.nogap + 1) %>% ungroup %>%
#      select(user, steps.window5, study.weekday.plus1),
#    by=c('user'='user','study.weekday.nogap'='study.weekday.plus1') 
#  ) %>% ungroup
#suggest.analysis.wdays$steps.window5[suggest.analysis.wdays$study.weekday.nogap==0] <- 0
#suggest.analysis.wdays$steps.window5.avg <- 
#  with(suggest.analysis.wdays, steps.window5 / ifelse(study.weekday.nogap < 5, study.weekday.nogap, 5))
#suggest.analysis.wdays$steps.window5.avg[suggest.analysis.wdays$study.weekday.nogap==0] <- 0
#suggest.analysis.wdays$steps.window5.log <- with(suggest.analysis.wdays, log(steps.window5 +0.5))
#suggest.analysis.wdays$steps.window5.log.avg <- with(suggest.analysis.wdays, log(steps.window5.avg + 0.5))
#suggest.analysis.wdays$steps.window5.sqrt.avg <- with(suggest.analysis.wdays, sqrt(steps.window5.avg))

#suggest.analysis.wdays <-
#  suggest.analysis.wdays %>%
#  left_join(
    #daily %>%#
##      filter(!is.na(study.day.nogap), !weekendTrue) %>% 
#      mutate(daily.jbsteps.direct.NA0 = 
#               ifelse(is.na(jbsteps.direct), 0, jbsteps.direct)) %>% 
#      group_by(user) %>%
#      mutate(study.weekday.nogap = 0:(n()-1)) %>% 
#      mutate(daily.csteps.direct.NA0 = cumsum(daily.jbsteps.direct.NA0))%>%
#      ungroup %>%
#      select(user, study.weekday.nogap, daily.jbsteps.direct.NA0),
#    by=c('user'='user','study.weekday.nogap'='study.weekday.nogap')) %>% ungroup


## Variance from previous 5 weekdays
# suggest.analysis.wdays <- 
#   suggest.analysis.wdays %>%
#   mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
#          slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
#   left_join(
#     suggest.analysis.wdays %>%
#       mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
#              slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
#       group_by(user, slot) %>%
#       mutate(window5.steps60.var = rollapply(slot.steps60.prepost.zero,
#                                              width=5, FUN=var, align='right',fill=NA),
#              window5.steps60.log.var = rollapply(slot.steps60.prepost.log,
#                                                  width=5, FUN=var, align='right',fill=NA),
#              study.weekday.nogap.plus1 = study.weekday.nogap + 1) %>%
#       select(user, study.weekday.nogap.plus1, slot, window5.steps60.var,
#              window5.steps60.log.var) %>%
#       ungroup,
#     by=c('user'='user','study.weekday.nogap'='study.weekday.nogap.plus1',
#          'slot'='slot')
#   ) %>%
#   mutate(window5.steps60.sd = sqrt(window5.steps60.var),
#          window5.steps60.log.sd = sqrt(window5.steps60.log.var))


# Get the cumulative number of sent messages with a response from 5 weekdays
# before (take the count at the end of the day, so the window slides on the day scale) 
# suggest.analysis.wdays <-
#   suggest.analysis.wdays %>%
#   left_join( 
#     suggest.analysis.wdays %>%
#       filter(slot==1) %>%
#       mutate(study.weekday.plus4 = study.weekday.nogap + ndays_window_thumbs_wdays - 1,
#              n_out5_sent_have_response = n_prev_sent_have_response,
#              n_out5_active_have_response = n_prev_active_have_response,
#              n_out5_thumbs_updown = n_prev_thumbs_updown,
#              n_out5_active_thumbs_updown = n_prev_active_thumbs_updown) %>%
#       select(user, study.weekday.plus4, n_out5_sent_have_response, n_out5_thumbs_updown, 
#              n_out5_active_have_response, n_out5_active_thumbs_updown),
#     by=c('user'='user','study.weekday.nogap'='study.weekday.plus4')
#   ) %>%
#   mutate(n_window_sent_have_response = 
#            n_prev_sent_have_response - ifelse(is.na(n_out5_sent_have_response),
#                                               0,
#                                               n_out5_sent_have_response),
#          n_window_thumbs_updown =
#            n_prev_thumbs_updown - ifelse(is.na(n_out5_thumbs_updown),
#                                          0,
#                                          n_out5_thumbs_updown),
#          prop_window_thumbs_updown =
#            ifelse(n_window_sent_have_response==0, 0,
#                   n_window_thumbs_updown / n_window_sent_have_response),
#          n_window_active_have_response = 
#            n_prev_active_have_response - ifelse(is.na(n_out5_active_have_response),
#                                               0,
#                                               n_out5_active_have_response),
#          n_window_active_thumbs_updown =
#            n_prev_active_thumbs_updown - ifelse(is.na(n_out5_active_thumbs_updown),
#                                          0,
#                                          n_out5_active_thumbs_updown),
#          prop_window_active_thumbs_updown =
#            ifelse(n_window_active_have_response==0, 0,
#                   n_window_active_thumbs_updown / n_window_active_have_response))