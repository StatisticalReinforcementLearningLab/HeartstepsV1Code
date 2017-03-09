# need to have suggest data frame in workspace
library(dplyr)
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
  melt(id='user') %>% group_by(user) %>%
  summarise(selfeff_sum = sum(value))
suggest.analysis <-
  suggest.analysis %>%
  left_join(user_selfeff, by='user')

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
                                ifelse(location.category=='home','home','other'))) 
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
         n_prev_active_have_response = 
           cumsum(c(0,(send.active & have_thumbs)[-n()])),
         n_prev_thumbs_updown =  # sedentary AND active messages
           cumsum(c(0,(send & have_thumbs & thumbs_up_or_down)[-n()])),
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
             n_out7_thumbs_updown = n_prev_thumbs_updown,
             n_out7_active_have_response = n_prev_active_have_response,
             n_out7_active_thumbs_updown = n_prev_active_thumbs_updown) %>%
      select(user, study.day.plus6, n_out7_sent_have_response,
             n_out7_thumbs_updown, n_out7_active_have_response, n_out7_active_thumbs_updown),
    by=c('user'='user','study.day.nogap'='study.day.plus6')
  ) %>%
  mutate(n_window_sent_have_response = 
           n_prev_sent_have_response - ifelse(is.na(n_out7_sent_have_response),
                                              0,
                                              n_out7_sent_have_response),
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
  mutate(prop_window_thumbs_exp05 = as.numeric(fitted.values(ses(prop_window_thumbs_updown,
                                        initial='simple', alpha=0.05))),
         prop_window_thumbs_exp1 = as.numeric(fitted.values(ses(prop_window_thumbs_updown,
                                                                initial='simple', alpha=0.1))),
         prop_window_active_thumbs_exp05 = as.numeric(fitted.values(ses(prop_window_active_thumbs_updown,
                                                                 initial='simple', alpha=0.05))),
         prop_window_active_thumbs_exp1 = as.numeric(fitted.values(ses(prop_window_active_thumbs_updown,
                                                                initial='simple', alpha=0.1))))

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
             study.day.plus1 = study.day.nogap + 1) %>% ungroup %>%
      select(user, steps.window7, study.day.plus1),
    by=c('user'='user','study.day.nogap'='study.day.plus1') 
  ) %>%
  left_join(
    daily %>% 
      select(user,study.day.nogap,daily.jbsteps.direct=jbsteps.direct) %>%
      mutate(daily.jbsteps.direct.NA0 = 
               ifelse(is.na(daily.jbsteps.direct), 0, daily.jbsteps.direct)) %>% 
      filter(!is.na(study.day.nogap))%>%
      group_by(user) %>%
      mutate(daily.csteps.direct.NA0 = cumsum(daily.jbsteps.direct.NA0)) %>% ungroup,
    by=c('user'='user','study.day.nogap'='study.day.nogap'))
suggest.analysis$steps.window7[suggest.analysis$study.day.nogap==0] <- 0
suggest.analysis$steps.window7.avg <- 
  with(suggest.analysis, steps.window7 / ifelse(study.day.nogap < 7, study.day.nogap, 7))
suggest.analysis$steps.window7.avg[suggest.analysis$study.day.nogap==0] <- 0
suggest.analysis$steps.window7.log <- with(suggest.analysis, log(steps.window7 +0.5))
suggest.analysis$steps.window7.log.avg <- with(suggest.analysis, log(steps.window7.avg + 0.5))
suggest.analysis$steps.window7.sqrt.avg <- with(suggest.analysis, sqrt(steps.window7.avg))

## Variance of 1-hour centered window from previous 7 days
suggest.analysis <- 
  suggest.analysis %>% 
  mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
         slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
  left_join(
    suggest.analysis %>%
      mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
             slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
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

# Exponential weighting of past 7-day average step count
suggest.analysis <- 
  suggest.analysis %>% 
  group_by(user) %>%
  mutate(steps.window7.avg.exp05= as.numeric(fitted.values(ses(steps.window7.avg,
                                                               initial='simple', alpha=0.05))),
         steps.window7.avg.exp1= as.numeric(fitted.values(ses(steps.window7.avg,
                                                              initial='simple', alpha=0.1))),
         steps.window7.log.avg.exp05 = as.numeric(fitted.values(ses(steps.window7.log.avg,
                                                                    initial='simple', alpha=0.05))),
         steps.window7.log.avg.exp1 = as.numeric(fitted.values(ses(steps.window7.log.avg,
                                                                   initial='simple', alpha=0.1))),
         steps.window7.sqrt.avg.exp05 = as.numeric(fitted.values(ses(steps.window7.sqrt.avg,
                                                                    initial='simple', alpha=0.05))),
         steps.window7.sqrt.avg.exp1 = as.numeric(fitted.values(ses(steps.window7.sqrt.avg,
                                                                   initial='simple', alpha=0.1))))

# Exponential weighting of past 7-day standard deviation
suggest.analysis <-
  suggest.analysis %>%
  left_join(
    suggest.analysis %>% 
      filter(!is.na(window7.steps60.sd))%>%
      group_by(user) %>%
      mutate(window7.steps60.sd.exp05 = as.numeric(fitted.values(ses(window7.steps60.sd,
                                                                     initial='simple', alpha=0.05))),
             window7.steps60.sd.exp1 = as.numeric(fitted.values(ses(window7.steps60.sd,
                                                                    initial='simple', alpha=0.1))),
             window7.steps60.log.sd.exp1 = as.numeric(fitted.values(ses(window7.steps60.log.sd,
                                                                        initial='simple', alpha=0.1))),
             window7.steps60.log.sd.exp05 = as.numeric(fitted.values(ses(window7.steps60.log.sd,
                                                                         initial='simple', alpha=0.05)))) %>%
      ungroup %>%
      select(user, decision.index.nogap, window7.steps60.sd.exp05,
             window7.steps60.sd.exp1,window7.steps60.log.sd.exp1,window7.steps60.log.sd.exp05),
    by=c('user'='user','decision.index.nogap'='decision.index.nogap')
  )

###
### WEEKDAY DATA FRAME
###

daily$weekendTrue <- with(daily, strftime(study.date, '%u') %in% c(6,7))

# Steps total from previous 5 weekdays
suggest.analysis.wdays <-
  suggest.analysis.wdays %>%
  left_join(
    daily %>%
      filter(!is.na(study.day.nogap), !weekendTrue) %>%
      mutate(jbsteps.direct.NA0 = ifelse(is.na(jbsteps.direct),0,jbsteps.direct)) %>%
      group_by(user) %>%
      mutate(study.weekday.nogap = 0:(n()-1)) %>% 
      mutate(cumsteps = cumsum(jbsteps.direct.NA0),
             steps.window5 = cumsteps - lag(cumsteps, 5, default=0),
             study.weekday.plus1 = study.weekday.nogap + 1) %>% ungroup %>%
      select(user, steps.window5, study.weekday.plus1),
    by=c('user'='user','study.weekday.nogap'='study.weekday.plus1') 
  ) %>% ungroup
suggest.analysis.wdays$steps.window5[suggest.analysis.wdays$study.weekday.nogap==0] <- 0
suggest.analysis.wdays$steps.window5.avg <- 
  with(suggest.analysis.wdays, steps.window5 / ifelse(study.weekday.nogap < 5, study.weekday.nogap, 5))
suggest.analysis.wdays$steps.window5.avg[suggest.analysis.wdays$study.weekday.nogap==0] <- 0
suggest.analysis.wdays$steps.window5.log <- with(suggest.analysis.wdays, log(steps.window5 +0.5))
suggest.analysis.wdays$steps.window5.log.avg <- with(suggest.analysis.wdays, log(steps.window5.avg + 0.5))
suggest.analysis.wdays$steps.window5.sqrt.avg <- with(suggest.analysis.wdays, sqrt(steps.window5.avg))

suggest.analysis.wdays <-
  suggest.analysis.wdays %>%
  left_join(
    daily %>%
      filter(!is.na(study.day.nogap), !weekendTrue) %>% 
      mutate(daily.jbsteps.direct.NA0 = 
               ifelse(is.na(jbsteps.direct), 0, jbsteps.direct)) %>% 
      group_by(user) %>%
      mutate(study.weekday.nogap = 0:(n()-1)) %>% 
      mutate(daily.csteps.direct.NA0 = cumsum(daily.jbsteps.direct.NA0))%>%
      ungroup %>%
      select(user, study.weekday.nogap, daily.jbsteps.direct.NA0),
    by=c('user'='user','study.weekday.nogap'='study.weekday.nogap')) %>% ungroup


## Variance from previous 5 weekdays
suggest.analysis.wdays <- 
  suggest.analysis.wdays %>%
  mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
         slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
  left_join(
    suggest.analysis.wdays %>%
      mutate(slot.steps60.prepost.zero = jbsteps30pre.zero + jbsteps30.zero,
             slot.steps60.prepost.log = jbsteps30pre.log + jbsteps30.log) %>%
      group_by(user, slot) %>%
      mutate(window5.steps60.var = rollapply(slot.steps60.prepost.zero,
                                             width=5, FUN=var, align='right',fill=NA),
             window5.steps60.log.var = rollapply(slot.steps60.prepost.log,
                                                 width=5, FUN=var, align='right',fill=NA),
             study.weekday.nogap.plus1 = study.weekday.nogap + 1) %>%
      select(user, study.weekday.nogap.plus1, slot, window5.steps60.var,
             window5.steps60.log.var) %>%
      ungroup,
    by=c('user'='user','study.weekday.nogap'='study.weekday.nogap.plus1',
         'slot'='slot')
  ) %>%
  mutate(window5.steps60.sd = sqrt(window5.steps60.var),
         window5.steps60.log.sd = sqrt(window5.steps60.log.var))

# Exponential weighting of past 7-day average step count
suggest.analysis.wdays <- 
  suggest.analysis.wdays %>% 
  group_by(user) %>%
  mutate(steps.window5.avg.exp05= as.numeric(fitted.values(ses(steps.window5.avg,
                                                               initial='simple', alpha=0.05))),
         steps.window5.avg.exp1= as.numeric(fitted.values(ses(steps.window5.avg,
                                                              initial='simple', alpha=0.1))),
         steps.window5.log.avg.exp05 = as.numeric(fitted.values(ses(steps.window5.log.avg,
                                                                    initial='simple', alpha=0.05))),
         steps.window5.log.avg.exp1 = as.numeric(fitted.values(ses(steps.window5.log.avg,
                                                                   initial='simple', alpha=0.1))))

# Exponential weighting of past 5-day standard deviation
suggest.analysis.wdays <-
  suggest.analysis.wdays %>%
  left_join(
    suggest.analysis.wdays %>% 
      filter(!is.na(window5.steps60.sd))%>%
      group_by(user) %>%
      mutate(window5.steps60.sd.exp05 = as.numeric(fitted.values(ses(window5.steps60.sd,
                                                                     initial='simple', alpha=0.05))),
             window5.steps60.sd.exp1 = as.numeric(fitted.values(ses(window5.steps60.sd,
                                                                    initial='simple', alpha=0.1))),
             window5.steps60.log.sd.exp1 = as.numeric(fitted.values(ses(window5.steps60.log.sd,
                                                                        initial='simple', alpha=0.1))),
             window5.steps60.log.sd.exp05 = as.numeric(fitted.values(ses(window5.steps60.log.sd,
                                                                         initial='simple', alpha=0.05)))) %>%
      ungroup %>%
      select(user, decision.index.nogap, window5.steps60.sd.exp05,
             window5.steps60.sd.exp1,window5.steps60.log.sd.exp1,window5.steps60.log.sd.exp05),
    by=c('user'='user','decision.index.nogap'='decision.index.nogap')
  )

# Get the cumulative number of sent messages with a response from 5 weekdays
# before (take the count at the end of the day, so the window slides on the day scale) 
suggest.analysis.wdays <-
  suggest.analysis.wdays %>%
  left_join( 
    suggest.analysis.wdays %>%
      filter(slot==1) %>%
      mutate(study.weekday.plus4 = study.weekday.nogap + ndays_window_thumbs_wdays - 1,
             n_out5_sent_have_response = n_prev_sent_have_response,
             n_out5_active_have_response = n_prev_active_have_response,
             n_out5_thumbs_updown = n_prev_thumbs_updown,
             n_out5_active_thumbs_updown = n_prev_active_thumbs_updown) %>%
      select(user, study.weekday.plus4, n_out5_sent_have_response, n_out5_thumbs_updown, 
             n_out5_active_have_response, n_out5_active_thumbs_updown),
    by=c('user'='user','study.weekday.nogap'='study.weekday.plus4')
  ) %>%
  mutate(n_window_sent_have_response = 
           n_prev_sent_have_response - ifelse(is.na(n_out5_sent_have_response),
                                              0,
                                              n_out5_sent_have_response),
         n_window_thumbs_updown =
           n_prev_thumbs_updown - ifelse(is.na(n_out5_thumbs_updown),
                                         0,
                                         n_out5_thumbs_updown),
         prop_window_thumbs_updown =
           ifelse(n_window_sent_have_response==0, 0,
                  n_window_thumbs_updown / n_window_sent_have_response),
         n_window_active_have_response = 
           n_prev_active_have_response - ifelse(is.na(n_out5_active_have_response),
                                              0,
                                              n_out5_active_have_response),
         n_window_active_thumbs_updown =
           n_prev_active_thumbs_updown - ifelse(is.na(n_out5_active_thumbs_updown),
                                         0,
                                         n_out5_active_thumbs_updown),
         prop_window_active_thumbs_updown =
           ifelse(n_window_active_have_response==0, 0,
                  n_window_active_thumbs_updown / n_window_active_have_response))

## Exponential weighting of proportion (weekdays)
suggest.analysis.wdays <- 
  suggest.analysis.wdays %>%
  group_by(user) %>%
  mutate(prop_window_thumbs_exp05 = as.numeric(fitted.values(ses(prop_window_thumbs_updown,
                                                                 initial='simple', alpha=0.05))),
         prop_window_thumbs_exp1 = as.numeric(fitted.values(ses(prop_window_thumbs_updown,
                                                                initial='simple', alpha=0.1))),
         prop_window_active_thumbs_exp05 = as.numeric(fitted.values(ses(prop_window_active_thumbs_updown,
                                                                 initial='simple', alpha=0.05))),
         prop_window_active_thumbs_exp1 = as.numeric(fitted.values(ses(prop_window_active_thumbs_updown,
                                                                initial='simple', alpha=0.1)))) %>%
  ungroup