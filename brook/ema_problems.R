library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(grid)
library(reshape2)
### 
### Creates plots of EMA issues
### 

## To do: 
## use daily$connect to count instances without any EMA information
## count number of days with engagement records received before notificaiton records
## create .Rnw and .pdf uploaded to box folder

###
# user.ema.problems 
# has one row per user-ema.date combination for every ema date with at least one 
# notification, engagement, or response record

#
if (!exists('sys.var')) {source("init.R")}
load(paste(sys.var$mbox.data,'csv.RData',sep=''))
load(paste(sys.var$mbox.data,"analysis.RData",sep=''))

keep_users <- users$user[!users$exclude]
daily.analysis <- filter(daily, user %in% keep_users & !travel)
daily.analysis <- filter(daily.analysis, study.day.nogap <=41) # last study day is 41
# Count each type of EMA record (engagement, notification, response)
daily.analysis %>%
  left_join(
    select(notify, user, ema.date) %>% 
      group_by(user, ema.date) %>% 
      summarise(n_notify=n()),
    by=c('user'='user','study.date'='ema.date')
  ) %>%
  left_join(
    select(engage, user, ema.date) %>% group_by(user, ema.date) %>%
      summarise(n_engage = n()),
    by=c('user'='user','study.date'='ema.date')
  ) %>%
  left_join(
    select(ema, user, ema.date) %>% group_by(user, ema.date) %>%
      summarise(n_response = n()),
    by=c('user'='user','study.date'='ema.date')
  ) %>% filter(study.date <= last.date) %>%
  mutate_each(funs(ifelse(is.na(.), 0, .)), contains("n_")) -> daily.analysis


# Days without any EMA records
daily.analysis %>%
  group_by(user) %>%
  summarise(n_days = n(),
            n_noEMA = sum(!connect), # connect indicates day without EMA information
            prop_noEMA = n_noEMA / n_days) -> user.missing.ema

# Days with responses but zero engagements
daily.analysis %>%
  group_by(user) %>%
  summarise(n_days = n(),
            n_resp_no_engage = sum(n_engage==0 & n_response > 0),
            prop_resp_no_engage = n_resp_no_engage / n_days) -> user.resp.no.engage
  
# Plot the proportion of days without any EMA records
user.missing.ema %>% 
  arrange(prop_noEMA) %>% ungroup %>%
  mutate(user = factor(user, 
                       levels = user,
                       labels=paste(c('User ', rep('',length(user)-2),
                                            'User '), user, sep=''))) %>%
  ggplot(aes(x=prop_noEMA, y=user)) +
  geom_segment(aes(xend=0, yend=user), linetype='dotted',
               color='grey') +
  geom_point() + 
  theme_bw() + theme(panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.grid.major.x=element_line(linetype='dashed')) +
  ggtitle('Proportion of days without any EMA records\n(notification, response, engagement, planning)')+
  xlab('') +
  ylab('') -> plot.missing.ema
  
# Plot the proportion of days with no engagments but >0 responses
user.resp.no.engage %>%
  arrange(prop_resp_no_engage) %>% ungroup %>%
  mutate(user = factor(user, 
                       levels = user,
                       labels=paste(c('User ', rep('',length(user)-2),
                                      'User '), user, sep=''))) %>%
  ggplot(aes(x=prop_resp_no_engage, y=user))  +
  geom_segment(aes(xend=0, yend=user), linetype='dotted',
               color='grey') +
  geom_point() + 
  theme_bw() + theme(panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.grid.major.x=element_line(linetype='dashed')) +
  ggtitle('Proportion of days with 0 engagement records\nbut at least one response') +
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8)) +
  xlab('') + ylab('') -> plot.resp.no.engage
  
# Compile timestamps for notifications, engagements and responses
bind_rows(Notification = select(notify, user, ema.date, utime = notified.utime),
          Engagement = select(engage, user, ema.date, utime = engaged.utime),
          Response = select(ema, user, ema.date, utime = utime.stamp),
          .id = 'Type') %>% 
  left_join(select(daily.analysis, user, study.date, study.day, last.date)
            , by=c('user' = 'user', 'ema.date' = 'study.date')) %>%
  arrange(user, ema.date, utime) %>% 
  filter(user %in% users$user[!users$exclude], ema.date <= last.date) %>%
  mutate(utime_inday = as.POSIXct(format(utime, "%H:%M:%S"),
                                  format="%H:%M:%S")) -> user.ema.times
user.ema.times %>% 
  mutate(utime_instudy = utime_inday + study.day * 24 * 60 * 60) -> user.ema.times

# Count each type of record
user.ema.times %>% group_by(user, ema.date) %>%
  summarise(n_engage = sum(Type=='Engagement'), 
            n_notify = sum(Type=='Notification'),
            n_response = sum(Type=='Response'),
            utime_first_engage = min(utime[which(Type=='Engagement')]),
            utime_last_engage = max(utime[which(Type=='Engagement')]),
            utime_first_notify = min(utime[which(Type=='Notification')]),
            utime_first_respond = min(utime[which(Type=='Response')]),
            utime_last_respond = max(utime[which(Type=='Response')]),
            n_engage_bt_notif_resp = sum( 
              utime[which(Type=='Engagement')] >= utime_first_notify &
                utime[which(Type=='Engagement')] <= utime_first_respond
              ),
            n_engage_post_respond = length(utime[ # engagements after last response
              which(Type =='Engagement' & utime > utime_last_respond & 
                      n_response >0)
            ]),
            study.day = unique(study.day)) -> user.ema.problems

# Time between notification and response, 
# on days when there are responses and
# there is a notification
daily.analysis %>%
  left_join(
    filter(user.ema.problems, n_notify > 0 & n_response > 0) %>%
      mutate(time_bt_notify_respond = as.difftime(utime_first_respond - utime_first_notify,
                                                  units='mins')) %>%
      select(user, ema.date, time_bt_notify_respond),
    by = c('user'='user', 'study.date'='ema.date')
  ) -> daily.analysis

select(daily.analysis, user, study.date, n_notify, 
       n_response, time_bt_notify_respond) %>%
  filter(n_notify >0 & n_response > 0) %>%
  group_by(user) %>% summarise(mean_time_resp = mean(time_bt_notify_respond)) %>%
  arrange(mean_time_resp) %>%
  mutate(user = factor(user, levels=unique(user),
                       labels=paste(c(rep('',length(unique(user))-1), 'User '),
                                      unique(user), sep=''))) %>% 
  ggplot(aes(x=as.numeric(mean_time_resp), y=user)) +
  geom_segment(aes(xend=0, yend=user), linetype='dotted',
             color='grey') +
  geom_point() + 
  theme_bw() + theme(panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.grid.major.x=element_line(linetype='dashed')) +
  xlab('Average seconds between\nnotification and first response') +
  ylab('') -> plot.user.resp.time
  
# Fix the n_engage count
# Responses but no engagement records should count as an engagement
user.ema.problems %>% ungroup %>%
       mutate(n_engage_fixed = ifelse(
                              n_response > 0 & n_engage_bt_notif_resp == 0
                               , n_engage + 1, n_engage) - 
                            n_engage_post_respond) -> user.ema.problems

# Function to plot timeline of notifcation, engagement, response 
# for each user-day
plot_ema_times <- function(dat, facet = TRUE, count = TRUE){
  dat %>% ungroup %>%
  mutate(user = factor(user, levels=unique(user), 
                       labels=paste('User',unique(user))),
         study.day = factor(study.day, levels=unique(study.day), 
                            labels=paste('Study day',unique(study.day))),
         Type = factor(Type, levels=c('Notification','Engagement','Response'))) -> dat
  dat %>%
  ggplot(aes(x=as.POSIXlt(utime_inday, 'EST'), y=user)) +
    geom_point(aes(shape=Type, color=Type),
               position=position_jitter(height=0.2, width=0)) -> ret
  if (count){
    ret <- ret +
        geom_label(aes(label=n_engage_fixed,
                  x=as.POSIXct(format(utime_plot_engage, "%H:%M:%S"),
                                                     format="%H:%M:%S"),
                  y=user),
               size=3, hjust=1, vjust = 1, label.padding=unit(0.15, 'lines'),
               nudge_y=-0.25,
              data = dat %>% group_by(user, study.day) %>%
                summarise(n_engage_fixed = unique(n_engage_fixed),
                          utime_plot_engage = max(utime)))
    }
  ret <- ret +
    theme(strip.background=element_rect(fill=NA),
          panel.background=element_rect(fill=NA),
          panel.grid.major.y=element_line(color='grey',linetype='dashed'),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.border=element_rect(fill=NA, color='grey'),
          legend.position='bottom',
           legend.direction='horizontal') +
    xlab('') + ylab('')+ 
    scale_x_datetime(date_labels = '%H:%M:%S',
                      breaks = function(x) return(c(min(x),max(x)))) +
    scale_color_manual(values = setNames(brewer.pal(3,name='Set1'),
                                         c('Notification','Engagement', 'Response')),
                       name='') + 
    scale_shape_manual(name = '',
                       values = c('Notification'=0, 'Engagement'=16,'Response'=2)) -> ret
  
  if (facet){
    ret <- ret + facet_wrap(~study.day, scales='free')
  }  
  return(ret)
}


# Demonstration plot of different engagement scenarios
user.ema.problems %>% filter(study.day %in% c(14,25,1,29,11,12) &
                               user %in% c(46,35,4,14,28)) %>%
  inner_join(select(user.ema.times, -study.day), by=c('user','ema.date')) %>%
  plot_ema_times(count=FALSE) -> plot.eng.scenarios

filter(user.ema.problems, n_notify >0 & utime_first_engage < utime_first_notify) %>% 
  inner_join(select(user.ema.times, -study.day), by=c('user','ema.date')) %>%
  plot_ema_times(count=FALSE) -> plot.engage.before.notify

# Sort users by mean number of engagements per EMA
with(user.ema.problems %>% group_by(user) %>%
  summarise(mean_engagements = mean(n_engage_fixed)) %>%
  arrange(desc(mean_engagements)),
  user) -> user.byengage.fixed

# Histogram of engagement frequency using updated count
user.ema.problems %>% 
  select(user, study.day, n_engage_fixed, n_response) %>% 
  mutate(responded = n_response >0) %>%
  group_by(user, n_engage_fixed, responded) %>% summarise(count= n()) %>% 
  group_by(user) %>% mutate(count_all = sum(count)) %>%
  mutate(rfreq = count / count_all) %>%
  ungroup %>% mutate(
    user = factor(user, levels=rev(user.byengage.fixed),
                  labels=paste(c(rep('', length(unique(user))-1), 'User '),
                               rev(user.byengage.fixed), sep=''))
  ) %>% 
  ggplot(aes(x=n_engage_fixed, y=rfreq)) +
  geom_bar(aes(fill=responded), stat='identity',
           color=NA) + 
  facet_wrap(~user, nrow = 5) + xlab('Engagements') +
  ylab('Proportion of EMAs') + 
  scale_y_continuous(breaks=c(0,0.5,1)) +
  scale_x_continuous(breaks=c(0,2,8)) +
  scale_fill_manual(values=c('FALSE'='grey', 'TRUE'='darkgreen'),
                     breaks=c('FALSE','TRUE'),
                     labels=c('No responses','At least 1 response'),
                     name='') +
  theme(panel.grid=element_blank(),
        strip.background=element_rect(fill=NA, color='white'),
        panel.background=element_rect(color='grey', fill=NA),
        strip.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.line=element_blank(),
        legend.position=c(1,0),
        legend.direction='vertical',
        legend.justification=c(1,0)) +
  ggtitle('EMA engagements\nby user') -> plot.hist.eng.fixed

# Plot of average engagements over time (day in study)
# Probably incorrect, should use study.day.nogap
user.ema.problems %>% 
  ggplot(aes(x=study.day, y=n_engage_fixed)) + 
  stat_summary(geom='line', fun.y=mean) +
  theme(strip.background=element_rect(fill=NA),
      panel.background=element_rect(fill=NA, color='grey'),
      panel.grid=element_blank(),
      axis.line=element_line(color='grey',size=0.25)) +
  scale_y_continuous(breaks=c(0,0.5,1,1.5)) +
  xlab('Day in study') + ylab("Average # engagements") -> plot.avg.eng.time.fixed

select(daily.analysis, user, study.day.nogap) %>% 
  group_by(study.day.nogap) %>% 
  summarise(n_users = n()) %>%
  ggplot(aes(x=study.day.nogap, y=n_users)) + 
  geom_line() +
  theme(strip.background=element_rect(fill=NA),
      panel.background=element_rect(fill=NA, color='grey'),
      panel.grid=element_blank(),
      axis.line=element_line(color='grey',size=0.25)) +
  scale_x_continuous(breaks=c(0,25,50,75)) +
  scale_y_continuous(breaks=c(0,15,30,45), labels=c(' 0',' 15', ' 30', ' 45')) +
  xlab('Day in study') + ylab('Users remaining') -> plot.nusers

