library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(grid)

### ema_plots.R
### Author: Brook Luers
### Plots the frequency of EMA engagements using the 'engage' data frame
### Creates 3 PDF files:
###     freq_engagements_heat.pdf   heatmap of relative freq. of engagements by user
###     freq_engagements_hist.pdf   histogram of the same
###     avg_engagements_time.pdf    average engagements by day 
###                                 of study (averaged over users)
### These plots assume that rows in EMA_Context_Engaged (data frame 'engage')
###     are populated correctly, and that a missing row in this table indicates
###     a lack of engagement for that user-ema. 

source("init.R")
setwd(sys.var$mbox.data)
load("csv.RData")
load("analysis.RData")
setwd(sys.var$repo)

eng_lab <- "'Engagements'\n(rows in EMA_Context_Engaged)"

with(engage.summary.byday %>% 
  summarise(mean.engagements = mean(num.ema.engagements)) %>%
  arrange(desc(mean.engagements)),
  user
) -> user.byengage

## Compute number of engagements for each user-EMA
left_join(select(daily, user, study.day, study.date),
            select(engage, user, ema.date, engaged.time), 
            by=c('user'='user','study.date'='ema.date')
            ) %>%
  group_by(user, study.day) %>% 
  summarise(num.ema.engagements = sum(!is.na(engaged.time))) %>%
  mutate(last.study.day = max(study.day)) -> engage.summary.byday

engage.summary.byday %>% group_by(user, num.ema.engagements) %>%
  summarise(count = n()) %>%
  mutate(rfreq = count / sum(count)) -> engage.summary

na.0 <- function(x) {
  ret <- x
  ret[which(ret==0)] <- NA
  return(ret)

}
engage.summary.byday %>% ungroup %>%
  ggplot() +
  stat_summary(aes(x=study.day, y=num.ema.engagements),
               fun.y=mean, geom='line') +
  #facet_wrap(~user) + 
  theme(strip.background=element_rect(fill=NA),
        panel.background=element_rect(fill=NA, color='grey'),
        panel.grid=element_blank(),
        axis.line=element_line(color='grey',size=0.25)) +
  xlab('Day in study') + ylab("Avg. num. 'engagements'\n(rows in EMA_Context_Engaged") +
  scale_y_continuous(breaks=c(0,0.5,1)) +
  scale_x_continuous(breaks=c(0,25,50,75)) -> plot.avg.eng.time

engage.summary.byday %>%
  group_by(study.day) %>%
  summarise(nusers = n()) %>%
  ggplot(aes(x= study.day, y=nusers)) + geom_line() +
  theme(strip.background=element_rect(fill=NA),
        panel.background=element_rect(fill=NA, color='grey'),
        panel.grid=element_blank(),
        axis.line=element_line(color='grey',size=0.25)) +
  scale_x_continuous(breaks=c(0,25,50,75)) +
  scale_y_continuous(breaks=c(0,15,30,45), labels=c(' 0',' 15', ' 30', ' 45')) +
  xlab('Day in study') + ylab('Users remaining') -> plot.nusers


engage.summary %>% ungroup %>%
  mutate(
    user = factor(user, levels=rev(user.byengage),
                   labels=paste(c(rep('', length(unique(user))-1), 'User '),
                  rev(user.byengage), sep=''))
    ) %>%
  ggplot(aes(x=num.ema.engagements, y=user)) + 
  geom_tile(aes(fill=rfreq), color = 'black',
            size=0.5)  +
  scale_fill_distiller(direction=1,
                       name='',
                       breaks=c(0.1,0.8),
                       guide = guide_colorbar(barheight=unit(6, 'pt'))) +
  ggtitle('Relative frequency of engagements\nby user') +
  xlab('Engagements') + ylab('') + 
  scale_x_continuous(breaks=c(0,1,2,4,8)) + 
  theme(panel.grid=element_blank(),
        legend.position='top',
        legend.direction='horizontal',
        legend.justification=c(1,0.5),
        panel.background = element_rect(fill=NA)) -> plot.heat.eng

engage.summary %>% ungroup %>%
  mutate(user = factor(user, levels=user.byengage,
                       labels=paste(c('User ', rep('', length(unique(user))-1)),
                                    user.byengage, sep=''))) %>%
  ggplot(aes(x=num.ema.engagements)) +
  geom_bar(aes(x=num.ema.engagements, y=rfreq), stat='identity',
           fill='white', color='black') + 
  facet_wrap(~user, nrow = 5) + xlab('Engagements') +
  ylab('Proportion') + 
  scale_y_continuous(breaks=c(0,0.5,1)) +
  scale_x_continuous(breaks=c(0,2,8)) +
  theme(panel.grid=element_blank(),
        strip.background=element_rect(fill=NA, color='white'),
        panel.background=element_rect(color='grey', fill=NA),
        strip.text=element_text(size=9),
        axis.text=element_text(size=9),
        axis.line=element_blank()) +
  ggtitle('Relative frequency of engagements by user') -> plot.hist.eng

###
### Create PDFs
###
pdf('avg_engagements_time.pdf', width=(16/9)* 5, height=5)
grid.arrange(plot.avg.eng.time + xlab('') + ylab('Average Engagements'),
             plot.nusers,
             nrow = 2
)
dev.off()

ggsave('freq_engagements_hist.pdf', 
       plot.hist.eng, width=(4/3) * 7, height=7, units='in')

ggsave('freq_engagements_heat.pdf',
       plot.heat.eng, width=4, height = 6.5, units='in')

