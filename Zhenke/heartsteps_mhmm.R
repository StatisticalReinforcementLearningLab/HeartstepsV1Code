#
# Multilevel Hidden Markov Model for Heartsteps
#
# Zhenke Wu | zhenkewu@gmail.com
# 1st version: Apr 10, 2017 - generate minute-by-minute plot at local time.
# 2nd version: May 23, 2017 - fit JAGS model
rm(list=ls())
gitcode_dir <- "/Users/zhenkewu/Dropbox/ZW/professional/git/heartstepsdata/Zhenke"
test_dir <- "/Users/zhenkewu/Dropbox/ZW/professional/=research_agenda/mHealth/streaming_inference/code_streaming_inference"
#
# load data:
# 
setwd("/Users/zhenkewu/Dropbox/ZW/professional/git/heartstepsdata/")
source("init.R")
load(paste0(sys.var$mbox.data, "analysis-small.RData"))
setwd(test_dir)

#
# functions:
#

user_num_id_seq        <- 11
day_in_study_local_seq <- 5 # individual 1 and day 31 is really wierd: no step counts.

build_data <- function(user_num_id_seq,day_in_study_local_seq,daily,jawbone,suggest){
  steps_list <- vector(mode = "list",length(user_num_id_seq)*length(day_in_study_local_seq))
  suggest_list <- vector(mode = "list",length(user_num_id_seq)*length(day_in_study_local_seq))
  count <- 0
  for (user_num_id in user_num_id_seq){
    for (day_in_study_local in day_in_study_local_seq){
      count <- count +1
      if (!(day_in_study_local %in% daily$study.day.nogap[daily$user == user_num_id])) {
        stop(paste("Day", day_in_study_local, "does not exist for user", user_num_id))}
      date_for_link <- daily$study.date[daily$user==user_num_id & daily$study.day.nogap == day_in_study_local &
                                          !is.na(daily$study.day.nogap)]
      
      res <- jawbone[jawbone$user==user_num_id & jawbone$start.date == as.Date(date_for_link),]
      start.time.local <- as.POSIXlt(res$start.utime.local)
      res$hour_local <- start.time.local$hour+start.time.local$min/60+start.time.local$sec/3600 # from 0 to 24.
      res$minute_ind <- start.time.local$hour*60 + start.time.local$min
      res$day_in_study_local <- rep(day_in_study_local,nrow(res))
      res$date_for_link_local <- rep(date_for_link,nrow(res))
      
      dsuggest <- subset(suggest,user==user_num_id & study.date == as.Date(date_for_link))
      dsuggest$start.local.strip <- as.POSIXlt(dsuggest$utime.stamp+dsuggest$gmtoff) # <-- convert GMT time to local time using "gmtoff".
      # question: what does NA mean for an hour_local:
      dsuggest$hour_local <- dsuggest$start.local.strip$hour + dsuggest$start.local.strip$min / 60 + dsuggest$start.local.strip$sec / 60^2 # <-- includes available and message sent/not sent; not available.
      dsuggest$minute_ind <- dsuggest$start.local.strip$hour*60 + dsuggest$start.local.strip$min
      # table(rowSums(is.na(cbind(dsuggest$start.local.strip$hour,dsuggest$start.local.strip$min,dsuggest$start.local.strip$sec)))) # <-- all or none of hour, min, sec are available.
      
      dsuggest$decision_type <- rep(NA,nrow(dsuggest))
      dsuggest$decision_type[dsuggest$link==1 & dsuggest$avail == TRUE] <- 2
      dsuggest$decision_type[dsuggest$link==0 & dsuggest$avail == TRUE] <- 1
      dsuggest$decision_type[dsuggest$avail == FALSE] <- 0
      dsuggest$day_in_study_local  <- rep(day_in_study_local,nrow(dsuggest))
      dsuggest$date_for_link_local <- rep(date_for_link,nrow(dsuggest))
      dsuggest$wday <- lubridate::wday(as.Date(date_for_link,'%Y-%d-%m'), label=TRUE, abbr = FALSE)
      
      steps_list[[count]]   <- res
      suggest_list[[count]] <- dsuggest
    }
  }
  

  list(steps  = do.call("rbind",steps_list)[,c("user","day_in_study_local","date_for_link_local",
                                               "hour_local","steps")], 
       suggest = do.call("rbind",suggest_list)[,c("user","day_in_study_local","date_for_link_local",
                                                  "hour_local",
                                                  "minute_ind",
                                                  "decision_type","wday")]) 
  # <-- currently we do not have the minute by minute level information. Essentially
  # We do not need to store information other than minutes where actual steps are recorded.
  # This relates to the C++ language implementation of the memory-efficient sparse data format.
}

dat_full <- build_data(c(1,11),c(5,6),daily,jawbone,suggest)

plot_minute_level_steps <- function(dat_person_day){
  dat_steps   <- dat_person_day$steps
  dat_suggest <- dat_person_day$suggest
  with(dat_steps, plot(steps ~ hour_local, type = "p", pch = 20, lwd=0.5, 
                       xaxt="n",xlim = c(0, 24), ylim = c(-20, 210),
                       ylab = "Minute-level step count", xlab = "Hours since midnight (user local time)",
                       main = unique(paste0("\nUser ", user, ", Day ", day_in_study_local,
                                            " (", date_for_link_local,"; ",dat_suggest$wday,")")),bty="n")) # <-- done within the newly created data frame: steps, day.time (to minutes)
  segments(dat_steps$hour_local, 0, y1 = dat_steps$steps,lwd = 0.25,col="grey")
  axis(1,at=c(0,1:24),labels=c(0,1:24))
  segments(dat_suggest$hour_local-0.5, c(-20,-10,-5)[dat_suggest$decision_type+1], dat_suggest$hour_local+0.5)
  points(dat_suggest$hour_local, c(-20,-10,-5)[dat_suggest$decision_type+1], 
         pch = c(0,15,17)[dat_suggest$decision_type+1], 
         col = c("red","blue","blue")[dat_suggest$decision_type+1])
}

plot_minute_level_steps(build_data(c(1),c(31),daily,jawbone,suggest))

show_user <- 11
show_day  <- 5
pdf(file.path(test_dir,paste0("mbm_raw_data_random_user_",show_user,"_day_",show_day,".pdf")),height=4,width=12)
par(mfrow=c(1,2))
plot_minute_level_steps(build_data(c(show_user),c(show_day),daily,jawbone,suggest))
plot_minute_level_steps(build_data(c(show_user),c(show_day+1),daily,jawbone,suggest))
dev.off()

# all persons, all day:
for (user in unique(daily$user)){
  pdf(file.path(test_dir,paste0("mbm_raw_data_user_",user,".pdf")),height=14,width=20)
  par(mfrow=c(7,7))
  curr_day_nogap <- daily$study.day.nogap[daily$user == user & !is.na(daily$study.day.nogap)]
  for (i in curr_day_nogap) {
    curr_dat <- build_data(user,i,daily,jawbone,suggest)
    if (nrow(curr_dat$steps)!=0){
      plot_minute_level_steps(curr_dat)
    }
  }
  dev.off()
}

# pdf(file.path(test_dir,"gap_time_distribution_exponential.pdf"),height=6,width=10)
# par(mfrow=c(3,5))
# for (id in 1:15){
#   gap_vec <- NULL
#   y <- NULL
#   for (day in c(0,1:22)){
#     if (!(day %in% daily$study.day.nogap[daily$user == id])) 
#       warning(paste("Day", day, "does not exist for user", id))
#     day.date <- daily$study.date[daily$user == id & daily$study.day.nogap == day
#                                  & !is.na(daily$study.day.nogap)]
#     d <- subset(jawbone, user == id & start.date == as.Date(day.date)) # <-- reason for missing some step counts data??
#     d$start.local.strip <- as.POSIXlt(d$start.utime.local)
#     d$day.time <- d$start.local.strip$hour + d$start.local.strip$min / 60 + d$start.local.strip$sec / 60^2
#     gap_vec <- c(gap_vec,diff(d$day.time))
#     y <- c(y,d$steps)
#   }  
#   h <- hist(gap_vec,breaks="Scott",plot=FALSE)
#   plot(h$mids,log(h$density),xlab="gap",ylab="log(frequency)",main=paste0("User",id),xlim=c(0,15))
# }
# dev.off()

## Generate 20 plots at random  
## NB: This doesn't pull from user 35, who disappeared after 3 weeks
## NB: Only pulls up to an individual's day 35 on study


