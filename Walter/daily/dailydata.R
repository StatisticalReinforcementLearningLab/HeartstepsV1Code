### Setup
sys.var_repo = "~/Documents/github/heartstepsdata"
setwd(sys.var_repo)
source("init.R")

sys.var <- switch(Sys.info()["sysname"],
                  "Windows" = list(locale = "English",
                                   mbox = "Z:/HeartSteps/Data"),
                  "Darwin" = list(locale = "en_US",
                                  mbox = "/Volumes/dav/HeartSteps/Data"),
                  "Linux" = list(locale = "en_US.UTF-8",
                                 mbox = "~/mbox/HeartSteps/Data"))

setwd(sys.var$mbox)
load("csv.RData")
load("analysis.RData")
daily.jb = read.csv("daily.jbsteps.csv")
setwd("/Volumes/dav/HeartSteps/Hyesun's")
load("location_for_decision.RData")
load("weather.RData")

## Included ema.set to daily to analyze ema.set
daily <- merge(daily,
               aggregate(subset(ema, select = c(ema.set)),
                         by = with(ema, list(user, ema.date)),
                         function(x) na.omit(x)[1]),
               by.x = c("user", "study.date"),
               by.y = paste("Group", 1:2, sep = "."), all.x = TRUE)

##############attach daily jbsteps##############################
daily.jb$DATE=as.character(daily.jb$DATE)
daily.jb$DATE = as.Date(daily.jb$DATE, "%Y%m%d")

daily=merge(daily, subset(daily.jb,select=c(user, DATE, m_steps)), by.x=c("user","study.date")
        , by.y = c("user","DATE"), all.x=TRUE)

## daily log jawbone steps and pre log jawbone steps
daily$dailyjb.log=log(daily$m_steps.x+1/2)
daily$dailyjbpre.log=c(0,daily$dailyjb.log[-nrow(daily)])
daily$dailyjbpre.log[daily$study.day==0] = 0

## daily sqrt jbsteps and pre sqrt jbsteps
daily$dailyjb.sqrt=sqrt(daily$m_steps.x)
daily$dailyjbpre.sqrt=c(0,daily$dailyjb.sqrt[-nrow(daily)])
daily$dailyjbpre.sqrt[daily$study.day==0] = 0

## lag2 ##no effect
daily$dailyjbpre2.sqrt=c(0, 0, daily$dailyjb.sqrt[-((nrow(daily)-1):nrow(daily))])
daily$dailyjbpre2.sqrt[daily$study.day==0] = 0
daily$dailyjbpre2.sqrt[daily$study.day==1] = 0

## quadratic study day nogap
daily$study.day.nogap.sq=(daily$study.day.nogap)^2

## study.day.pre
daily$study.day.nogap.pre=daily$study.day.nogap-1
daily$study.day.nogap.pre.sq=(daily$study.day.nogap.pre)^2

##daily plan TRUE and pre plan TRUE
daily$planTrue=(daily$planning %in% c("structured","unstructured"))
daily$planTrue.pre=c(0,daily$planTrue[-nrow(daily)])
daily$planTrue.pre[daily$study.day==0] = 0
daily$planWeight=(daily$planning %in% c("structured","unstructured","no_planning"))
daily$planWeight.pre=c(0,daily$planWeight[-nrow(daily)])

## unstructured planning 
daily$USplanTrue=(daily$planning %in% c("unstructured"))
daily$USplanTrue.pre=c(0,daily$USplanTrue[-nrow(daily)])
daily$USplanTrue.pre[daily$study.day==0] = 0

daily$SplanTrue=(daily$planning %in% c("structured"))
daily$SplanTrue.pre=c(0,daily$SplanTrue[-nrow(daily)])
daily$SplanTrue.pre[daily$study.day==0] = 0

## respond 
#daily$respond.pre=c(0,daily$respond[-nrow(daily)])
#daily$respond.pre[daily$study.day==0] = 0

## setting city and state######################################
decision$city.coor=location_for_decision$city.coor
decision$state.coor=location_for_decision$state.coor


## attaching city and state####################################
tmp=aggregate(paste(city.coor,state.coor,sep="_")~date.stamp+user,data=decision, 
               FUN = function(x) length(unique(x)))
names(tmp)[1]="study.date" ;names(tmp)[3]="city.number"
temp1=merge(daily, tmp, by.x = c("user", "study.date"), by.y = c("user", "study.date"),
      all.x = TRUE)

## add city and state to suggest data frame#############################
suggest_temp <- merge(suggest,subset(decision,
                                  select=c("user", "date.stamp", "slot","city.coor","state.coor")),
                   by.x = c("user", "study.date", "slot"),
                   by.y = c("user", "date.stamp", "slot"), all.x = TRUE)

## add precipitation data to suggest data frame ############################
## precipitation has "None" value as well
suggest_temp <- merge(suggest_temp,subset(weather,select=c("study.date","city.coor","state.coor","precip")),
                   by.x=c("study.date", "city.coor","state.coor"),
                   by.y=c("study.date", "city.coor","state.coor"), all.x = TRUE, sort=FALSE)

suggest_temp <- with(suggest_temp, suggest_temp[order(user, study.date, slot),])
suggest_temp$temperature[suggest_temp$temperature==(-1024)] <- NA
suggest_temp$precipitation.chance[suggest_temp$precipitation.chance=="unknown"] <- NA
suggest_temp$precipitation.chance[suggest_temp$precipitation.chance==(-1)] <- NA


###add average temperature based of "suggest" data to "daily" data frame
tmp <- aggregate(temperature~study.date+user
              , data=suggest_temp, FUN = function(x) mean(x, na.rm = TRUE), na.action = na.pass)
tmp1 <- aggregate(temperature~study.date+user
               , data=suggest_temp, FUN = function(x) paste(sum(!is.na(x)),"/",length(x),sep=""), 
               na.action = na.pass)

tmp <- cbind(tmp,tmp1[,3])
names(tmp)[1] <- "study.date" ;names(tmp)[3] <- "temp_mean";names(tmp)[4] <- "temp_day_used";

temp1 <- merge(temp1, tmp,
         by.x = c("user", "study.date"), by.y = c("user", "study.date"),
         all.x = TRUE)

###add daily precipitation based on "suggest" data to "daily" data frame
tmp <- aggregate(as.numeric(precip)~study.date+user
               , data=suggest_temp, FUN = function(x) mean(x, na.rm = TRUE), na.action = na.pass)
tmp1 <- aggregate(precip~study.date+user
               , data=suggest_temp, FUN = function(x) paste(sum(!is.na(x)),"/",length(x),sep=""),
               na.action = na.pass)

tmp <- cbind(tmp,tmp1[,3])
names(tmp)[1] <- "study.date" ;names(tmp)[3] <- "daily.precip_mean";names(tmp)[4] <- "daily.precip_mean_used";

temp1 <- merge(temp1, tmp,
         by.x = c("user", "study.date"), by.y = c("user", "study.date"),
         all.x = TRUE)

temp1 <- with(temp1,temp1[order(user,study.date),])

temp <- temp1[!is.na(temp1$study.day.nogap),]
temp <- temp[temp$study.day.nogap %in% 0:41,]

temp=temp[temp$study.day.nogap!=0,]
temp=temp[temp$study.day.nogap!=1,]

temp=temp[!is.na(temp$dailyjb.sqrt) & !is.na(temp$dailyjbpre.sqrt),]
tmp1=temp[as.logical(temp$planWeight.pre),]
## there is missing value in context.wday which is wrong########################################
tmp1$wday=strftime(tmp1$study.date,'%u')
tmp1$weekendTrue=tmp1$wday %in% 6:7
## Weekday wday is coded as 1:5 is Mon-Fri#####################################################

model.plan.week <- geeglm(dailyjb.sqrt~ dailyjbpre.sqrt + I(weekendTrue)+I(planTrue.pre - .5)+I(planTrue.pre - .5):I(weekendTrue), id = user, scale.fix = T, data = tmp1)

summary(model.plan.week)

### Data that we want for now ###

## General info
tmp1$user # User
tmp1$study.day.nogap # Study day

# Days with missing data are indicators of missing tracker data!!
# We currently don't have weather data for those days!!
# Example:
tmp1$study.day.nogap[1:10] # Missing days 4 & 11 & 12 !

## Exogenous variables:
tmp1$precipitation.chance # Precipitation chance
tmp1$daily.precip_mean # Mean precipitation rainfall
tmp1$temp_mean # Mean temparature

tmp1$weekendTrue # Weekend indicator

## Action data
tmp1$planning.today

## I know how many EMAS were filled out
tmp1$ema.set.length # 243 NA's , 4 with less than 7 filled out, 793 with 7 or 8 ; I'd count 243 + 4 as not answering
is.na(tmp1$ema.set.length) # Measure of engagement

## App USAGE

## App sessions: the number of times the user opened the app that day 
## and remained in the app for at least 2 seconds
tmp1$app.sessions[1:10] 

## App secs: is the total time in seconds that the user spent in the 
## app over those sessions counted in app.sessions
tmp1$app.secs[1:10]

cor(log(tmp1$app.sessions[tmp1$app.secs > 0]), log(tmp1$app.secs[tmp1$app.secs > 0]))

plot(log(tmp1$app.sessions[tmp1$app.secs > 0]), log(tmp1$app.secs[tmp1$app.secs > 0]))

# STill need (a) thumbs up / down, and (b) number of times app is used