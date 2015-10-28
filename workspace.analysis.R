## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")
file <- "analysis.RData"

## --- aggregate EMA data frame from EMA-question to EMA level
temp <- subset(ema, select = c(user, contextid, message.time, time.stamp))
temp.min <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[1])
temp.max <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[length(x)])
names(temp.min)[3:4] <- paste("min", names(temp.min)[3:4], sep = ".")
names(temp.max)[3:4] <- paste("max", names(temp.max)[3:4], sep = ".")
temp <- merge(temp.min, temp.max, by = c("user", "contextid"))
                  
ema$hectic <- with(ema, as.numeric(ifelse(question == "1", response, NA)))
ema$stress <- with(ema, as.numeric(ifelse(question == "2", response, NA)))
ema$typical <- with(ema, as.numeric(ifelse(question == "3", response, NA)))
ema$follow <- with(ema, ifelse(question == "5", response, NA))
ema$msg.down <- with(ema, ifelse(question == "6", message, NA))
ema$msg.up <- with(ema, ifelse(question == "7", message, NA))

ema <- cbind(ema,
             match.option(ema4, ema$response, ema$question == "4", "q4", FALSE),
             match.option(ema6, ema$response, ema$question == "6", "q6"),
             match.option(ema7, ema$response, ema$question == "7", "q7"),
             match.option(research1, ema$response,
                          ema$question == "research1", "rq1"),
             match.option(research2, ema$response,
                          ema$question == "research2", "rq2"))

temp <- 
                 
ema <- 
ema <- merge(aggregate(cbind(contextid),
                       function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)][1])),
             aggregate(subset(ema, select = -contextid), list(ema$contextid),
                       function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)][1]))

save(suggest, file = file)
