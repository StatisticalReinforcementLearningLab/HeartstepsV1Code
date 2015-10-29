## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

load("csv.RData")
source("init.R")
setwd(sys.var$mbox)
file <- "analysis.RData"

## --- users
user <- merge(intake, exit, by = c("user", "userid"), all = TRUE)

## --- aggregate EMA data frame from EMA-question to EMA level

## minimum and maximum message and stamp times
temp <- subset(ema, select = c(user, contextid, message.time, time.stamp))
temp.min <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[1])
temp.max <- aggregate(temp[, 3:4], temp[, 1:2], function(x) sort(x)[length(x)])
names(temp.min)[3:4] <- paste("min", names(temp.min)[3:4], sep = ".")
names(temp.max)[3:4] <- paste("max", names(temp.max)[3:4], sep = ".")
temp <- merge(temp.min, temp.max, by = c("user", "contextid"))

## response strings to indicators (where possible)
ema$hectic <- with(ema, as.numeric(ifelse(question == "1", response, NA)))
ema$stress <- with(ema, as.numeric(ifelse(question == "2", response, NA)))
ema$typical <- with(ema, as.numeric(ifelse(question == "3", response, NA)))
ema$energy <-
  with(ema, as.numeric(ifelse(question == "research3", response, NA)))
ema$urge <- with(ema, as.numeric(ifelse(question == "research4", response, NA)))
ema$follow <- with(ema, ifelse(question == "5", response, NA))
ema$msg.down <- with(ema, ifelse(question == "6", message, NA))
ema$msg.up <- with(ema, ifelse(question == "7", message, NA))

ema <- cbind(ema,
             match.option(ema4, ema$response,
                          ema$question == "4", "active", FALSE),
             match.option(ema6, ema$response, ema$question == "6", "down"),
             match.option(ema7, ema$response, ema$question == "7", "up"),
             match.option(research1, ema$response,
                          ema$question == "research1", "barrier"),
             match.option(research2, ema$response,
                          ema$question == "research2", "enabler"))

## order in which each question was asked
ema$order.hectic <- with(ema, ifelse(question == "1", order, NA))
ema$order.stress <- with(ema, ifelse(question == "2", order, NA))
ema$order.typical <- with(ema, ifelse(question == "3", order, NA))
ema$order.active <- with(ema, ifelse(question == "4", order, NA))
ema$order.follow <- with(ema, ifelse(question == "5", order, NA))
ema$order.down <- with(ema, ifelse(question == "6", order, NA))
ema$order.up <- with(ema, ifelse(question == "7", order, NA))
ema$order.barrier <- with(ema, ifelse(question == "research1", order, NA))
ema$order.enabler <- with(ema, ifelse(question == "research2", order, NA))
ema$order.energy <- with(ema, ifelse(question == "research3", order, NA))
ema$order.urge <- with(ema, ifelse(question == "research4", order, NA))

dim(temp)
ema <- aggregate(subset(ema, select = c(hectic:order.urge)),
                 subset(ema, select = c(user, contextid)),
                 function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)][1]))
dim(ema)
ema <- merge(temp, ema, by = c("user", "contextid"), sort = TRUE)
dim(ema)


save(user, ema, file = file)
