## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(mbox)
load("csv.RData")
file <- "analysis.RData"

steps <- merge.last(jawbone,
                    subset(decision, select = c(user, notify, utime.stamp)),
                    "user", "end.utime", "utime.stamp")

save(steps, file = file)
