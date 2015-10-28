## load exported csv files into data frames,
## tidy up and save as an R workspace (.RData file)

source("init.R")
setwd(sys.var$mbox)
load("csv.RData")
file <- "analysis.RData"

suggest <- merge.last(jawbone,
                      subset(decision, select = c(user, notify, utime.stamp)),
                      "user", "end.utime", "utime.stamp")

save(suggest, file = file)
