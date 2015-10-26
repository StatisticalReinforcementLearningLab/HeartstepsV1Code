## run this first

library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")

## FIXME: Add other systems
mbox <- switch(Sys.info()["sysname"],
               "Windows" = "Z:/HeartSteps/Data/",
               "Darwin" = "/Volumes/dav/HeartSteps/Data/",
               "Linux" = "~/mbox/HeartSteps/Data/")

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
