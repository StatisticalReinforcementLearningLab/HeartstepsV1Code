## run this first

library(knitr)
library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")

## FIXME: Add other systems
mbox <- switch(Sys.info()["sysname"],
               "Windows" = "Z:/HeartSteps/Data/",
               "Darwin" = "/Volumes/dav.box.com/dav/HeartSteps/Data/",
               "Linux" = "~/mbox/HeartSteps/Data/")

options(stringsAsFactors = FALSE)
if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale("LC_TIME", "English")
} else
  Sys.setlocale("LC_TIME", "en_US.UTF-8")