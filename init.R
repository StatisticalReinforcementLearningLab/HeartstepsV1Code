## run this first

source("functions.R")

## FIXME: Add other systems
mbox <- switch(Sys.info()["sysname"],
               "Windows" = "Z:/HeartSteps/Data/",
               "Linux" = "~/mbox/HeartSteps/Data/")

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

library(knitr)
library(xtable)
