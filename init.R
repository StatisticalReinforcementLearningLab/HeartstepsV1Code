## run this first

library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")

wd <- getwd()
mbox <- switch(Sys.info()["sysname"],
               "Windows" = "Z:/HeartSteps/Data/",
               "Darwin" = "/Volumes/dav/HeartSteps/Data/",
               "Linux" = "~/mbox/HeartSteps/Data/")

options(stringsAsFactors = FALSE)

## largest number of digits used to represent fractional seconds
options(digits.secs = 6)

## largest number of digits used to represent fractional seconds
## + number of digits in Unix time
options(digits = 10 + 6)

if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
}