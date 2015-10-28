## run this first

library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")

wd <- getwd()

options(stringsAsFactors = FALSE)

## largest number of digits used to represent fractional seconds
options(digits.secs = 6)

## largest number of digits used to represent fractional seconds
## + number of digits in Unix time
options(digits = 10 + 6)

if (Sys.info()["sysname"] == "Windows") {
  mbox <- "Z:/HeartSteps/Data/"
  Sys.setlocale("LC_TIME", "English")
}
else if (Sys.info()["sysname"] == "Darwin") { # Mac
  mbox <- "/Volumes/dav/HeartSteps/Data/"
  Sys.setlocale("LC_TIME", "en_US")
}
else if (Sys.info()["sysname"] == "Linux") {
  mbox <- "~/mbox/HeartSteps/Data/"
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
}
else
  warning("Unrecognized system. Set 'mbox' path and time locale manually.")
