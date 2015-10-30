## run this first

library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")
source("ema.options.R")

wd <- getwd()

options(stringsAsFactors = FALSE)

## largest number of digits used to represent fractional seconds
options(digits.secs = 6)

## number of digits in Unix time (seconds since 1970-01-01 00:00 UTC)
## + largest number of digits used to represent fractional seconds
options(digits = 10 + 6)

sys.var <- switch(Sys.info()["sysname"],
                  "Windows" = list(mbox = "Z:/HeartSteps/Data/",
                                   locale = "English"),
                  "Darwin" = list(mbox = "/Volumes/dav/HeartSteps/Data/",
                                  locale = "en_US"),
                  "Linux" = list(mbox = "~/mbox/HeartSteps/Data/",
                                 locale = "en_US.UTF-8"))

Sys.setlocale("LC_TIME", sys.var$locale)

## latest date of user activity reflected in the data files
last.date <- char2date("2015-10-28")
