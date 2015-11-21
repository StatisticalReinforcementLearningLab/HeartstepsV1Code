## run this first

library(tikzDevice)
library(xtable)
library(zoo)

source("xzoo.R")
source("functions.R")
source("read.data.R")
source("ema.options.R")

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
sys.var$repo <- getwd()

## time zone identifiers are localized, so set the locale
Sys.setlocale("LC_TIME", sys.var$locale)

## arithmetic on POSIXct objects uses system time zone, so set this to UTC
Sys.setenv(TZ = "GMT")

## select POSIXlt elements
ltime <- c("year", "mon", "yday", "mday", "wday", "hour", "min", "sec")

## intervention time slot labels, in order
slots <- c("morning", "lunch", "afternoon", "evening", "dinner", "ema")
