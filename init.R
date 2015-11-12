## run this first

library(tikzDevice)
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

## time zone identifiers are localized, so set the locale
Sys.setlocale("LC_TIME", sys.var$locale)

## arithmetic on POSIXct objects uses system time zone, so set this to UTC
Sys.setenv(TZ = "GMT")

## select POSIXlt elements
ltime <- c("year", "mon", "yday", "mday", "wday", "hour", "min", "sec")

## intervention time slots are user-designated, under some constraints
slots <- c(morning = 5,    # morning  05:00 - 09:30
           lunch = 11,     # lunch  11:00 - 13:00
           afternoon = 14, # afternoon  14:30 - 15:00
           evening = 16,   # evening  16:30 - 18:00
           dinner = 19,    # dinner  19:30 - 20:30
           ema = 21)       # planning/EMA  dinner + 1 hour - 23:59
