## run this first
## nb: ensure that getwd() returns the local repository

# Search for and install all necessary packages if needed
for (pkg in c("tikzDevice", "xtable", "zoo", "geepack")) {
  if (!(pkg %in% installed.packages()[, "Package"])) {
    install.packages(pkg, dependencies = TRUE)
  }
  # load required packages
  require(pkg, character.only = TRUE)
}

source("xzoo.R")
source("xgeepack.R")
source("functions.R")
source("read.data.R")
source("ema.options.R")

options(stringsAsFactors = FALSE)

## largest number of digits used to represent fractional seconds
options(digits.secs = 6)

## number of digits in Unix time (seconds since 1970-01-01 00:00 UTC)
## + largest number of digits used to represent fractional seconds
options(digits = 10 + 6)

## system-dependent variables
sys.var <- switch(Sys.info()["sysname"],
                  "Windows" = list(locale = "English",
                                   mbox = "Z:/HeartSteps/"),
                  "Darwin" = list(locale = "en_US",
                                  mbox = "/Volumes/dav/HeartSteps/"),
                  "Linux" = list(locale = "en_US.UTF-8",
                                 mbox = "~/mbox/HeartSteps/"))
sys.var$repo <- getwd()
sys.var$mbox.data <- paste(sys.var$mbox, "Data/", sep = "")

## time zone identifiers are localized, so set the locale
Sys.setlocale("LC_ALL", sys.var$locale)

## arithmetic on POSIXct objects uses system time zone, so set this to UTC
Sys.setenv(TZ = "GMT")

## select POSIXlt elements
ltime <- c("year", "mon", "yday", "mday", "wday", "hour", "min", "sec")

## intervention time slot labels, in order
slots <- c("morning", "lunch", "afternoon", "evening", "dinner", "ema")
