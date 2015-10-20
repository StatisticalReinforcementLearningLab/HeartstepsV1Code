## run this first

source("functions.R")

## FIXME: Add other systems
mbox <- switch(Sys.info()["sysname"], "Linux" = "~/mbox")

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
