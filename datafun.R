## helper functions

## strip whitespace
omit.space <- function(x) {
  x <- gsub("\\n", "", x, perl = TRUE)
  x <- gsub("^ +", "", x, perl = TRUE)
  x <- gsub(" +$", "", x, perl = TRUE)
  gsub(" +$", " ", x, perl = TRUE)
}

## copy variable from y to x, taking first matches in an identifier
copy <- function(x, y, varname, idname)
  y[match(x[[idname]], y[[idname]]), names(y) == varname]

## --- dates and times

## convert JSON-formatted character strings to a data frame
json2data <- function(x) {
  x <- x[-c(1, length(x))]
  x <- sapply(x, gsub, pattern = "[{[]$", replacement = "list(", perl = TRUE)
  x <- sapply(x, gsub, pattern = "[}]],$", replacement = "),", perl = TRUE)
  x <- sapply(x, gsub, pattern = "\" +: +", replacement = "=", perl = TRUE)
  x <- sapply(x, gsub, pattern = " +\"", replacement = "", perl = TRUE)
  do.call("data.frame",
          eval(parse(text = paste(c("list(", x, ")"), collapse = ""))))
}

## retreive Google Maps timezone data from gps coordinates
## nb: rate and number of daily queries are limited
## https://developers.google.com/maps/documentation/timezone/usage-limits
gps2timezone <- function(lat, long, utime = 0) {
  u <- url(paste("https://maps.googleapis.com/maps/api/timezone/json?location=",
                 lat, ",", long, "&timestamp=", utime, "&sensor=false",
                 sep = "", collapse = ""))
  on.exit(close(u))
  json2data(readLines(u))
}

## convert character string to Date, assuming the given format
char2date <- function(x, format = "%Y-%m-%d")
  as.Date(paste(x), format = format)

## convert character string to POSIXlt, assuming the given format
char2ltime <- function(x, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  do.call("c", mapply(strptime, x = paste(x), format = format, tz = tz,
                      SIMPLIFY = FALSE))

## convert POSIXlt to Unix time (seconds since 1970-01-01 00:00.00 UTC)
## nb: this accounts for different time zones
ltime2utime <- function(x)
  unclass(as.POSIXct(x))

## convert POSIX[cl]t to time of day
timeofday <- function(x) {
  x <- as.numeric(format(x, "%H"))
  cut(x, c(min(x, na.rm = TRUE) - 1, 12, 17, max(x, na.rm = TRUE) + 1),
      labels = c("Morning", "Afternoon", "Evening"), right = FALSE)
}
