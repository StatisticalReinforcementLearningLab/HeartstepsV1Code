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

## --- date and time conversions
## nb: we always convert from character strings because POSIXt classes don't
##     support multiple time zones

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

## convert character string to Unix time, under the given time zone and format
## (Unix time is seconds since 1970-01-01 00:00.00 UTC)
char2utime <- function(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S") {
  do.call("c", mapply(strptime, x = x, format = format, tz = tz, SIMPLIFY = FALSE))
}

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

## --- checks

checktz <- function(tz, id, file) {
  d$tz_valid <- d$tz %in% OlsonNames()
  if (any(!d$tz_valid)) {
    with(d, print(table(timezone, tz, useNA = "ifany")))
    cat("Users with invalid timezone data:\n")
    with(d, print(sort(unique(userid[!tz_valid]))))
  }
}

checkdup <- function(x, id, file) {
  id <- substitute(id)
  id <- eval(id, x)
  d <- x[id %in% id[duplicated(id)], ]
  print(nrow(d))
  if (nrow(d))
    write.csv(d, row.names = FALSE, file = file)
  else if (file.exists(file))
    file.remove(file)
}
