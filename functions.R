## helper functions

## strip whitespace, normalize quotes
strip.white <- function(x) {
  x <- gsub("\\n", "", x, perl = TRUE)
  x <- gsub("^ +", "", x, perl = TRUE)
  x <- gsub(" +$", "", x, perl = TRUE)
  gsub("[‘’“”\"]", "'", x, perl = TRUE)
}

## copy variable from y to x, taking first matches in an identifier
copy <- function(x, y, varname, idname)
  y[match(x[[idname]], y[[idname]]), names(y) == varname]

## all unique identifier values
get.ids <- function(id.name, ...)
  sort(unique(unlist(lapply(list(...), function(x) x[[id.name]]))))

## --- date and time conversions
## nb: POSIXt objects (scalar, vector, list) don't support multiple time zones

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

## retreive Google Maps timezone data from GPS coordinates
## nb: rate and number of daily queries are limited
## https://developers.google.com/maps/documentation/timezone/usage-limits
gps2timezone <- function(lat, long, utime = 0) {
  u <- url(paste("https://maps.googleapis.com/maps/api/timezone/json?location=",
                 lat, ",", long, "&timestamp=", utime, "&sensor=false",
                 sep = "", collapse = ""))
  on.exit(close(u))
  json2data(readLines(u))
}

## convert character string to Date, under the given format
char2date <- function(x, format = "%Y-%m-%d")
  as.Date(paste(x), format = format)

## convert character string to Unix time (seconds since 1970-01-01 00:00 UTC),
## under the given GMT/UTC offset and format
char2utime <- function(x, offset = 0, format = "%Y-%m-%d %H:%M:%S") {
  l <- mapply(strptime, x = x, format = format, tz = "GMT", SIMPLIFY = FALSE)
  l <- do.call("c", lapply(l, as.POSIXct, tz = "GMT"))
  as.numeric(l) - offset
}

## convert character string to POSIXlt elements (weekday, month, etc.),
## under the given time zone and format
char2calendar <- function(x, tz, format = "%Y-%m-%d %H:%M:%S") {
  l <- mapply(strptime, x = x, format = format, tz = tz, SIMPLIFY = FALSE)
  l <- data.frame(do.call("rbind", lapply(l, unlist)), row.names = NULL)
  subset(l, select = sec:yday)
}

## --- checks

## invalid time zone identifier
check.tz <- function(x, tz, file) {
  tz <- substitute(tz)
  tz <- eval(tz, x)
  d <- d[!(tz %in% OlsonNames()), ]
  if (nrow(d))
    write.csv(d, row.names = FALSE, file = file)
  else if (file.exists(file))
    file.remove(file)
}

## duplicate values in variables passed to ...
check.dup <- function(x, file, ...) {
  id <- substitute(list(...))
  id <- do.call("paste", eval(id, x))
  d <- x[id %in% id[duplicated(id)], , drop = FALSE]
  print(n <- nrow(d))
  if (n)
    write.csv(d, row.names = FALSE, file = file)
  else if (file.exists(file))
    file.remove(file)
  invisible(d)
}
