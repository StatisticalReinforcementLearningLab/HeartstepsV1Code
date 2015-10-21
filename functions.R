## helper functions

## strip whitespace, normalize quotes
strip.white <- function(x) {
  x <- gsub("\\n", "", x, perl = TRUE)
  x <- gsub("^ +", "", x, perl = TRUE)
  x <- gsub(" +$", "", x, perl = TRUE)
  gsub("[‘’“”\"]", "'", x, perl = TRUE)
}

## copy variable from y to x, taking first matches in an identifier
copy <- function(x, y, var.name, id.name)
  y[match(x[[id.name]], y[[id.name]]), names(y) == var.name]

## data frame of unique variable values, possibly indexed by any additional
## "time" named in var.name
get.values <- function(var.name, index = FALSE, ...) {
  d <- lapply(list(...), function(x) x[var.name[var.name %in% names(x)]])
  m <- min(unlist(lapply(d, ncol)))
  if (index) {
    d <- lapply(d, function(x) x[, 1:m, drop = FALSE])
    sapply(1:length(d), function(x) names(d[[x]][m]) <<- "time")
  }
  d <- do.call("rbind", d)
  d <- d[do.call("order", d), , drop = FALSE]
  d <- d[!duplicated(d[1:(m - index), , drop = FALSE]), , drop = FALSE]
  row.names(d) <- NULL
  d
}

write.data <- function(x, file, ...) {
  if (nrow(x))
    write.csv(x, file = file, row.names = FALSE, ...)
  else if (file.exists(file))
    file.remove(file)
}

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

## duplicate values in variables passed to ...
check.dup <- function(x, file, ...) {
  id <- substitute(list(...))
  id <- do.call("paste", eval(id, x))
  d <- x[id %in% id[duplicated(id)], , drop = FALSE]
  print(nrow(d))
  write.data(d, file = file)
  invisible(d)
}
