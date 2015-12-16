## helper functions

## normalize text fields
## nb: messages are inconsistent in their representation of UTF-8 characters
##     (e.g. some sources omit, others use an ASCII replacement)
normalize.text <- function(x) {
  ## strip new line escape code
  x <- gsub("\\n", "", x)
  ## strip all punctuation
  x <- gsub("[[:punct:]]", "", x)
  ## strip leading and trailing whitespace
  x <- gsub("^[[:space:]]+", "", x)
  x <- gsub("[[:space:]]+$", "", x)
  ## normalize remaining whitespace
  x <- gsub("[[:space:]]+", " ", x)
  ## strip non-ASCII characters
  iconv(x, "UTF-8", "ASCII", "")
}

## capitalize first letter
capitalize <- function(x) {
  x <- gsub("_", " ", x)
  sapply(x,
         function(y) if (is.na(y)) NA
                     else paste0(toupper(substring(y, 1, 1)), substring(y, 2)))
}

## copy variable from y to x, taking first matches in an identifier
copy <- function(x, y, var.name, id.name)
  y[match(x[[id.name]], y[[id.name]]), names(y) == var.name]

## data frame of unique variable values
get.values <- function(var.name, ...) {
  d <- lapply(list(...), function(x) x[var.name[var.name %in% names(x)]])
  d <- do.call("rbind", d)
  d <- d[!duplicated(d), , drop = FALSE]
  row.names(d) <- NULL
  d
}

## match x against values delimited in y
match.option <- function(x, y, l = rep(TRUE, length(y)), prefix = "",
                          other = TRUE, sep = "@", other.prefix = "Other==") {
  d <- data.frame(matrix(NA, length(y), length(x)))
  d[l, ] <- data.frame(do.call("rbind",
                               lapply(lapply(strsplit(y[l], "@"), unlist),
                                      function(z) match(x, z, nomatch = 0))))
  if (!is.null(names(x)))
    names(d) <- names(x)
  if (other) {
    d$other <- NA
    sapply(c(x, other.prefix),
           function(o) y[l] <<- gsub(o, "", y[l], fixed = TRUE))
    y[l] <- gsub("@", "", y[l])
    d$other[l] <- y[l]
  }
  names(d) <- paste(prefix, names(d), sep = ".")
  d
}

## like merge, but "proximally" in the named variables;
## bring y into x, such that id.x = id.y and the largest var.y <= var.x
merge.last <- function(x, y, id, var, id.x = id, id.y = id, var.x = var,
                       var.y = var, ...) {
  by.x <- c(id.x, var.x)
  by.y <- c(id.y, var.y)
  d <- merge(x[, names(x) %in% by.x], y, by.x = by.x, by.y = by.y, all = TRUE)
  j <- !(names(d) %in% names(x))
  v <- d[[var.x]]
  v[is.na(d[, j, drop = FALSE][, 1])] <- NA
  d <- cbind(d, v)
  names(d)[ncol(d)] <- var.y
  j <- c(j, TRUE)
  d[, j] <- do.call("data.frame", lapply(d[, j, drop = FALSE],
                                         function(v) impute(d[, 1], d[, 2], v)))
  print(nrow(d <- merge(x, d, by = by.x, all.x = TRUE, ...)))
  d
}

## write data frame to file if non-empty, otherwise delete file
write.data <- function(x, file, ...) {
  print(n <- nrow(x))
  if (n)
    write.csv(x, file = file, row.names = FALSE, ...)
  else if (file.exists(file))
    file.remove(file)
  invisible(x)
}

## duplicate values in variables passed to ...
check.dup <- function(x, file, ..., subset = rep(TRUE, nrow(x))) {
  id <- substitute(list(...))
  id <- do.call("paste", eval(id, x))
  if (!missing(subset)) {
    subset <- substitute(subset)
    subset <- eval(subset, x)
  }
  x$dup <- duplicated(id)
  d <- x[subset & id %in% id[x$dup], , drop = FALSE]
  write.data(d, file = file)
  invisible(list(is.dup = x$dup, data = d))
}

## --- date and time conversions
## nb: POSIXt objects (scalar, vector, list) don't support multiple time zones

## convert JSON-formatted character strings to a data frame
json2data <- function(x) {
  x <- x[-c(1, length(x))]
  x <- sapply(x, gsub, pattern = "[{[]$", replacement = "list(")
  x <- sapply(x, gsub, pattern = "[}]],$", replacement = "),")
  x <- sapply(x, gsub, pattern = "\" +: +", replacement = "=")
  x <- sapply(x, gsub, pattern = " +\"", replacement = "")
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

## convert character string to POSIXct in GMT/UTC,
## under the given offset and format
char2utime <- function(x, offset = 0, format = "%Y-%m-%d %H:%M:%OS") {
  ## character to POSIXlt
  l <- mapply(strptime, x = x, format = format, tz = "GMT", SIMPLIFY = FALSE)
  ## POSIXlt to POSIXct, which carries no time zone
  l <- do.call("c", lapply(l, as.POSIXct, tz = "GMT"))
  ## correct POSIXct value under the given UTC offset
  l - offset
}

## convert character string to POSIXlt elements (weekday, month, etc.),
## under the given time zone and format
char2calendar <- function(x, tz = "GMT", format = "%Y-%m-%d %H:%M:%OS",
                          prefix = NULL) {
  ## character to local POSIXlt
  l <- mapply(strptime, x = x, format = format, tz = tz, SIMPLIFY = FALSE)
  ## select POSIXlt list elements to numeric data frame
  l <- data.frame(do.call("rbind",
                          lapply(l, function(y) unlist(unclass(y)[ltime]))),
                  row.names = NULL)
  if (!is.null(prefix))
    names(l) <- paste(prefix, names(l), sep = ".")
  l
}

## convert hour and minute date-time elements to user-designated slots
## nb: data is presumed to contain the designated time slots in hours
ltime2slot <- function(hour, min, data = parent.frame()) {
  hour <- substitute(hour)
  hour <- eval(hour, data)
  min <- substitute(min)
  min <- eval(min, data)
  hours <- cbind(hour + (min / 60),
                 data[, match(paste(slots, "hours", sep = "."), names(data))])
  apply(hours, 1, function(x) findInterval(x[1], x[-1]))
}

## check the user-designated slot hours in a given data frame are valid
## nb: slot ranges are selection-dependent, but we consider the widest range
valid.slots <- function(x = parent.frame()) {
  with(x, pmin(findInterval(morning.hours, c(5, 9.5), TRUE) == 1,
               findInterval(lunch.hours, c(11, 13), TRUE) == 1,
               findInterval(afternoon.hours, c(14, 15), TRUE) == 1,
               findInterval(evening.hours, c(16.5, 18), TRUE) == 1,
               findInterval(dinner.hours, c(19, 20.5), TRUE) == 1,
               findInterval(ema.hours, c(19, 23.99)) == 1,
               apply(x[, grepl("\\.hours$", names(x))], 1,
                     function(y) all(y == cummax(y))))) == 1
}
