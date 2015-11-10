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
  x <- gsub("[[:space:]]", " ", x)
  ## strip non-ASCII characters
  iconv(x, "UTF-8", "ASCII", "")
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

## bring y into x, such that id.x = id.y and the largest var.y <= var.x
merge.last <- function(x, y, id, var, id.x = id, id.y = id, var.x = var,
                       var.y = var, order.by = NULL, ...) {
  by.x <- c(id.x, var.x)
  by.y <- c(id.y, var.y)
  d <- merge(x[, names(x) %in% by.x], y, by.x = by.x, by.y = by.y, all = TRUE)
  if (!missing(order.by))
    d <- d[with(d, order(order.by)), ]
  d <- impute.locf(d, d[[id]])
  merge(x, d, by = by.x, all.x = TRUE, ...)
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
  l <- mapply(strptime, x = x, format = format, tz = "GMT", SIMPLIFY = FALSE)
  l <- do.call("c", lapply(l, as.POSIXct, tz = "GMT"))
  l - offset
}

## convert character string to POSIXlt elements (weekday, month, etc.),
## under the given time zone and format
char2calendar <- function(x, tz = "GMT", format = "%Y-%m-%d %H:%M:%OS",
                          prefix = NULL) {
  l <- mapply(strptime, x = x, format = format, tz = tz, SIMPLIFY = FALSE)
  l <- data.frame(do.call("rbind", lapply(l, unlist)), row.names = NULL)
  l <- subset(l, select = sec:yday)
  l <- do.call("data.frame", lapply(l, as.numeric))
  if (!is.null(prefix))
    names(l) <- paste(prefix, names(l), sep = ".")
  l
}

## convert hour (0-23) to (approximate) time slot
## nb: (after) dinner and EMA slot definitions are dependent
hour2slot <- function(x)
  s <- c("ema",       # defined as dinner + 1 hour - 23:59  ~  21:00 - 04:00
         "morning",   # 05:00 - 09:30  ~  05:00 - 10:59
         "lunch",     # 11:00 - 13:00  ~  11:00 - 13:50
         "afternoon", # 14:30 - 15:00  ~  14:00 - 15:59
         "evening",   # 16:30 - 18:00  ~  16:00 - 18:59
         "dinner",    # 19:30 - 20:30  ~  19:00 - 20:59
         "ema")[findInterval(x, c(0, 5, 11, 14, 16, 19, 21, 23))]
