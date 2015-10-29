## read CSV-formatted data into an R data frame

read.data <- function(file, order.by = NULL, utime = TRUE, ptime = TRUE, ...) {
  ## read each entry in file into a data frame
  d <- sapply(file, read.csv, header = TRUE, strip.white = TRUE, ...,
              simplify = FALSE)
  ## if multiple files are specified, ensure each data frame in d contains the
  ## the same variables in the same order; if one such variable does not appear
  ## in a given data frame, add it as a vector of missing values
  if (length(d) > 1) {
    l <- d
    n <- unique(unlist(lapply(d, names)))
    d <- lapply(l, function(x) data.frame(matrix(NA, nrow(x), length(n),
                                                 dimnames = list(NULL, n))))
    sapply(1:length(l), function(i) d[[i]][, match(names(l[[i]]), n)] <<- l[[i]])
  }
  ## append, row-wise, data frames in the list d to one another,
  ## save resulting data frame back into d
  d <- do.call("rbind", d)
  ## normalize variable names
  names(d) <- tolower(names(d))
  names(d) <- gsub("_", ".", names(d))
  names(d)[grep("^(user|id)$", names(d))[1]] <- "userid"
  ## omit extraneous variables
  d <- d[, !(names(d) %in% c("key", "variable.name")), drop = FALSE]
  ## keep only pilot users
  if ("userid" %in% names(d)) {
    d <- subset(d, grepl("heartsteps.test[0-9]+", userid))
    d$user <- as.numeric(gsub("(heartsteps\\.test|@gmail.*$)", "", d$userid))
  }
  ## l indicates character variables representing date-times by name
  ## (e.g. 'time.finished', 'time.started', 'time.stamp', 'time.updated', ...)
  l <- grepl("(^time\\.(fin|sta|up)|\\.(date|)time$)", names(d))
  if (any(l)) {
    l <- c(l, FALSE, FALSE)
    ## add offset in seconds from GMT/UTC, time zone identifier
    d$gmtoff <- 0
    d$tz <- if (is.null(d$timezone)) ""
            else d$timezone
    if (!is.null(d$utc.to.local.delta)) {
      d$gmtoff <- 60 * d$utc.to.local.delta
      d$tz <- paste("Etc/GMT", c("-", "+")[pmax(1, sign(d$gmtoff) + 1)],
                    formatC(abs(d$gmtoff) / 60^2), sep = "")
      d$tz[is.na(d$gmtoff)] <- ""
    }
    ## Unix time in UTC
    if (utime) {
      u <- do.call("data.frame",
                   mapply(char2utime, x = d[, l, drop = FALSE],
                          offset = d[, names(d) == "gmtoff", drop = FALSE],
                          SIMPLIFY = FALSE))
      names(u) <- gsub("(date|)time", "utime", names(u))
      d <- cbind(d, u)
    }
    ## selected POSIXlt elements in time zone/DST specified by tz
    if (ptime) {
      p <- do.call("data.frame",
                   mapply(char2calendar, x = d[, l, drop = FALSE],
                          tz = d[, names(d) == "tz", drop = FALSE],
                          SIMPLIFY = FALSE))
      d <- cbind(d, p)
    }
  }
  ## sort by given list of variables
  order.by <- substitute(order.by)
  order.by <- eval(order.by, d)
  if (!is.null(order.by))
    d <- d[do.call("order", order.by), ]
  row.names(d) <- NULL
  d
}
