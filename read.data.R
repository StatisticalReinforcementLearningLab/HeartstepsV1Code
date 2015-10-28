## read csv file(s) into a data frame

read.data <- function(file, order.by = NULL, ...) {
  ## Read all files in, store each as an element in a list d
  d <- sapply(file, read.csv, header = TRUE, strip.white = TRUE, ...,
              simplify = FALSE)
  
  ## If more than 1 file name is supplied, extract unique columns
  ## from both and create a single data.frame containing all entries
  ## (Used if dataset is split over multiple files)
  if (length(d) > 1) {
    l <- d
    n <- unique(unlist(lapply(d, names)))
    d <- lapply(l, function(x) data.frame(matrix(NA, nrow(x), length(n),
                                                 dimnames = list(NULL, n))))
    sapply(1:length(l), function(i) d[[i]][, match(names(l[[i]]), n)] <<- l[[i]])
  }
  d <- do.call("rbind", d)
  names(d) <- tolower(names(d))
  names(d) <- gsub("_", ".", names(d))
  d <- d[, names(d) != "key", drop = FALSE]
  ## keep only pilot users
  names(d)[names(d) %in% c("user", "id")] <- "userid"
  if ("userid" %in% names(d)) {
    d <- subset(d, grepl("heartsteps.test[0-9]", userid, perl = TRUE))
    d$user <- as.numeric(gsub("(heartsteps\\.test|@gmail.*$)", "", d$userid,
                              perl = TRUE))
  }
  ## add offset in seconds from GMT/UTC, time zone identifier
  l <- grepl("(^time\\.(fin|sta|up)|\\.(date|)time$)", names(d), perl = TRUE)
  if (any(l)) {
    l <- c(l, FALSE, FALSE)
    d$gmtoff <- 0
    d$tz <- if (is.null(d$timezone)) ""
            else d$timezone
    if (!is.null(d$utc.to.local.delta)) {
      d$gmtoff <- 60 * d$utc.to.local.delta
      d$tz <- paste("Etc/GMT", c("-", "+")[pmax(1, sign(d$gmtoff) + 1)],
                    formatC(abs(d$gmtoff) / 60^2), sep = "")
      d$tz[is.na(d$gmtoff)] <- ""
    }
    ## all Unix times, POSIXlt elements
    u <- do.call("data.frame",
                 mapply(char2utime, x = d[, l, drop = FALSE],
                        offset = d[, ncol(d) - 1, drop = FALSE],
                        SIMPLIFY = FALSE))
    names(u) <- gsub("(date|)time", "utime", names(u), perl = TRUE)
    p <- do.call("data.frame",
                 mapply(char2calendar, x = d[, l, drop = FALSE],
                        tz = d[, ncol(d), drop = FALSE], SIMPLIFY = FALSE))
    d <- cbind(d, u, p)  }
  ## sort by given list of variables
  order.by <- substitute(order.by)
  order.by <- eval(order.by, d)
  if (!is.null(order.by))
    d <- d[do.call("order", order.by), ]
  row.names(d) <- NULL
  d
}
