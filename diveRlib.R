#### Library for processing van Essen / Schlumberger Diver MON files

### Data I/O

read.mon <- function(filename) {
  cat("Reading ", filename, ".\n", sep = "")
  x <- read.table(filename, header = FALSE, skip = 53, # Attention, magic number!
                    comment.char = "E", # skip last line "END OF DATA FILE"
                    col.names = c("date", "time", "h", "temp"))
  x$t <- strptime(paste(x$date, x$time), format="%Y/%m/%d %H:%M:%S")
  x[, c("t", "h", "temp")]
}

read.all.mon <- function(dir, basename) {
  WD <- setwd(dir)
  x <- do.call(rbind,
               lapply(sort(list.files(".",
                                      pattern = paste("^", basename, ".+\\.(MON|mon)$", sep=""))),
                      FUN = read.mon))
  setwd(WD)
  x
}


### General

between <- function(i, range) {
    i >= range[1] & i <= range[2]
}

but.last <- function(x) {
    head(x, n = -1)
}

paste.path <- function(...) {
  paste(..., sep = "/")
}


### Time and time series

as.Date.ymd <- function(year, month, day) {
  as.Date(paste(year, month, day, sep = "-"))
}

first.day.of.next.month <- function(year, month) {
  seq(as.Date.ymd(year, month, 1),
      length.out = 2, by = "months")[2]
}

last.day.of.month <- function(year, month) {
  first.day.of.next.month(year, month) - 1
}


splicein.NA <- function(df, t) {
	splice <- data.frame(matrix(NA, nrow = length(t), ncol=ncol(df)))
	names(splice) <- names(df)
	splice$t <- t
	rbind(df[df$t < min(t), ], splice, df[df$t > max(t), ])
}

t.range <- function(from.to, format = "%d.%m.%Y %H:%M") {
    stopifnot(length(from.to) == 2)
    as.POSIXct(strptime(from.to, format = format))
}

make.t.range.year <- function(year) {
    t.range(paste("01.01.", c(year, year+1), " 00:00", sep=""))
}

make.t.range.quarter <- function(year, quarter) {
    from <- paste("01.", (quarter-1)*3 + 1, ".", year, " 00:00", sep="")
    to   <- paste("01.", (quarter*3 + 1) %% 12,  ".",
                  c(rep(year, 3), year+1)[quarter], " 00:00", sep="")
    t.range(c(from, to))
}

make.t.range.month <- function(year, month) {
    t.range(paste("01.", c(month, month%%12 + 1), ".",
                  c(year, c(rep(year, 11), year+1)[month]),
                  " 00:00", sep=""))
}

make.t.range.week.of.month <- function(year, month, week, last.week.full = TRUE) {
  stopifnot(between(week, c(1, 5)))
  start.day <- (week - 1)*7 + 1
  if (between(week, c(1, 4))) {
    t.range(paste(c(start.day, start.day + 7), ".", month, ".", year,
                  " 00:00", sep=""))
  } else {
    if (last.week.full) {
      end.date <- as.Date.ymd(year, month, start.day) + 7
    } else {
      end.date <- first.day.of.next.month(year, month)
    }
    c(as.Date.ymd(year, month, start.day), end.date)
  }
}


shift.time <- function(df, offset.seconds, t.col = "t") {
  df[[t.col]] <- df[[t.col]] + offset.seconds
  df
}

daylightsaving.to.standard.time <- function(df, t.col = "t") {
  shift.time(df, -3600, t.col)
}
