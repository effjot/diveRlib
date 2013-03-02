#### Library for processing van Essen / Schlumberger Diver MON files

### Data I/O

mon.skip.to.records <- 53  # Attention, magic number!
                           # Line where the real data records start

mon.datetime.format <- "%Y/%m/%d %H:%M:%S"

mon.line.sep <- "\r\n"     # MON files have DOS/Windows newlines


## Read a single MON file, return a list with header as text lines and
## data with timestamps converted to POSIXct type
read.mon.complete <- function(filename) {
  cat("Reading ", filename, ".\n", sep = "")
  header <- readLines(filename, n = mon.skip.to.records)
  data <- read.table(filename, header = FALSE, skip = mon.skip.to.records,
                     comment.char = "E", # skip last line "END OF DATA FILE"
                     col.names = c("date", "time", "h", "temp"))
  data$t <- strptime(paste(data$date, data$time), format="%Y/%m/%d %H:%M:%S")
  list(data = data[, c("t", "h", "temp")], unparsed.header = header)
}


## Read a single MON file, return only the dataframe
read.mon <- function(filename) {
  read.mon.complete(filename)$data
}


## Read all MON files starting with basename in alphabetical order and
## return as a single dataframe
read.all.mon <- function(dir, basename) {
  WD <- setwd(dir)
  x <- do.call(rbind,                      # combine into single dataframe
               lapply(sort(list.files(".", # read files in order
                                      pattern = paste("^", basename,
                                        ".+\\.(MON|mon)$", sep=""))),
                      FUN = read.mon))
  setwd(WD)
  x
}


write.mon.complete <- function(filename, mon) {
  df <- mon$data[c("h", "temp")]
  df$timestamp <- strftime(mon$data$t, format = mon.datetime.format)

  con <- file(filename, "wb")           # write binary to ensure CRLF newlines
  writeLines(but.last(mon$unparsed.header), con, sep = mon.line.sep)
  cat(nrow(df), mon.line.sep, file = con, sep = "")
  write.table(df[c("timestamp", "h", "temp")], file = con, eol = mon.line.sep,
               quote = FALSE, row.names = FALSE, col.names = FALSE)
  close(con)
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

t.range.year <- function(year) {
    t.range(paste("01.01.", c(year, year+1), " 00:00", sep=""))
}

t.range.quarter <- function(year, quarter) {
    from <- paste("01.", (quarter-1)*3 + 1, ".", year, " 00:00", sep="")
    to   <- paste("01.", (quarter*3 + 1) %% 12,  ".",
                  c(rep(year, 3), year+1)[quarter], " 00:00", sep="")
    t.range(c(from, to))
}

t.range.month <- function(year, month) {
    t.range(paste("01.", c(month, month%%12 + 1), ".",
                  c(year, c(rep(year, 11), year+1)[month]),
                  " 00:00", sep=""))
}

t.range.week.of.month <- function(year, month, week, last.week.full = TRUE) {
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
  
