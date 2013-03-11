#### Library for processing van Essen / Schlumberger Diver MON files

diveRlib.ID.string <- "diveRlib, v0.1, 2013-03-04" # used (optionally) for writing header data


### Data I/O

mon.max.header.lines <- 70 # Maximum number of lines read for finding
                           # end of header / beginning of data

mon.header.datetime.format <- "%S:%M:%H %d/%m/%y"
mon.data.datetime.format <- "%Y/%m/%d %H:%M:%OS"

mon.timezone <- "Etc/GMT-1" # Attention, here "-" is east of GMT!

mon.line.sep <- "\r\n"     # MON files have DOS/Windows newlines

mon.write.sections <- c("Logger settings", "Channel 1", "Channel 2", "Series settings", "Channel 1 from data header", "Channel 2 from data header") # sections to write into new MON headers, in order


## Read a single MON file, return a list with header as text lines and
## data with timestamps converted to POSIXct type
read.mon.complete <- function(filename) {
  cat("Reading ", filename, "\n", sep = "")

  ## read and parse header sections
  header <- readLines(filename, n = mon.max.header.lines)
  data.header.line <- grep("[Data]", header, fixed = TRUE)
  header <- head(header, data.header.line - 1)
  hdr <- parse.header(header)

  ## measurements compensated?
  comp <- hdr$FILEINFO$COMP.STATUS == "Done"

  ## read data
  data <- read.table(filename, header = FALSE, skip = data.header.line + 1,
                     comment.char = "E", # skip last line "END OF DATA FILE"
                     col.names = c("date", "time", "h", "temp"))
  data$t <- strptime(paste(data$date, data$time), format = mon.data.datetime.format,
                     tz = mon.timezone)

  list(data = data[, c("t", "h", "temp")], hdr = hdr, comp = comp,
       unparsed.header = header)
}


## Read a single MON file, return only the dataframe
read.mon <- function(filename) {
  read.mon.complete(filename)$data
}


## Read all MON files starting with basename in alphabetical order and
## return as a single dataframe
read.mon.all <- function(dir, basename) {
  WD <- setwd(dir)
  x <- do.call(rbind,                      # combine into single dataframe
               lapply(sort(list.files(".", # read files in order
                                      pattern = paste("^", basename,
                                        ".+\\.(MON|mon)$", sep=""))),
                      FUN = read.mon))
  setwd(WD)
  x
}


parse.header <- function(unparsed.header) {
  unparsed <- unparsed.header[unparsed.header != ""] # strip empty lines

  ## separate file info "banner" from INI-style part with logger info
  separation <- grep("===[[:blank:]]*BEGINNING OF DATA", unparsed)
  file.info.unparsed <- unparsed[3:(separation-1)] # first two lines are decoration
  ini <- unparsed[(separation+1):length(unparsed)]

  ## file info part: split at colon
  file.info <- data.frame(key = sub("^[[:space:]]*([^:]+):.+", "\\1",
                            file.info.unparsed), stringsAsFactors = FALSE)
  file.info$val <- sub("^[[:space:]]*[^:]+:(.+)", "\\1", file.info.unparsed)


  ## logger info part: find lines with INI-style section headings ("[section]")
  ## and extract names
  li <- data.frame(is.sec.name = grepl("^[[:blank:]]*\\[.+\\]", ini))

  li[li$is.sec.name, "section"] <- sub("^[[:blank:]]*\\[(.+)\\]", "\\1",
                                   ini[li$is.sec.name])

  ## then, the other lines are key=val pairs
  li[!li$is.sec.name, "key"] <- sub("^[[:blank:]]*([^=]+)=.+", "\\1",
                                    ini[!li$is.sec.name])
  li[!li$is.sec.name, "val"] <- sub("^[[:blank:]]*[^=]+=(.+)", "\\1",
                                    ini[!li$is.sec.name])

  ## assign corresponding section names to rows containing key/value pairs
  ## (based on a solution by Earl F. Glynn,
  ##  https://stat.ethz.ch/pipermail/r-help/2007-June/134110.html)
  li$section <- li$section[which(li$is.sec.name)[cumsum(li$is.sec.name)]]


  ## combine both parts, trim whitespace from keys and values (section is alread
  df <- data.frame(section = c(rep("FILEINFO", nrow(file.info) + 1),
                                        # + 1 to generate empty row for section name
                     trim(li$section)),
                   key = trim(c("", file.info$key, li$key)),
                   val = trim(c("", file.info$val, li$val)),
                   stringsAsFactors = FALSE)

  ## transform to nested list (header$section$key)
  sapply(split(df, df$section),
         FUN = function(sec) {
           val        <- as.list(sec[-1, "val"])
           names(val) <- sec[-1, "key"]
           val
         })
}


format.header <- function(header, created.by.diveRlib = FALSE) {
  if (created.by.diveRlib)
    header$FILEINFO$`CREATED BY` <- diveRlib.ID.string

  con <- file("", "w+b")                # build strings in temp file
  on.exit(close(con))

  ## helper function: print with proper newline
  nl <- function(string) { cat(string, mon.line.sep, sep = "", file = con) }

  ## helper function: convert key/value list to dataframe
  lst.to.df <- function(sec, pad.key) {
    data.frame(key = names(sec), val = unlist(sec),
               stringsAsFactors = FALSE, row.names = NULL)
  }


  ## file info part with headings/"banner"; data colon-separated
  nl("Data file for DataLogger.")
  nl(str.dup("=", 78))

  df <- lst.to.df(header$FILEINFO)
  df$key <- pad(df$key, 11)
  write.table(df, sep = ": ", col.names = FALSE, row.names = FALSE,
              file = con, eol = mon.line.sep, quote = FALSE)

  nl(c(str.dup("=", 26), "    BEGINNING OF DATA     ", str.dup("=", 26)))

  ## logger info part, equal-sign-separated; write out predfined sections
  lapply(mon.write.sections,
         FUN = function(sec) {
           nl(paste0("[", sec, "]"))
           df <- lst.to.df(header[[sec]])
           df$key <- pad(df$key, 26)
           write.table(df, sep = "=", col.names = FALSE, row.names = FALSE,
              file = con, eol = mon.line.sep, quote = FALSE)
         })

  ## return stored strings from file
  readLines(con)
}


write.mon.complete <- function(filename, mon) {
  df <- mon$data[c("h", "temp")]

  ## format time and numbers for output
  op <- options(digits.secs = 1)
  on.exit(options(op))
  df$t.fmt <- strftime(mon$data$t, format = mon.data.datetime.format,
                       tz = mon.timezone)
  df$h.fmt <- formatC(mon$data$h, format="f", digits = 1,
                      width = 12, drop0trailing = FALSE)
  df$temp.fmt <- formatC(mon$data$temp, format="f", digits = 2,
                         width = 11, drop0trailing = FALSE)

  con <- file(filename, "wb")           # write binary to ensure CRLF newlines
  on.exit(close(con), add = TRUE)

  ## helper function: print with proper newline
  nl <- function(string = "") { cat(string, mon.line.sep, sep = "", file = con) }

  writeLines(mon$unparsed.header, con, sep = mon.line.sep)

  nl(); nl()
  nl("[Data]"); nl(nrow(df))
  write.table(df[c("t.fmt", "h.fmt", "temp.fmt")], sep = " ",
              file = con, eol = mon.line.sep,
               quote = FALSE, row.names = FALSE, col.names = FALSE)

  nl("END OF DATA FILE OF DATALOGGER FOR WINDOWS")
}



### General

between <- function(i, range) {
  i >= range[1] & i <= range[2]
}

last <- function(x) {
  tail(x, 1)
}

but.last <- function(x) {
  head(x, n = -1)
}

paste.path <- function(...) {
  paste(..., sep = "/")
}

## remove leading or trailing whitespace from string
trim <- function(string) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", string)
}

## duplicate strings, works with times as a vector
## taken from stringr package
str.dup <- function(string, times) {
  # Use dataframe to do recycling
  data <- data.frame(string, times)
  n <- nrow(data)
  string <- data$string
  times <- data$times

  vapply(seq_len(n), function(i) {
    paste(rep.int(string[i], times[i]), collapse = "")
  }, character(1))
}

## pad string to desired length with spaces to the right
pad <- function(string, len) {
  paste0(string, str.dup(" ", pmax(0, len - nchar(string))))
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


## Return dataframe without measurements before logger was installed
## (i.e. head ≈ atm. pressure) As default, compare to single threshold
## value, which should work if the water colum above to logger wasn't
## too small.  It is also possible to supply a vector, e.g. baro
## logger values, to compare to.
remove.out.of.water <- function(df, h.min = 1060, h.col = "h") {
  first.in.water <- which(df[h.col] > h.min)[1]
  df[first.in.water:nrow(df), ]
}
