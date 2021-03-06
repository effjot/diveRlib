#### Library for processing van Essen / Schlumberger Diver MON files

diveRlib.ID.string <- "diveRlib, v0.1, 2013-03-04" # used (optionally) for writing header data


### Data I/O

## Variables / configuration

mon.max.header.lines <- 70 # Maximum number of lines read for finding
                           # end of header / beginning of data

mon.upcase.location <- TRUE # Convert location to upper case (Diver Office downcases everything)

mon.header.datetime.format <- "%S:%M:%H %d/%m/%y"
mon.fileinfo.date.format <- "%d/%m/%Y"
mon.fileinfo.time.format <- "%H:%M:%S"
mon.data.datetime.format <- "%Y/%m/%d %H:%M:%OS"

mon.timezone <- "Etc/GMT-1" # Attention, here "-" is east of GMT!
                            # You may also want to set the TZ
                            # env. variable for displaying timestamps
                            # accordingly

mon.line.sep <- "\r\n"     # MON files have DOS/Windows newlines

mon.write.sections <- c("Logger settings", "Channel 1", "Channel 2", "Channel 3", "Series settings", "Channel 1 from data header", "Channel 2 from data header", "Channel 3 from data header") # sections to write into new MON headers, in order


## Read a single MON file, return a list with header as text lines and
## data with timestamps converted to POSIXct type
read.mon.complete <- function(filename, upcase.location = mon.upcase.location,
                              dec = ".", unit = "auto") {

  cat("Reading ", filename, " with dec=", dec, sep = "")

  ## read and parse header sections
  header <- readLines(filename, n = mon.max.header.lines)
  data.header.line <- grep("[Data]", header, fixed = TRUE)
  header <- head(header, data.header.line - 1)
  hdr <- parse.header(header)

  ## logger location
  loc <- hdr$Series$Location
  if (upcase.location) loc <- toupper(loc)

  ## measurements compensated?
  comp <- hdr$FILEINFO$COMP.STATUS %in% c("Done", "Fertig", "Unvollst.")

  ## get parameters and number format (decimal separator, units)
  dec.and.units <- extract.dec.and.units(hdr)
  if (dec == "auto") {
    dec <- dec.and.units$dec[1]
    cat(" (using ", dec, ")", sep = "")
  }

  data.cols <- dec.and.units$param
  cat(", parameters", data.cols)

  ## read data
  data <- read.table(filename, header = FALSE, skip = data.header.line + 1,
                     comment.char = "E", # skip last line "END OF DATA FILE"
                     dec = dec,
                     col.names = c("date", "time", data.cols))
  data$t <- as.POSIXct(strptime(paste(data$date, data$time),
                                format = mon.data.datetime.format,
                                tz = mon.timezone))

  ## convert water column to metres, if necessary
  if (unit == "auto") {
    unit <- dec.and.units[dec.and.units$param == "wc", "unit"]
  }
  wc.factor <- switch(unit,
                      keep = 1.0,
                      m = 1.0,
                      cm = 0.01,
                      NULL)
  if (is.null(wc.factor)) stop("Unknown unit for water column: ", unit)
  cat(", unit=", unit, "\n", sep = "")
  data$h <- wc.factor * data$wc
  list(loc = loc, comp = comp, data = data[, c("t", data.cols, "h")],
       hdr = hdr, unparsed.header = header)
}


## Read a single MON file, return only the dataframe
read.mon <- function(filename, dec = ".") {
  read.mon.complete(filename, dec = dec)$data
}


## Read several MON files, return list of dataframes
read.mons <- function(filenames, dec = ".") {
  lapply(filenames, FUN = read.mon, dec = dec)
}

## Read several MON files, return list of complete MON structures
read.mons.complete <- function(filenames, dec = ".") {
  lapply(filenames, FUN = read.mon.complete, dec = dec)
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
  li$key <- gsub(" +", " ", li$key)     # remove repeated spaces
  li[!li$is.sec.name, "val"] <- sub("^[[:blank:]]*[^=]+=(.+)", "\\1",
                                    ini[!li$is.sec.name])

  ## assign corresponding section names to rows containing key/value pairs
  ## (based on a solution by Earl F. Glynn,
  ##  https://stat.ethz.ch/pipermail/r-help/2007-June/134110.html)
  li$section <- li$section[which(li$is.sec.name)[cumsum(li$is.sec.name)]]


  ## combine both parts, trim whitespace from keys and values (section
  ## is already trimmed)
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


## Get units of all channels from the "Series settings" part of the
## parsed header
extract.dec.and.units <- function(header) {
  channel.info <- header[paste(
    "Channel",
    1:as.integer(header$`Logger settings`$`Number of channels`),
    "from data header")]

  x <- lapply(channel.info,
              FUN = function(ch) {
                kind <-
                  if (grepl("pegel|wasserstand|pressure|level",
                            ch$Identification, ignore.case = TRUE)) {
                    "wc"
                  } else if (grepl("temperatur", ch$Identification,
                                   ignore.case = TRUE)) {
                    "temp"
                  } else if (grepl("leitf|cond", ch$Identification,
                                   ignore.case = TRUE)) {
                    "cond"
                  } else NULL
                if (!is.null(kind)) {
                  pieces <- strsplit(ch$`Reference level`, " +")
                  dec <- if (grepl(".", pieces[1], fixed = TRUE))
                    "." else ","
                  c(kind, dec, last(pieces)[[1]])
                } else NULL
              })

  df <- data.frame(row.names = paste("Channel", 1:length(x)),
                   param = sapply(x, FUN = nth(1)),
                   dec = sapply(x, FUN = nth(2)),
                   unit = sapply(x, FUN = last),
                   stringsAsFactors = FALSE)
  stopifnot(length(unique(df$dec)) == 1) # different decimal separators
  df
}


## Take complete MON data structure, updates header fields "Start
## date", "End date" and FILEINFO section.  Returns only the updated
## header structure, so you can feed the return value directly to
## format.header().  If no filename is given, the old one is kept
update.header <- function(mon, filename = NULL, t.col = "t") {
  start.end <- mon$data[c(1, nrow(mon$data)), "t"]
  mon$hdr[["Series settings"]][c("Start date / time", "End date / time")] <-
    strftime(start.end, format = mon.header.datetime.format,
             tz = mon.timezone)

  mon$hdr$FILEINFO$DATE <- strftime(Sys.time(), format=mon.fileinfo.date.format)
  mon$hdr$FILEINFO$TIME <- strftime(Sys.time(), format=mon.fileinfo.time.format)

  if (!is.null(filename))
    mon$hdr$FILEINFO$FILENAME <- chartr("/", "\\", filename)

  mon$hdr
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

  ## logger info part, equal-sign-separated; write out predefined sections
  ## ensure decimal separator is period in numbers
  lapply(intersect(mon.write.sections, names(header)), # only available sections
         FUN = function(sec) {
           nl(paste0("[", sec, "]"))
           df <- lst.to.df(header[[sec]])
           i <- tolower(df$key) %in%    # fields with numbers
                  c("range", "reference level", "master level", "altitude")
           df[i, "val"] <- gsub(",", ".", df[i, "val"])
           df$key <- pad(df$key, 26)
           write.table(df, sep = "=", col.names = FALSE, row.names = FALSE,
              file = con, eol = mon.line.sep, quote = FALSE)
         })

  ## return stored strings from file
  readLines(con)
}


## Write out MON file from complete MON structure.  Uses the unparsed.hdr;
## format it with format.header() and update.header() before if data has
## changed.
write.mon.complete <- function(filename, mon, h.digits = 1) {
  has.cond <- "cond" %in% colnames(mon$data)

  cols <-c("h", "temp", if (has.cond) "cond")
  df <- mon$data[cols]

  ## format time and numbers for output
  op <- options(digits.secs = 1)
  on.exit(options(op))
  df$t.fmt <- strftime(mon$data$t, format = mon.data.datetime.format,
                       tz = mon.timezone)
  df$h.fmt <- formatC(mon$data$h, format="f", digits = h.digits,
                      width = 12, drop0trailing = FALSE)
  df$temp.fmt <- formatC(mon$data$temp, format="f", digits = 2,
                         width = 11, drop0trailing = FALSE)
  if (has.cond)
    df$cond.fmt <- formatC(mon$data$cond, format="f", digits = 3,
                         width = 11, drop0trailing = FALSE)

  con <- file(filename, "wb")           # write binary to ensure CRLF newlines
  on.exit(close(con), add = TRUE)

  ## helper function: print with proper newline
  nl <- function(string = "") { cat(string, mon.line.sep, sep = "", file = con) }

  writeLines(mon$unparsed.header, con, sep = mon.line.sep)

  nl(); nl()
  nl("[Data]"); nl(nrow(df))
  write.table(df[c("t.fmt", "h.fmt", "temp.fmt",
                   if (has.cond) "cond.fmt")],
              sep = " ", file = con, eol = mon.line.sep,
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  nl("END OF DATA FILE OF DATALOGGER FOR WINDOWS")
}


## Join list of several logger dataframes into a single dataframe in
## the given order (does not yet check if they are chronological)
join.data <- function(dataframes) {
  do.call(rbind, dataframes)
}


## Read diver geometry (installation depths etc.) from Excel-exported CSV
read.diver.geometry <- function(filename,
                                col.names = c("loc", "date", "time",
                                  "l", "h.0", "serial", "comment"),
                                date.format = "%d.%m.%Y",
                                time.format = "%H:%M",
                                unit = "cm") {
  stopifnot(c("loc", "date", "time", "l", "h.0") %in% col.names)
  stopifnot(unit %in% c("cm", "m"))
  geo <- read.csv2(filename, as.is = TRUE, col.names = col.names)
  geo[geo$time == "", "time"] <- "12:00"
  geo$t <- as.POSIXct(strptime(paste(geo$date, geo$time),
                               format = paste(date.format, time.format)))
  if (unit == "cm")
    geo <- transform(geo, l = l/100, h.0 = h.0/100)

  geo[c("loc", "t", "h.0", "l")]
}


## Calculate absolute head (water level) from water column and logger geometry (both as list of all zoos / all dataframes)
calc.abs.head <- function(wc, geometry, cutoff = FALSE) {
  geo <- zoo(geometry[c("h.0", "l")], geometry$t)
  merged.full <- merge(wc, geo)
  # na.locf only on the geometry columns, to protect NAs in logger data
  merged.geo.filled <- na.locf(merged.full[, c("h.0", "l")])
  # logger data with geometry only at logged times
  z <- merge(wc, merged.geo.filled, all = c(TRUE, FALSE))
  if (cutoff) {  # can't use transform, as it doesn't see cutoff variable
    z$h <- ifelse.zoo(z$wc < (z$l - cutoff), z$h.0 - z$l + z$wc, NA)
  } else {
    z$h <- z$h.0 - z$l + z$wc
  }
  z
}


## Read manual measurements (absolute water levels) from GeODin-exported CSV
read.manual.meas <- function(filename,
                             col.names = c("loc", "fullname",
                               "elev", "borehole.depth", "invid", "invtype",
                               "smpid", "smpname", "date", "time", "parsmpid",
                               "geodinguid", "h"),
                             date.format = "%d.%m.%Y",
                             time.format = "%H:%M",
                             unit = "m") {
  stopifnot(c("loc", "date", "time", "h") %in% col.names)
  stopifnot(unit %in% c("cm", "m"))
  man <- read.csv(filename, as.is = TRUE, col.names = col.names)
  man[man$time == "", "time"] <- "12:00"
  man$t <- as.POSIXct(strptime(paste(man$date, man$time),
                               format = paste(date.format, time.format)))
  if (unit == "cm")
    man$h <- man$h/100

  man[c("loc", "t", "h")]
}



### General

between <- function(i, range) {
  i >= range[1] & i <= range[2]
}

first <- function(x) {
  head(x, 1)
}

last <- function(x) {
  tail(x, 1)
}

but.last <- function(x) {
  head(x, n = -1)
}

first.last <- function(x) {
  if (is.data.frame(x)) {
    x[c(1, nrow(x)), ]
  } else {
    c(head(x, 1), tail(x, 1))
  }
}

nth <- function(n) {
  function(x) x[n]
}

even.numbered.elements <- function(v) {
  v[seq(along.with = v) %% 2 == 0]
}
odd.numbered.elements <- function(v) {
  v[seq(along.with = v) %% 2 == 1]
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


## return a data.frame with renamed columns
rename.col <- function(df, name.old, name.new) {
  stopifnot(length(name.old) == length(name.new))
  if (length(name.old) == 1) {
    colnames(df)[colnames(df) == name.old] <- name.new
  } else {
    # vectorizing with "==" or "%in%" fails when name.old not in same order as colnames
    col.new <- sapply(colnames(df),
                      FUN = function(cn) {
                        if (cn %in% name.old) { name.new[name.old == cn] } else cn
                      })
    colnames(df) <- col.new
  }
  df
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


STYLE <-
  list(data  = list(col = "grey", lwd.normal = 1, lwd.minmax = 0.5),
       mean  = list(col = "red", lwd = 2),
       day   = list(col = "aquamarine", lwd = 2),
       night = list(col = "royalblue3", lwd = 2),
       quant = list(col = "grey25", lwd.normal = 1, lwd.minmax = 0.8),
       grid  = list(col = "black", lwd = 0.5, lty = "dotted"),
       grid.major = list(col = "black", lwd = 0.5, lty = "solid"))

plot.axis.grid.year <- function(t.range, style = STYLE) {
    axis.POSIXct(1, at=seq(t.range[1], t.range[2], by="months"),
                 format="%d.%m.")
    abline(h = seq(0, 40, by = 5),
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
    abline(v = seq(t.range[1], t.range[2], by="months"),
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
}

plot.axis.grid.years <- function(t.range, short.month.labels = TRUE,
                                 style = STYLE) {
    month.seq <- seq(t.range[1] + 365.26/12*86400*0.5,
                     t.range[2] - 365.26/12*86400*0.5, by = "months")
    month.labels <- format(month.seq, format = "%b")
    if (short.month.labels)
      month.labels <- substr(month.labels, 1, 1)

    axis.POSIXct(1, at=seq(t.range[1], t.range[2], by = "months"),
                 labels = FALSE)
    mtext(month.labels, side = 1, line = 0.8, at = month.seq)

#    year.seq <- seq(t.range[1] + 182.6*86400,
#                    t.range[2] - 182.6*86400, by = "years")
    year <- as.POSIXlt(t.range)$year + 1900

## Hack für Juli 2010    year[2] <- year[2]+1

    year.seq <- seq(ISOdate(year[1] - 1, 1, 1),
                    ISOdate(year[2] + 1, 1, 1), by = "years")

    year.label.seq <- seq(ISOdate(year[1], 7, 1),
                          ISOdate(year[2] - 1, 7, 1), by = "years")

    ## axis.POSIXct(1, at=seq(t.range[1], t.range[2], by = "years"),
    ##              labels = FALSE, tck = -0.04)
    axis.POSIXct(1, at=year.seq, labels = FALSE, tck = -0.04, lwd.ticks = 1.4)
    mtext(format(year.label.seq, format="%Y"),
          side = 1, line = 2.5, cex = 1.5, at = year.label.seq)

    abline(h = seq(0, 40, by = 5),
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
    abline(v = seq(t.range[1], t.range[2], by = "months"),
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
}


plot.axis.grid.month <- function(t.range, style = STYLE) {
    axis.POSIXct(1, at=seq(t.range[1], t.range[2], by="days"), format="%d")
    abline(h = seq(0, 30, by = 5),
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
    v.lines <- seq(t.range[1], t.range[2], by = "days")
    abline(v = v.lines[as.POSIXlt(v.lines)$wday != 1],
           col = style$grid$col, lty = style$grid$lty, lwd = style$grid$lwd)
    abline(v = v.lines[as.POSIXlt(v.lines)$wday == 1],
           col = style$grid.major$col, lty = style$grid.major$lty,
           lwd = style$grid.major$lwd)
}



## Shifting timestamps (arbitrary and for removing daylight saving time)

shift.time <- function(df, offset.seconds, t.col = "t") {
  df[[t.col]] <- df[[t.col]] + offset.seconds
  df
}

daylightsaving.to.standard.time <- function(df, t.col = "t") {
  shift.time(df, -3600, t.col)
}


## Return dataframe without measurements before/after logger was installed
## (i.e. head ≈ atm. pressure) As default, compare to single threshold
## value, which should work if the water colum above to logger wasn't
## too small.  It is also possible to supply a vector, e.g. baro
## logger values, to compare to.
trim.out.of.water <- function(df, h.min = 10.60, h.col = "h",
                              what = "before") {
  first.in.water <- if (what %in% c("before", "both")) {
    which(df[h.col] > h.min)[1]
  } else 1
  last.in.water <- if (what %in% c("after", "both")) {
    last(which(df[h.col] > h.min))
  } else nrow(df)
  df[first.in.water:last.in.water, ]
}


## Replace out of water measurements with NA.  Compare to threshold
## value (or vector, e.g. from baro logger) h.min; anything less or
## equal than the threshold is assumed out of water.  Has S3 methods
## for data.frame and zoo

out.of.water.as.NA <- function(x, h.min = 10.60, ...) {
    class(x) <- data.class(x)
    UseMethod("out.of.water.as.NA", x)
}

out.of.water.as.NA.data.frame <- function(x, h.min = 10.60, h.col = "h") {
  out.of.water <- which(x[h.col] <= h.min)
  x[out.of.water, h.col] <- NA
  x
}

out.of.water.as.NA.zoo <- function(x, h.min = 10.60) {
  out.of.water <- which(x <= h.min)
  x[out.of.water] <- NA
  x
}


## Fill in gaps (i.e. logger was not running, no records produced) in
## weakly regular zoo time series, using na.approx
## (lin. interpolation) as default.  Result is a regular time series.
fill.gaps.zoo <- function(x, FUN = na.approx) {
  stopifnot(is.regular(x, strict = FALSE))
  t <- time(temp.zoo)
  xout <- seq(t[1], last(t), by = deltat(x))
  as.zooreg(do.call(FUN, list(x, xout = xout)))
}


## Set values within time range to NA
set.range.to.NA <- function(x, t.range, cols = c("wc", "h"), na.value = NA) {
  stopifnot(length(t.range) == 2)
  window(x, start = t.range[1], end = t.range[2])[, cols] <-
    rep(na.value, length(cols))
  x
}


### Barometric compensation and water level calculation

## Constants and utilities

grav <- 9.81                      # gravity of Earth, in N/kg
rho.w.10 <- 0.99974               # density of water at 10°C, in kg/m³

rho.w <- function(temp) {
  1/(1+((2.31*temp-2)^2-182)*0.000001)
}

## Compensate raw water colum data (zoo) with baro measurements (zoo)
baro.comp <- function(w.raw, baro) {
  stopifnot(is.zoo(w.raw), is.zoo(baro))
  b <- na.approx(baro, xout = time(w.raw)) # interpol. missing baro data
  m <- merge(w.raw, b)
  in.water <- m[which(m$w > m$b + 0.1)]
  in.water$w.raw - in.water$b
}


### Plotting

## Compare logger and manually measured water levels
plot.comparison <- function(loc, logger.zoo, manual.zoo, ...) {
  logger <- na.trim(logger.zoo[, loc])
  plot(logger, xlim = c(start(logger), end(logger)),
       col = "grey",
       main = paste("Time series of logger data and manual measurements for",
         loc), ...)
  points(manual.zoo[, loc], col = "red", ...)
}

## Correlation plot between logger and manual measurements
plot.correlation <- function(loc, logger.zoo, manual.zoo,
                             tolerance = 0.02, ...) {
  merged <- na.omit(merge(logger = logger.zoo[, loc],
                          man = manual.zoo[, loc], all = FALSE))
  plot(merged$logger, merged$man,
       main = paste("Logger data vs. manual measurements for", loc),
       xlab = "Logger water levels", ylab = "Manual measurements",
       ...)
  abline(0, 1)
  abline(+tolerance, 1, col = "grey")
  abline(-tolerance, 1, col = "grey")
  invisible(merged)
}
