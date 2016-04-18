#### Datenbereinigung der "rohen" MON-Dateien (nach Export vom
#### Loggerauslese-Laptop, aber vor Import in Diver Office zur
#### Barokompensation

library(zoo)

library(functional)
Compose <- function (...) {             # redefine for
  fs <- rev(list(...))                  # proper order
  if (!all(sapply(fs, is.function)))
    stop("Argument is not a function")
  function(...) Reduce(function(x, f) f(x), fs, ...)
}


### Setup

Sys.setenv(TZ="Etc/GMT-1")

base.dir <- "P:/2008_INKA-BB/Rohdaten/Datenlogger"


## do or skip time-consuming parts

do.fixfiles <- FALSE
do.readdata <- TRUE
do.read.manual.meas <- TRUE
do.compensation <- TRUE
do.abs.heads <- TRUE
do.clean <- TRUE


## correspondence between abbreviated and full names

location.numbers <- c(205, 206, 209, 210, 212, 213, 216, 220, 224)
location.fullnames <- c(paste0("GW-WAS-", location.numbers),
                             "LP-BRW-5OP", "Baro")
location.shortnames <- c(paste0("was", location.numbers),
                               "lp5ow", "baro")
location.names <- data.frame(shortname = location.shortnames,
                             fullname = location.fullnames)
rm(location.numbers)


## convenience

quarter <- as.difftime("0:15:00") # one quarter hour time difference



### Korrektur Zeitversatz (irrtmlich Sommerzeit)

if (do.fixfiles) {
  fix.file <- function(dir, basename) {
    filenames <- paste.path(dir, paste0(c("", "KORR_"), basename))
    x <- read.mon.complete(filenames[1], dec = "auto")
    x$data <- daylightsaving.to.standard.time(x$data)
    x$data <- trim.out.of.water(x$data)
    x$unparsed.header <- format.header(update.header(x, filenames[2]))
    write.mon.complete(filenames[2], x)
  }

  cur.dir <- paste.path(base.dir, "Bruchwald LN, Auslesung 2013-02-23")
  fix.file(cur.dir, "gw-was-206_130225124849_75778.MON")
  fix.file(cur.dir, "lp-brw-5op_130225125129_75779.MON")
  fix.file(cur.dir, "gw-was-212_130225124909_J5284.MON")
  fix.file(cur.dir, "gw-was-213_130225125016_N0144.MON")

  cur.dir <- paste.path(base.dir, "Bruchwald LN, Auslesung 2013-03-25+26")
  fix.file(cur.dir, "GW-WAS-206.MON")
  fix.file(cur.dir, "GW-WAS-212.MON")
  fix.file(cur.dir, "GW-WAS-213.MON")
  fix.file(cur.dir, "LP-BRW-5OP.MON")

  cur.dir <- paste.path(base.dir, "Bruchwald LN, Auslesung 2013-05-03")
  fix.file(cur.dir, "GW-WAS-220^J5298^13-05-03 14-02-33.MON")
}


### Read all data into data.frames and (list of) zoo objects

## Helper function: build complete filenames from fixed and variable parts;
## takes vector of date (i.e. subdir suffix) -- filename pairs

build.filenames <- function(dat.name, dir = base.dir,
                            subdir.basename = "Bruchwald LN, Auslesung",
                            ext = "MON") {
  paste.path(dir,
             paste(subdir.basename,
                   odd.numbered.elements(dat.name)),
             paste(even.numbered.elements(dat.name),
                   "MON", sep = "."))
}


## Baro timeseries

baro.all.files <- build.filenames(c(
  "2011-05-05", "Baro Bruchwald ULN",
  "2011-05-11", "Baro Bruchwald ULN",
  "2011-10-21", "Baro Bruchwald ULN",
  "2012-03-30", "Baro Bruchwald ULN",
  "2012-07-12", "baro bruchwald uln_120713112829_K1941",
  "2013-02-23", "baro bruchwald uln_130225124744_K1941",
  "2013-03-25+26", "Baro Bruchwald ULN",
  "2013-11-05+06", "Baro Bruchwald ULN^K1941^13-11-05 16-21-13",
  "2013-11-26", "Baro Bruchwald ULN^K1941^13-11-26 10-14-19"))


## When baro loggers had to be changed, for a time both loggers ran
## parallel. Limit to data from only one device.

fix.baro.overlap <- function(baro.df) {
  t.change <- which(baro.df$t == ISOdatetime(2011, 5, 5, 11, 45, 0))
  i <- c(1:t.change[1], (t.change[2]+1):nrow(baro.df))
  baro.df[i, ]
}

if (do.readdata) {
  baro.data <- Compose(fix.baro.overlap, join.data,
                       Curry(read.mons, dec = "auto"))(baro.all.files)

  baro.zoo <- zoo(baro.data$h, baro.data$t)
}


## Diver timeseries

diver.files <- lapply(
  list(was205 = c("2011-08-02", "GW-WAS-205",
         "2011-11-08", "GW-WAS-205",
         "2011-12-09", "GW-WAS-205",
         "2012-08-27", "GW-WAS-205",
         "2013-03-28+04-05", "GW-WAS-205",
         "2013-11-05+06", "GW-WAS-205^J9438^13-11-07 11-05-22"),
       was206 = c("2013-03-25+26", "KORR_GW-WAS-206",
         "2013-11-05+06", "GW-WAS-206^75778^13-11-06 19-43-09",
         "2013-11-26", "GW-WAS-206^75778^13-11-26 09-30-45"),
       was209 = c("2011-05-05", "GW-WAS-209 Location korrigiert",
         "2011-08-02", "GW-WAS-209",
         "2011-10-21", "GW-WAS-209",
         "2012-07-12", "gw-was-209_120713112908_H0997",
         "2013-02-23", "gw-was-209_130225124858_H0997",
         "2013-11-05+06", "GW-WAS-209^H0997^13-11-05 13-21-58",
         "2013-11-26", "GW-WAS-209^H0997^13-11-26 10-28-50"),
       was210 = c("2013-11-05+06", "GW-WAS-210^J4511^13-11-06 11-57-31",
         "2013-11-26", "GW-WAS-210^J4511^13-11-26 10-26-31"),
       was212 = c("2013-03-25+26", "KORR_GW-WAS-212",
         "2013-11-05+06", "GW-WAS-212^J5284^13-11-05 15-54-43"),
       was213 = c("2013-03-25+26", "KORR_GW-WAS-213",
         "2013-11-05+06", "GW-WAS-213^N0144^13-11-05 16-00-17"),
       was220 = c("2013-05-03", "KORR_GW-WAS-220^J5298^13-05-03 14-02-33",
         "2013-09-11", "GW-WAS-220^J5298^13-09-11 14-53-15",
         "2013-11-26", "GW-WAS-220^J5298^13-11-26 09-16-09"),
       was216 = c("2011-07-13", "GW-WAS-216",
         "2011-10-21", "GW-WAS-216",
         "2012-03-30", "GW-WAS-216",
         "2012-07-12", "gw-was-216_120713112941_F0432",
         "2013-02-23", "gw-was-216_130225125025_F0432",
         "2013-03-25+26", "GW-WAS-216",
         "2013-11-05+06", "GW-WAS-216^F0432^13-11-05 16-17-24"),
       was224 = c("2011-10-21", "GW-WAS-224",
         "2012-03-30", "GW-WAS-224",
         "2012-07-12", "gw-was-224_120713113201_G0181",
         "2013-02-23", "gw-was-224_130225125118_G0181",
         "2013-03-25+26", "GW-WAS-224",
         "2013-11-05+06", "GW-WAS-224^G0181^13-11-05 16-04-49"),
       lp5ow = c("2013-03-25+26", "KORR_LP-BRW-5OP",
         "2013-11-05+06", "LP-BRW-5OP^75779^13-11-06 11-36-32")
  ),
  FUN = build.filenames)


if (do.readdata) {
  diver.data <- lapply(diver.files,
                       Compose(join.data,
                               Curry(read.mons, dec = "auto")))
  diver.zoo <- lapply(diver.data,
                      Compose(out.of.water.as.NA,
                              function(x) zoo(x$h, x$t)))
  temp.zoo <- lapply(diver.data,
                     function(x) zoo(x$temp, x$t))
}


### Baro-compensation (only zoo)

if (do.compensation) {
  wat.col <- lapply(diver.zoo, Curry(baro.comp, baro = baro.zoo))
  wc <- do.call(merge, wat.col)
}


### Calculate absolute heads (".uncl" = unclean = no outliers etc. fixed)

if (do.abs.heads) {

  ## read records of geometry data
  diver.geometry.complete <- read.diver.geometry("p:/2008_INKA-BB/Bruchwald am LN/Datenlogger/Logger Einbau+Umbau+Prfung.csv", unit = "cm")

  ## add locations' fullnames; shortnames go into loc
  diver.geometry.complete <- transform(
    merge(diver.geometry.complete, location.names,
          by.x = "loc" , by.y = "fullname", all.x = TRUE, all.y = FALSE),
    fullname = loc, loc = as.character(shortname), shortname = NULL)

  ## split into list of dataframes (for each location), without baro
  diver.geometry <- split(diver.geometry.complete,
                          ifelse(diver.geometry.complete$loc == "baro",
                                 NA, diver.geometry.complete$loc))

  ## calculate heads for all divers
  wat.head.uncl <- mapply(FUN = calc.abs.head, wat.col,
                          diver.geometry[names(wat.col)], # ensure
                                                   # same ordering
                          MoreArgs = list(cutoff = 0.5))

  ## zoo of heads only (drop water column and geometry)
  h.uncl <- do.call(merge, lapply(wat.head.uncl, function(l) { l$h }))

}


### Cleanup, outliers, faulty data

if (do.clean) {

  wat.head <- wat.head.uncl

  ## GW-WAS-216

  # water level suddenly shifted up for some time
  start <- ISOdatetime(2012, 7, 12, 12, 30, 00)
  end   <- ISOdatetime(2012, 9,  9, 14, 30, 00)
  offsets <- zoo(c(1.007, 0.775), c(start, end))
  corr <- merge(window(wat.head.uncl$was216, start = start, end = end),
                offsets)
  corr$offsets <- na.approx(corr$offsets)
  corr$wc <- corr$wc - corr$offsets
  corr$h  <- corr$h  - corr$offsets
  window(wat.head$was216, start = start, end = end)[, c("wc", "h")] <-
    corr[, c("wc", "h")]

  # series of spikes to extreme values
  wat.head$was216 <- set.range.to.NA(wat.head$was216,
                                     c(ISOdatetime(2012,  9, 14, 10, 30, 00),
                                       ISOdatetime(2012,  9, 16, 20, 15, 00)))
  wat.head$was216 <- set.range.to.NA(wat.head$was216,
                                     c(ISOdatetime(2012,  9, 24,  0, 15, 00),
                                       ISOdatetime(2012, 11,  7, 15, 45, 00)))
  wat.head$was216 <- set.range.to.NA(wat.head$was216,
                                     c(ISOdatetime(2012, 11, 13, 19, 00, 00),
                                       ISOdatetime(2012, 11, 17,  3, 30, 00)))

  # some manual measurements for comparison
  man216 <- zoo(c(37.10, 37.05, 37.26, 37.20, 37.08, 37.03, 36.94, 36.91),
                ISOdatetime(2012, c(6, 6, 7, 7, 7, 8, 9, 9),
                            c(2, 16, 10, 12, 28, 11, 9, 22),
                            12, 00, 00))

  ## GW-WAS-220

  # times are offset by 7min 16s => interpolate for quarter hours
  start <- ISOdatetime(2013,  9, 11, 13, 45, 00)
  end   <- ISOdatetime(2013, 11, 26,  9, 15, 00)
  regular.times <- seq(start, end, by = "15 min")
  orig <- window(wat.head$was220, start = start, end = end)
  interpol <- na.approx(orig, xout = regular.times, na.rm = FALSE)
  before <- window(wat.head$was220, start = start(wat.head$was220),
                   end = start - quarter)
  after  <- window(wat.head$was220, start = end + quarter,
                   end = end(wat.head$was220))
  wat.head$was220 <- rbind(before, interpol, after)


  ## zoo of corrected heads only (drop water column and geometry)
  h <- do.call(merge, lapply(wat.head, function(l) { l$h }))

}


if (do.read.manual.meas) {

  ## read manual measurements
  manual.gw.complete <- read.manual.meas("p:/2008_INKA-BB/Bruchwald am LN/Datenlogger/Handmessungen alle GWMS.csv")
  manual.surf.complete <- read.manual.meas("p:/2008_INKA-BB/Bruchwald am LN/Datenlogger/Handmessungen alle OWMS.csv")

  ## add locations' fullnames; GW shortnames are in loc (but upper case)
  man.gw <- manual.gw.complete
  man.gw$loc = tolower(man.gw$loc)
  man.gw <- merge(man.gw, location.names,
                  by.x = "loc", by.y = "shortname", all.x = TRUE, all.y = FALSE)
  man.gw$fullname <- as.character(man.gw$fullname)

  man.surf <- merge(manual.surf.complete, location.names,
                    by.x = "loc" , by.y = "fullname",
                    all.x = TRUE, all.y = FALSE)
  man.surf$fullname <- man.surf$loc
  man.surf$loc <- as.character(man.surf$shortname)
  man.surf$shortname <- NULL

  manual.complete <- rbind(man.gw, man.surf)

  manual <- split(manual.complete, manual.complete$loc)

  man <- do.call(merge, lapply(manual,
                               function(x) { zoo(x$h, x$t) }))
}

