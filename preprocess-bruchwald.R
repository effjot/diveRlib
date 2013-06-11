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


Sys.setenv(TZ="Etc/GMT-1")


### Verzeichnis

base.dir <- "P:/2008_INKA-BB/Rohdaten/Datenlogger"


### Korrektur Zeitversatz (irrtümlich Sommerzeit)

do.fix <- FALSE
if (do.fix) {
  fix.file <- function(dir, basename) {
    filenames <- paste.path(dir, paste0(c("", "KORR_"), basename))
    x <- read.mon.complete(filenames[1])
    x$data <- daylightsaving.to.standard.time(x$data)
    x$data <- remove.out.of.water(x$data)
    x$unparsed.header <- format.header(update.header(x, filenames[2]))
    write.mon.complete(filenames[2], x)
  }

  cur.dir <- paste.path(base.dir, "Bruchwald ÜLN, Auslesung 2013-02-23")
  fix.file(cur.dir, "gw-was-206_130225124849_75778.MON")
  fix.file(cur.dir, "lp-brw-5op_130225125129_75779.MON")
  fix.file(cur.dir, "gw-was-212_130225124909_J5284.MON")
  fix.file(cur.dir, "gw-was-213_130225125016_N0144.MON")

  cur.dir <- paste.path(base.dir, "Bruchwald ÜLN, Auslesung 2013-03-25+26")
  fix.file(cur.dir, "GW-WAS-206.MON")
  fix.file(cur.dir, "GW-WAS-212.MON")
  fix.file(cur.dir, "GW-WAS-213.MON")
  fix.file(cur.dir, "LP-BRW-5OP.MON")
}


### komplette Baro-Zeitreihe

baro.all.files <-
  paste.path(base.dir,
             c(paste.path(c("Bruchwald ÜLN, Auslesung 2011-05-05",
                            "Bruchwald ÜLN, Auslesung 2011-05-11",
                            "Bruchwald ÜLN, Auslesung 2011-10-21",
#                            "Bruchwald ÜLN, Auslesung 2012-02-29",
                            "Bruchwald ÜLN, Auslesung 2012-03-30"),
                          "Baro Bruchwald ULN.MON"),
               paste.path("Bruchwald ÜLN, Auslesung 2012-07-12",
                          "baro bruchwald uln_120713112829_K1941.MON"),
               paste.path("Bruchwald ÜLN, Auslesung 2013-02-23",
                          "baro bruchwald uln_130225124744_K1941.MON"),
               paste.path("Bruchwald ÜLN, Auslesung 2013-03-25+26",
                          "Baro Bruchwald ULN.MON"),
               paste.path("Bruchwald ÜLN, Auslesung 2013-05-17",
                          "Baro Bruchwald ULN^K1941^13-05-17 16-09-13.MON")))

## Überlappung Parallelbetrieb während Loggerwechsel
fix.baro.overlap <- function(baro.df) {
  t.change <- which(baro.df$t == ISOdatetime(2011, 5, 5, 11, 45, 0))
  i <- c(1:t.change[1], (t.change[2]+1):nrow(baro.df))
  baro.df[i, ]
}

baro.data <- Compose(fix.baro.overlap, join.data,
                     Curry(read.mons, dec = "auto"))(baro.all.files)

baro.zoo <- zoo(baro.data$h, baro.data$t)




## w220 <- read.mon(paste.path(base.dir,
##                             "Bruchwald ÜLN, Auslesung 2013-05-03",
##                             "GW-WAS-220^J5298^13-05-03 14-02-33.MON"),
##                  dec = ",")
## z220 <- out.of.water.as.NA(zoo(w220$h, w220$t))

## #test data for baro.comp: shifted wrt baro
## x220 <- shift.time(w220, 8*60)
## xz220 <- out.of.water.as.NA(zoo(x220$h, x220$t))



diver.files <- lapply(
  list(      # for each logger, vector of date-basename pairs
    was205 = c("2011-08-02", "GW-WAS-205",
      "2011-11-08", "GW-WAS-205",
      "2011-12-09", "GW-WAS-205",
      "2012-08-27", "GW-WAS-205",
      "2013-03-28+04-05", "GW-WAS-205"),
    was206 = c("2013-03-25+26", "KORR_GW-WAS-206",
      "2013-03-28+04-05", "GW-WAS-206"),
    was209 = c("2011-05-05", "GW-WAS-209 Location korrigiert",
      "2011-08-02", "GW-WAS-209",
      "2011-10-21", "GW-WAS-209",
      "2012-07-12", "gw-was-209_120713112908_H0997",
      "2013-02-23", "gw-was-209_130225124858_H0997",
      "2013-05-17", "GW-WAS-209^H0997^13-05-17 15-11-54")
    ),
  FUN = function(dat.name) {
    paste.path(base.dir,
               paste("Bruchwald ÜLN, Auslesung",
                     odd.numbered.elements(dat.name)),
               paste(even.numbered.elements(dat.name),
                     "MON", sep = "."))
  }
)

diver.data <- lapply(diver.files,
                     Compose(join.data,
                             Curry(read.mons, dec = "auto")))

diver.zoo <- lapply(diver.data,
                    Compose(out.of.water.as.NA,
                            function(x) zoo(x$h, x$t)))
