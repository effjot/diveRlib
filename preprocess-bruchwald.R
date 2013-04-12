#### Datenbereinigung der "rohen" MON-Dateien (nach Export vom
#### Loggerauslese-Laptop, aber vor Import in Diver Office zur
#### Barokompensation

library(zoo)

Sys.setenv(TZ="Etc/GMT-1")


### Verzeichnis

base.dir <- "P:/2008_INKA-BB/Rohdaten/Datenlogger"


### Korrektur Zeitversatz (irrtümlich Sommerzeit)

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

