#### Datenbereinigung der "rohen" MON-Dateien (nach Export vom
#### Loggerauslese-Laptop, aber vor Import in Diver Office zur
#### Barokompensation

library(zoo)

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
}


### komplette Baro-Zeitreihe

baro.all.files.1 <-
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
                          "Baro Bruchwald ULN.MON")))
baro.all.files.2 <-
  paste.path(base.dir,
               paste.path("Bruchwald ÜLN, Auslesung 2013-05-03",
                          "Baro Bruchwald ULN^K1941^13-05-03 15-08-51.MON"))
baro.data <- join.data(c(read.mons(baro.all.files.1),
                         read.mons(baro.all.files.2, dec = ",")))
