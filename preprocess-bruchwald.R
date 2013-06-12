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
    x <- read.mon.complete(filenames[1], dec = "auto")
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

  cur.dir <- paste.path(base.dir, "Bruchwald ÜLN, Auslesung 2013-05-03")
  fix.file(cur.dir, "GW-WAS-220^J5298^13-05-03 14-02-33.MON")
}


### Read all data into data.frames and (list of) zoo objects

## Helper function: build complete filenames from fixed and variable parts;
## takes vector of date (i.e. subdir suffix) -- filename pairs

build.filenames <- function(dat.name, dir = base.dir,
                            subdir.basename = "Bruchwald ÜLN, Auslesung",
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
  "2013-05-17", "Baro Bruchwald ULN^K1941^13-05-17 16-09-13"))

## Überlappung Parallelbetrieb während Loggerwechsel
fix.baro.overlap <- function(baro.df) {
  t.change <- which(baro.df$t == ISOdatetime(2011, 5, 5, 11, 45, 0))
  i <- c(1:t.change[1], (t.change[2]+1):nrow(baro.df))
  baro.df[i, ]
}

baro.data <- Compose(fix.baro.overlap, join.data,
                     Curry(read.mons, dec = "auto"))(baro.all.files)

baro.zoo <- zoo(baro.data$h, baro.data$t)


## Diver timeseries

diver.files <- lapply(
  list(was205 = c("2011-08-02", "GW-WAS-205",
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
         "2013-05-17", "GW-WAS-209^H0997^13-05-17 15-11-54"),
       was210 = c("2013-05-17", "GW-WAS-210^J4511^13-05-17 15-05-50"),
       was212 = c("2013-03-25+26", "KORR_GW-WAS-212",
         "2013-05-03", "GW-WAS-212^J5284^13-05-03 14-44-03"),
       was213 = c("2013-03-25+26", "KORR_GW-WAS-213",
         "2013-05-03", "GW-WAS-213^N0144^13-05-03 14-52-54"),
       was220 = c("2013-05-03", "GW-WAS-220^J5298^13-05-03 14-02-33"),
       was216 = c("2011-07-13", "GW-WAS-216",
         "2011-10-21", "GW-WAS-216",
         "2012-03-30", "GW-WAS-216",
         "2012-07-12", "gw-was-216_120713112941_F0432",
         "2013-02-23", "gw-was-216_130225125025_F0432",
         "2013-03-25+26", "GW-WAS-216",
         "2013-05-17", "GW-WAS-216^F0432^13-05-17 16-10-27"),
       was224 = c("2011-10-21", "GW-WAS-224",
         "2012-03-30", "GW-WAS-224",
         "2012-07-12", "gw-was-224_120713113201_G0181",
         "2013-02-23", "gw-was-224_130225125118_G0181",
         "2013-03-25+26", "GW-WAS-224",
         "2013-05-17", "GW-WAS-224^G0181^13-05-17 16-13-21"),
       lp5 = c("2013-03-25+26", "KORR_LP-BRW-5OP",
         "2013-05-03", "LP-BRW-5OP^75779^13-05-03 14-34-29")
  ),
  FUN = build.filenames)


diver.data <- lapply(diver.files,
                     Compose(join.data,
                             Curry(read.mons, dec = "auto")))

diver.zoo <- lapply(diver.data,
                    Compose(out.of.water.as.NA,
                            function(x) zoo(x$h, x$t)))


### Baro-compensation (only zoo)

wat.col <- lapply(diver.zoo, Curry(baro.comp, baro = baro.zoo))

wc <- do.call(merge, wat.col)


wat.depth <- list()
wat.depth$was205 <- -(200.5 - 0.51 - wat.col$was205)
wat.depth$was209 <- -(220.0 - 0.39 - wat.col$was209)
wat.depth$was210 <- -(458.2 - 0.81 - wat.col$was210)
wat.depth$was212 <- -(262.4 - 0.55 - wat.col$was212)
wat.depth$was213 <- -(238.2 - 0.75 - wat.col$was213)
wat.depth$was216 <- -(303 - 0.66 - wat.col$was216)#wurde 2011 verlängert
wat.depth$was224 <- -(291 - 0.80 - wat.col$was224)

wat.level <- list()
wat.level$was205 <- 38.427 + wat.depth$was205/100 #nicht konst.
wat.level$was209 <- 38.574 + wat.depth$was209/100
wat.level$was210 <- 39.147 + wat.depth$was210/100
wat.level$was212 <- 38.206 + wat.depth$was212/100
wat.level$was213 <- 38.429 + wat.depth$was213/100
wat.level$was216 <- 38.924 + wat.depth$was216/100
wat.level$was224 <- 39.030 + wat.depth$was224/100

dw <- do.call(merge, wat.depth)
h <- do.call(merge, wat.level)


# Test-Plots

plot(h[, c("was216", "was224")], screens=1, col=c("green", "blue"), ylim=c(36.8,37.6))

plot(window(wc[, c("was212", "was213")], start=ISOdate(2012,10,20), end=ISOdate(2013,05,1)), screens=1, col=c("green", "blue"))
