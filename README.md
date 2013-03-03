diveRlib
========

A library for processing van Essen / Schlumberger Diver MON files.  Currently reads and writes logging data, but no processing of header (info about logger, location, etc.) yet.

* `read.mon.complete(filename)`

  Reads MON file, returns a list with two elements: `$header` -- unparsed header as lines of text;
  `$data` -- dataframe with columns `t` (time), `h` (water level), `temp` (temperature).

* `read.mon(filname)`

  Reads MON file, returns only the logging data.

* `read.mon.all(dir, basename)`

  Reads als MON files in `dir` which start with `basename` in
  alphabetical order, returns logging date in a single dataframe.
  Useful for logging data distributed across several files or
  monitoring locations with different loggers.

* `write.mon.complete(filename, mon)`

  Takes a list with (unparsed) `$header` and `$data` and writes it to a MON file.




