diveRlib
========

A library for processing van Essen / Schlumberger Diver MON files.  Currently reads and writes logging data, but no processing of header (info about logger, location, etc.) yet.

* `read.mon.complete(filename)`

  Reads MON file, returns a list with these elements:
  - `$data` -- dataframe with columns `t` (time), `h` (water level), `temp` (temperature).
  - `$hdr` -- header as a nested list (`$hdr$section$key`)
  - `$comp` -- logical: barometric compensation already done?
  - `$unparsed.header` -- original, unparsed header as lines of text

* `read.mon(filname)`

  Reads MON file, returns only the logging data.

* `read.mon.all(dir, basename)`

  Reads als MON files in `dir` which start with `basename` in
  alphabetical order, returns logging date in a single dataframe.
  Useful for logging data distributed across several files or
  monitoring locations with different loggers.

* `write.mon.complete(filename, mon)`

  Takes MON data as a list (as returned by `mon.read.complete()`),
  writes `$unparsed.header` and `$data` to a MON file.  If you changed
  `$hdr`, use `format.header()` to get a updated `$unparsed.header`.




