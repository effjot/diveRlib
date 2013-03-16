diveRlib
========

A library for processing van Essen / Schlumberger Diver MON files.  Currently reads and writes logging data, but no processing of header (info about logger, location, etc.) yet.

* `read.mon.complete(filename)`

  Reads MON file, returns a list with these elements:
  - `$loc` -- location / logger name
  - `$comp` -- logical: barometric compensation already done?
  - `$data` -- dataframe with columns `t` (time), `h` (water level), `temp` (temperature).
  - `$hdr` -- header as a nested list (`$hdr$section$key`)
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

* `update.header(mon, filename, t.col)`

  Takes MON data (nested list, as above) and returns an updated header
  structure (*only* the header structure, so you can use the return
  value directly in a call to `format.header()`).  These fields in
  `mon$hdr` are updated:

  - `'Series settings'$'Start date / time'` and `'Series
    settings'$'End date / time'` are set to the actual start and end
    of the time series (`mon$data`), in case you modified the data.
    (Pass name of time colum `t.col` if it isn't the standard `t`.)

  - `FILEINFO$DATE` and `FILEINFO$TIME` are set to the current time.
  - optionally, the `filename` argument is stored in
    `FILEINFO$FILENAME`; slashes in the path (as R uses them) are
    converted to backslashes.

* `format.header(header, created.by.diveRlib)`

  Takes header (parsed into nested list) and returns text
  representation (vector of strings) which can be used for writing a
  MON file.  With `created.by.diveRlib=TRUE`, a diveRlib
  identification string can be written to the `CREATED BY` field
  instead of the Diver Office version information.  However, Diver
  Office won't read such a file.  (Which you might intend to avoid
  re-importing duplicates into your database.)
