## Search duplicates

options(warn=2, max.print=150)

foo <- lapply(diver.data,
              function(x) {
                message("Logger ", n[substitute(x)[[3]]]);
                zoo(x$h, x$t)
              })

# stops with non-unique index warning

# test case: diver.files had
#   "2013-05-03", "GW-WAS-212^J5284^13-05-03 14-44-03",

w <- foo$was212
w$t[which(duplicated(w$t))]
