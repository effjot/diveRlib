### flach-tief

sued <- window(h[, c("was216", "was224")], start=ISOdate(2011,07,01), end=ISOdate(2013,11,15))
sued$diff <- sued$was216 - sued$was224

plot(sued[, c("was216", "was224")], screens = 1, col = c("red", "blue"), ylim = c(36.6, 37.6), main = "flache/tiefe GWMS (rot, blau): WAS216, WAS224", xlab = "", ylab = "h (m NHN)")

plot(sued$diff, ylim = c(-0.1, +0.1), main = "Differenz flache/tiefe GWMS: WAS216 - WAS224", xlab = "", ylab = "Delta h (m)", panel.first = c(abline(h = 0.0, col = "grey")))

plot(window(h[, c("was209", "was210")], start=ISOdate(2013,05,01), end=ISOdate(2013,11,15)), screens = 1, col = c("red", "blue"), ylim = c(36.7, 37.9), main = "flache/tiefe GWMS (rot, blau): WAS209, WAS210", xlab = "", ylab = "h (m NHN)")

