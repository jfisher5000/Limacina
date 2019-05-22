#plot of standardized Upwelling Index (UWI) for 45N

# Update data:
url <- "ftp://orpheus.pfeg.noaa.gov/outgoing/upwell/monthly/upindex.mon"
datafile <- "UWI.index"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
# Extract the "45N" section:
  system(paste("grep '45N 125W' ", datafile, " > uwi.txt"))
}

#Upwelling index (UWI):
uwi <- read.table("uwi.txt", colClasses=c("factor","factor",rep("numeric",13)),
                  header=F, fill=T, col.names=c("Lat","Long","YEAR","JAN",
                                      "FEB","MAR","APR","MAY","JUN","JUL",
                                      "AUG","SEP","OCT","NOV","DEC"))
#Only one lat/long, so delete columns:
uwi <- uwi[, 3:15]
print(summary(uwi))

#separate out year column before processing:
uwi.yrs <- uwi[,1]
uwi <- uwi[,2:13]

#Compute monthly anomalies:
uwi.anom <- scale(uwi)
print(summary(uwi.anom))

#create raw data time series:
uwi <- as.vector(as.matrix(t(uwi)))
uwi <- ifelse(is.na(uwi),0,uwi)
uwi.ts <- ts(uwi, freq=12, start=c(min(uwi.yrs),1))
print(summary(uwi.ts))

#create anomaly time series:
anom <- as.vector(as.matrix(t(uwi.anom)))
anom <- ifelse(is.na(anom),0,anom)
anom.ts <- ts(anom, freq=12, start=c(min(uwi.yrs),1))
print(summary(anom.ts))


postscript("UWI_plot.eps", horiz=F, onefile=T, family="Helvetica",
           paper="special", width=6, height=4.5)
par(omi=c(0.25,0.1,0.1,0.1))

.xmin <- 1950
.xmax <- 2020

#plot(uwi.ts, lwd=1, col="gray50",  xaxs="i",
#	main='Upwelling Index (UWI) at 45N, 125W',
#	ylab=expression(m^3 * s^{-1} * (100 * m)^{-1}), xlab="Year",
#        xlim=c(.xmin,.xmax))
plot(anom.ts, lwd=1, col="gray50",  xaxs="i",
	main='Upwelling Index (UWI) at 45N, 125W',
	ylab="Standardized Anomaly", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-4,4))
abline(h=0)
abline(h=c(-4,-3,-2,-1,1,2,3,4),lty=2, col="gray50")
abline(v=.xmin:.xmax, lty=2, col="gray75")
abline(v=seq(.xmin, .xmax, 10), lty=1, col="gray50")
mtext('Monthly Values', side=3, cex=0.75)

xx <- min(uwi.yrs)+(0:(length(uwi.ts)-1))/12
xx <- c(xx,rev(xx))

#y.pos <- ifelse(uwi.ts>0,uwi.ts,0)
y.pos <- ifelse(anom.ts>0,anom.ts,0)
polygon(xx,c(y.pos,rep(0,length(y.pos))), col="blue")
#y.neg <- ifelse(uwi.ts<0,uwi.ts,0)
y.neg <- ifelse(anom.ts<0,anom.ts,0)
polygon(xx,c(y.neg,rep(0,length(y.pos))), col="red")

mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
