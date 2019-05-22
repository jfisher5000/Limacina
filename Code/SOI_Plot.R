#plot of standardized Southern Oscillation Index (SOI)

# Update data:
url <- "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/soi"
datafile <- "SOI.index"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
# Extract the "STANDARDIZED DATA" section:
  system(paste("grep -A 1000 STANDARDIZED ", datafile, " > soi.txt"))
}

#Southern oscillation index (SOI):
soi <- read.fwf("soi.txt", widths=c(4,rep(6,12)), skip=3, header=F)
print(summary(soi))
soi.yrs <- soi[,1]
soi <- as.vector(as.matrix(t(soi[,2:13])))
soi[soi < -99] <- NA    #missing values are -999.9
soi <- ifelse(is.na(soi),0,soi)
soi.ts <- ts(soi, freq=12, start=c(min(soi.yrs),1))
print(summary(soi.ts))

postscript("SOI_plot.eps", horiz=F, onefile=T, family="Helvetica",
           paper="special", width=6, height=4.5)
par(omi=c(0.25,0.1,0.1,0.1))

.xmin <- 1950
.xmax <- 2020

plot(soi.ts, lwd=1, col="gray50",  xaxs="i",
	main='Southern Oscillation Index (SOI)',
	ylab="Standardized Anomaly", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-4,4))
abline(h=0)
abline(h=c(-4,-3,-2,-1,1,2,3,4),lty=2, col="gray50")
abline(v=.xmin:.xmax, lty=2, col="gray75")
abline(v=seq(.xmin, .xmax, 10), lty=1, col="gray50")
mtext('Monthly Values', side=3, cex=0.75)

xx <- min(soi.yrs)+(0:(length(soi.ts)-1))/12
xx <- c(xx,rev(xx))
y.pos <- ifelse(soi.ts>0,soi.ts,0)
polygon(xx,c(y.pos,rep(0,length(y.pos))), col="blue")
y.neg <- ifelse(soi.ts<0,soi.ts,0)
polygon(xx,c(y.neg,rep(0,length(y.pos))), col="red")

mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
