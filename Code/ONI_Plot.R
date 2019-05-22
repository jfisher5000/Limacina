#ONI_Plot.R -- plot of Oceanic Niño Index (ONI)

#  The NOAA definition of a Nino/Nina event is that the 3-month moving
#  average anomaly of the Nino Area 3.4 anomaly exceeds +/- 0.5 C

# Tropical SST data source:
url <- "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/sstoi.indices"
# Local data file:
datafile <- "sstoi.indices"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
# TODO: rewrite this using file.info()$mtime for portability
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
}
#  read SST indices, skip names row because names not unique:
sst <- read.table(datafile, skip=1, header=F)
colnames(sst) <- c("Yr", "Mon", "N1.2", "N1.2Anom", "N3", "N3Anom",
                   "N4", "N4Anom", "N3.4", "N3.4Anom")
sst.yrs <- sst$Yr
# extract Nino3.4 anomalies
n3.4 <- sst[ , "N3.4Anom"] 
#make it a time series object
oni.ts <- ts(n3.4, freq=12, start=c(min(sst.yrs),1))  
# Compute the 3-month moving average:
oni.ts <- filter(oni.ts, c(1,1,1)/3, method="convolution", sides=1)
print(summary(oni.ts))
rm(sst)

  postscript("ONI_plot.eps", horiz=F, onefile=T, family="Helvetica",
             paper="special", width=6, height=4.5)
  par(omi=c(0.25,0.1,0.1,0.1))

# Years to include in plot:
.xmin <- 1950
.xmax <- 2020

#blank frame:
plot(oni.ts, lwd=1, type="n", xaxs="i",
	main=expression(Oceanic ~~ Ni * tilde(n) * o ~~ Index ~~ (ONI)),
        ylab=" Degrees Celsius", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-3,3))
## Note use of expression() to PORTABLY insert tilde via Latin-1 charset
#Grid lines in background:
abline(h=c(-3,-2,-1,1,2,3), lty=2, col="gray75")
abline(v=.xmin:.xmax, lty=2, col="gray75")
abline(v=seq(.xmin, .xmax, 10), lty=1, col="gray50")
box() #Redraw borders over grid

#Polygons:
xx <- min(sst.yrs)+(0:(length(oni.ts)-1))/12
xx <- c(xx,rev(xx))
# el Ninyo is defined as values > 0.5:
ninyo <- ifelse(oni.ts > 0.5, oni.ts, 0.5)
polygon(xx,c(ninyo,rep(0.5,length(ninyo))), col="red", border=NA)
# la Ninya is defined as values < -0.5:
ninya <- ifelse(oni.ts < -0.5, oni.ts, -0.5)
polygon(xx,c(ninya,rep(-0.5,length(ninya))), col="blue", border=NA)
# in between is neutral:
neutral <- ifelse(oni.ts > 0.5, 0.5, oni.ts)
neutral <- ifelse(neutral < -0.5, -0.5, neutral)
polygon(xx, c(neutral, rep(0,length(neutral))), col="grey", border=NA)

#Plot borders over polygons:
lines(oni.ts, lwd=1, col="black")
abline(h=c(-0.5, 0, 0.5), col="black") #Cutlines for Nino zones

#Legend:
zones <- c(expression(El ~~ Ni * tilde(n) * o),
	"Neutral",
	expression(La ~~ Ni * tilde(n) * a))
legend("bottom", zones, fill=c("red", "grey", "blue"), 
	ncol=3, bg="white", bty="n")
#Credit:
mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
