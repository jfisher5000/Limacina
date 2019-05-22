#plot of standardized Sea Level Height (SLH) for Astoria, OR

# Update data:
url <- "ftp://ilikai.soest.hawaii.edu/rqds/pacific/monthly/m572a.dat"
datafile <- "SLAstoria.txt"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
}

#Sea Level Height (SLH):
.tmp <- read.table(datafile, colClasses=c("factor","factor",rep("numeric",14)),
                  header=F, fill=T, skip=1,
                  col.names=c("StaNo","Sta","YEAR","RecNo",
                    "SL1","MD1","SL2","MD2","SL3","MD3",
                    "SL4","MD4","SL5","MD5","SL6","MD6"))
#Rearrange from 2-record 6-mo per record format, drop missing days (MD):
.part1 <- .tmp[.tmp[,"RecNo"]==1, c("YEAR","SL1", "SL2","SL3","SL4","SL5","SL6")]
.part2 <- .tmp[.tmp[,"RecNo"]==2, c("SL1", "SL2","SL3","SL4","SL5","SL6")]
slh <- cbind(.part1,.part2)
colnames(slh) <- c("YEAR","JAN","FEB","MAR","APR","MAY","JUN",
                          "JUL","AUG","SEP","OCT","NOV","DEC")
slh[slh==9999] <- NA  #replace missing value codes
print(summary(slh))
rm(.tmp,.part1,.part2)

#separate out year column before processing:
slh.yrs <- slh[,1]
slh <- slh[,2:13]

#Compute monthly anomalies:
slh.anom <- scale(slh)
print(summary(slh.anom))

#create raw data time series:
slh <- as.vector(as.matrix(t(slh)))
slh <- ifelse(is.na(slh),0,slh)
slh.ts <- ts(slh, freq=12, start=c(min(slh.yrs),1))
print(summary(slh.ts))

#create anomaly time series:
anom <- as.vector(as.matrix(t(slh.anom)))
anom <- ifelse(is.na(anom),0,anom)
anom.ts <- ts(anom, freq=12, start=c(min(slh.yrs),1))
print(summary(anom.ts))


postscript("SLH_plot.eps", horiz=F, onefile=T, family="Helvetica",
           paper="special", width=6, height=4.5)
par(omi=c(0.25,0.1,0.1,0.1))

.xmin <- 1950
.xmax <- 2020

plot(anom.ts, lwd=1, col="gray50",  xaxs="i",
	main='Sea Level Height (SLH) at Astoria',
	ylab="Standardized Anomaly", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-4,4))
abline(h=0)
abline(h=c(-4,-3,-2,-1,1,2,3,4),lty=2, col="gray50")
abline(v=.xmin:.xmax, lty=2, col="gray75")
abline(v=seq(.xmin, .xmax, 10), lty=1, col="gray50")
mtext('Monthly Values', side=3, cex=0.75)

xx <- min(slh.yrs)+(0:(length(slh.ts)-1))/12
xx <- c(xx,rev(xx))

#y.pos <- ifelse(slh.ts>0,slh.ts,0)
y.pos <- ifelse(anom.ts>0,anom.ts,0)
polygon(xx,c(y.pos,rep(0,length(y.pos))), col="red")
#y.neg <- ifelse(slh.ts<0,slh.ts,0)
y.neg <- ifelse(anom.ts<0,anom.ts,0)
polygon(xx,c(y.neg,rep(0,length(y.pos))), col="blue")

mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
