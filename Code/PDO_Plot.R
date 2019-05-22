#plot of PDO

# Update data:
url <- "http://jisao.washington.edu/pdo/PDO.latest.txt"
datafile <- "PDO.index"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
}

pdo <- read.table(datafile, skip=31, header=F, fill=T)
#NOTE:  this is character data because of trailing text lines in file
pdo <- as.numeric(as.matrix(t(pdo[,2:13])))  #convert to numeric vector
pdo <- ifelse(is.na(pdo),0,pdo)
pdo.ts <- ts(pdo, freq=12, start=c(1900,1))
print(summary(pdo.ts))
postscript("PDO_plot.eps", horiz=F, onefile=T, family="Helvetica",
           paper="special", width=6, height=4.5)
par(omi=c(0.25,0.1,0.1,0.1))

.xmin <- 1950
.xmax <- 2020
#Raw data line plot:
plot(pdo.ts, type="n", xaxs="i",
	main='Pacific Decadal Oscillation (PDO)',
	ylab="Standardized Anomaly", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-3,3))

#Background grid lines:
abline(h=c(-3,-2,-1,1,2,3),lty=2, col="gray75")
abline(v=(.xmin+1):(.xmax-1), lty=2, col="gray75")
abline(v=seq(.xmin,.xmax,10), lty=1, col="gray50")
box() #Redraw borders over grid

#Moving Average polygons:
pdo.ma <- filter(pdo.ts, rep(1,12)/12, method="convolution", sides=1)
pdo.ma <- pdo.ma[!is.na(pdo.ma)]
xx <- 1900 + (0:(length(pdo.ma)-1))/12 + 6/12  #shift MA plot
xx <- c(xx,rev(xx))
y.pos <- ifelse(pdo.ma>0, pdo.ma, 0)
polygon(xx,c(y.pos,rep(0,length(y.pos))), col="red")
y.neg <- ifelse(pdo.ma<0, pdo.ma, 0)
polygon(xx,c(y.neg,rep(0,length(y.neg))), col="blue")

#Add lines over polygon edges:
lines(pdo.ts, lwd=1, col="black")
abline(h=0, col="black")

mtext('Monthly (black) & 12-Month Average (color)', side=3, cex=0.75)
mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
