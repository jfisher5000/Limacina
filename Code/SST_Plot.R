#plot of standardized Sea Surface Temperature (SST) for Charleston, OR

# Update data:
# SST at Charleston tide station (Sta. 9432780), hourly intervals
#  Data starts 1993.09.24
# Read in existing data, if any:
tmp <- read.csv("SST_Hourly.csv", as.is=T, col.names=c("DateStr","SST"))
#  If this file is missing, run getCharlestonSST1993-2006.bash
# Find the last date in data:
latest <- strptime(tmp$DateStr[length(tmp$DateStr)],
                   "%b %d %Y %I:%M%p", tz="GMT")
# Start download at next day, end at yesterday:
start.date <- format(as.Date(latest)+1, "%Y%m%d")
end.date <- format(as.Date(Sys.time())-1, "%Y%m%d")
url <- paste("'http://opendap.co-ops.nos.noaa.gov/dods/IOOS/Water_Temperature.ascii?WATER_TEMPERATURE_PX.DATE_TIME,WATER_TEMPERATURE_PX.WaterTemp&WATER_TEMPERATURE_PX._STATION_ID=\"9432780\"&WATER_TEMPERATURE_PX._BEGIN_DATE=\"", start.date, "\"&WATER_TEMPERATURE_PX._END_DATE=\"", end.date, "\"'", sep="")

datafile <- "Charleston_SST.tmp"
# check if data file missing or older than 20 days:  ###POSIX ONLY###
lookupfile <- system(paste("find . -name ", datafile, " -mtime -20"), intern=T)
if (length(lookupfile) == 0) {              #No recent file
  system(paste("wget -O ", datafile, url))
  # Strip off header and select only hourly records:
  system(paste('grep -A 100000 ^WATER_TEMPERATURE ', datafile,
               ' | grep ":00" > SST_Hourly.tmp'))
  # Remove the "WATER_TEMP" and blank records, append to csv file:
  system('grep -v WATER_TEMP SST_Hourly.tmp | grep -v ^$ >> SST_Hourly.csv')
  system('rm SST_Hourly.tmp')
  
  #Re-read the updated csv file:
  tmp <- read.csv("SST_Hourly.csv", as.is=T, col.names=c("DateStr","SST"))
}

#Convert DateStr to Year, Month, and Day:
tmp.time <- strptime(tmp$DateStr, "%b %d %Y %I:%M%p", tz="GMT")
tmp <- cbind(tmp.time$year+1900, tmp.time$mon+1, tmp.time$mday, tmp.time$hour, tmp$SST)
colnames(tmp) <- c("Year", "Month", "Day", "Hour", "SST")
                     
#tmp <- read.fwf("SST_Hourly.txt", widths=c(7,5,2,2,3,3,8,8,8),
#                  header=F, fill=T, skip=1,
#                  col.names=c("StaNo","Year","Month","Day","Hour","Min",
#                    "AirT","SST","BarP"))
#Bad data 2002.11.21-2003.09.15--Sensor not working correctly
tmp.date <- as.Date(paste(tmp[,"Year"], tmp[,"Month"], tmp[,"Day"], sep="-"), format="%Y-%m-%d")
tmp[(tmp.date>as.Date("2002-11-20"))&(tmp.date<as.Date("2003-09-15")), "SST"] <- NA
print(summary(tmp))

#Compute monthly means, put in year x month table:
#  For now, ignore filtering to high tide--assume won't affect anomaly.
sst1 <- tapply(tmp[,"SST"], list(tmp[,"Year"], tmp[,"Month"]), mean, na.rm=T)
print(summary(sst1))

#Fill in 1992-1993 from South Slough data
tmp <- read.csv("SouthSloughChBridgeWaterQuality.csv", header=T, fill=T, skip=0)
#print(summary(tmp))
tmp.date <- as.Date(tmp$Date, format="%Y-%m-%d %H:%M:%S")
sst2 <- tapply(tmp$Temp, list(as.POSIXlt(tmp.date)$year+1900,
                              as.POSIXlt(tmp.date)$mon+1), mean, na.rm=T)
print(summary(sst2))

#  regression calibration:
tmp2 <- sst1[c("2002","2003","2004"), ]
mod1 <- lm(as.vector(tmp2) ~ as.vector(sst2))
print(summary(mod1))

# Fill in missing values in Charleston tide station:
tmp2[is.na(tmp2)] <- mod1$coef[1] + sst2[is.na(tmp2)]*mod1$coef[2]
sst1[c("2002","2003","2004"), ] <- tmp2
print(summary(sst1))

#Read in OIMB daily data 1966-1997, convert to monthly means
tmp <- read.csv("OIMB_SST_Daily_1966-1997.csv", header=F, fill=T, skip=16,
                col.names=c("Year","Month","Day","Time","Lat","Long",
                  "Depth","Loc","SST","Sal"))
sst3 <- tapply(tmp$SST, list(tmp$Year, tmp$Month), mean, na.rm=T)
print(summary(sst3))

#Calibrate Charleston tide station to OIMB monthly data 1994-1997
tmp1 <- sst1[c("1994","1995","1996","1997"), ]
tmp2 <- sst3[c("1994","1995","1996","1997"), ]
mod2 <- lm(as.vector(tmp1) ~ as.vector(tmp2))
print(summary(mod2))

#Form consolidated series from OIMB 1966-1993, tide station 1994-present
sst.mat <- rbind(sst3[as.character(1966:1993), ], sst1[-1, ])
colnames(sst.mat) <- c("Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec")
print(summary(sst.mat))

rm(tmp,tmp1,tmp2,mod1,mod2,sst1,sst2,sst3)

#Compute monthly anomalies:
sst.anom <- scale(sst.mat)
print(summary(sst.anom))

#Write out monthly means and anomalies:
Yr <- as.numeric(rownames(sst.mat))
write.table(round(cbind(Yr, sst.mat), 2),
          "Charleston_SST_Monthly.csv", sep=", ", row.names=F)
write.table(round(cbind(Yr, sst.anom), 2),
          "Charleston_SST_Anom.csv", sep=", ", row.names=F)

#create raw data time series:
sst <- as.vector(as.matrix(t(sst.mat)))
sst <- ifelse(is.na(sst),0,sst)
sst.ts <- ts(sst, freq=12, start=c(min(Yr),1))
print(summary(sst.ts))

#create anomaly time series:
anom <- as.vector(as.matrix(t(sst.anom)))
anom <- ifelse(is.na(anom),0,anom)
anom.ts <- ts(anom, freq=12, start=c(min(Yr),1))
print(summary(anom.ts))

postscript("SST_plot.eps", horiz=F, onefile=T, family="Helvetica",
           paper="special", width=6, height=4.5)
par(omi=c(0.25,0.1,0.1,0.1))

.xmin <- 1950
.xmax <- 2020

plot(anom.ts, lwd=1, col="gray50",  xaxs="i",
	main='Sea Surface Temperature (SST) at Charleston, OR',
	ylab="Standardized Anomaly", xlab="Year",
        xlim=c(.xmin,.xmax), ylim=c(-4,4))
abline(h=0)
abline(h=c(-4,-3,-2,-1,1,2,3,4),lty=2, col="gray50")
abline(v=.xmin:.xmax, lty=2, col="gray75")
abline(v=seq(.xmin, .xmax, 10), lty=1, col="gray50")
mtext('Monthly Values', side=3, cex=0.75)

xx <- min(Yr)+(0:(length(sst.ts)-1))/12
xx <- c(xx,rev(xx))

#y.pos <- ifelse(sst.ts>0,sst.ts,0)
y.pos <- ifelse(anom.ts>0,anom.ts,0)
polygon(xx,c(y.pos,rep(0,length(y.pos))), col="red")
#y.neg <- ifelse(sst.ts<0,sst.ts,0)
y.neg <- ifelse(anom.ts<0,anom.ts,0)
polygon(xx,c(y.neg,rep(0,length(y.pos))), col="blue")

mtext(paste("NOAA/NWFSC/FED, Updated ", format(Sys.time(), "%d %b %Y")),
      side=1, adj=1, cex=0.5, outer=T)

dev.off()
