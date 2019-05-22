#Basic calls for time-series anz

#to clear all vars
rm(list=ls())

#to load all your packages
load packages

#change your working directoy from the File drop-down
setwd("C:/R_Work")
getwd()

#to load a previous session
#load("NoCopes.RData")

#to import data from a CSV
copes <-read.csv("copesmo_w_phys_97-2014.csv",header=TRUE)

#to see the names of the headers in the file
names(copes)

#to see the names and parameters of the headers in the file
str(copes)

#to select all the data in a single column
nocopebiomass<-copes[,11]
Lognobiomass<-copes[,13]



#interpolate over the NaN
library(stinepack)
log_ncb_nonan <- na.stinterp(copes$Log10MeanNoBimass)
log_scb_nonan <- na.stinterp(copes$Log10MeanSoBimass)
spprich_nonan <- na.stinterp(copes$SppRichness)

#turn your data into a timeseries 
ncb.ts <- ts(log_ncb_nonan[], start = c(1997,1), end = c(2013,12), frequency = 12)
scb.ts <- ts(log_scb_nonan[], start = c(1997,1), end = c(2013,12), frequency = 12)
spprich.ts <- ts(spprich_nonan[], start = c(1996,5), end = c(2014,5), frequency = 12)

#decompose your time series into seasonal and long term trends with moving averages
decompose_ncb <- decompose(ncb.ts, type = c("additive"), filter = NULL)
decompose_scb <- decompose(scb.ts, type = c("additive"), filter = NULL)
plot (decompose_ncb)
plot (decompose_scb)

#use the Mann-Kendall test to see if your time series is increasing with a monotonic change
#I think this can be done on unsmoothed data?
library(Kendall)
MannKendall(ncb.ts)
MannKendall(scb.ts)
#okay now try it on the long term trend from decaverage
MannKendall(ncb_smootheddata)
MannKendall(scb_smootheddata)

#determine the autocorrelation of the unsmoothed data
acf(ncb.ts)
acf(scb.ts)
acf(spprich.ts)

#calculate a 12 month moving average BUT this is just the average of the next 12 datapoints
ncb_12mo <- SMA(ncb.ts,n=12)

###use this moving average which is a window of averages centered on a middle point with an order = to half your periodicity
library(pastecs)
ncb_12mo_window <- decaverage(ncb.ts, order=6)
scb_12mo_window <- decaverage(scb.ts, order=6)
spprich_12mo_window <- decaverage(spprich.ts, order=6)
plot(ncb_12mo_window)
plot(scb_12mo_window)

#to get out the long term mean [,1] and residuals [,2] from a list from decaverage
ncb.residuals <- (ncb_12mo_window$series[,2])
ncb_smootheddata <- (ncb_12mo_window$series[,1])
scb.residuals <- (scb_12mo_window$series[,2])
scb_smootheddata <- (scb_12mo_window$series[,1])
spprich_smooth <- (spprich_12mo_window$series[,1])

#run a correlation betwen 2 time series
cor.test(ncb_smootheddata,(copes$moPDO),method="pearson")
cor.test(scb_smootheddata,(copes$moPDO),method="pearson")
cor.test(ncb.residuals,(copes$Nino3.4),method="pearson")
cor.test(scb.residuals,(copes$Nino3.4),method="pearson")

#Fourier transform from the 12 mo smoothed data- I NEED OTHER PARAMETERS HERE THAT I DONT UNDERSTAND
fft_ncb_12mo <- fft(ncb_12mo_window)

#to return a martix without NA (the moving avg puts NAs in your data)
ncb_12mo_no_NA <- na.omit(ncb_12mo)

#remove the longterm mean before you run the cum sum
ncb_mean_remove <- ncb.ts-mean(ncb.ts)

#calcualte the cumsum of the biomass
ncb_cumsum <- cumsum(ncb_mean_remove)
spprich_cumsum <- cumsum(spprich_smooth-mean(spprich_smooth))

#this cumsum from Pastecs removes the mean of your data as k local.trend(x, k=mean(x))

#run the Mann-Kendall test to look to see if your timeseries is increasing

#run a correlation betwen 2 time series
cor.test(ncb_cumsum,pdo_cumsum,method="pearson")
cor.test(spprich_cumsum,pdo_cumsum,method="pearson")

#plot 2 series on the same plot
plot(spprich_cumsum, col="red", ylim=c(-60,60))
axis(4,ylim=c(-40,40))
par(new=T) #tell R to not clear the frame before the next command
plot(pdo_cumsum, col="blue", axis=F)
mtext(4,text="PDO")
par(new=F) #you can clear the frame now

#run a cross correlation of the detrended data
dataccf <- ccf(ncb_smootheddata,pdo.ts)
dataccf <- ccf(spprich_cumsum,pdo_cumsum)

#in Pastecs- run the Eigen Vector Filtering function to shift your ts at different lags and then decompose it into the major Eigenvectors (PCA)
ncb.evf <- decevf(ncb.ts, lag=8) #lag is the max lags to be used
plot(ncb.evf)
#get the residuals
ncb.evf.resid1 <- (ncb.evf$series[,2])
acf(ncb.evf.resid1)
ncb.evf.resid2 <- decevf(ncb.evf.resid1, lag=8)
plot(ncb.evf.resid2)
                   
#save your workspace
save.image("NoCopes.RData")

#load your workspace
load("NoCopes.RData")

#create and export a new table with these columns 
richness_PDO = cbind(copes$Year,copes$MM,copes$SppRichness,spprich_cumsum,copes$moPDO,pdo_cumsum)
write.table(richness_PDO,file="spprich_pdo.txt",row.names=FALSE,col.names=TRUE,sep=",")

smooth_copes = cbind(copes$Year,copes$MM,copes$Log10MeanNoBimass,copes$Log10MeanSoBimass,ncb_smootheddata,scb_smootheddata)
write.table(smooth_copes,file="smooth_copes.txt",row.names=FALSE,col.names=TRUE,sep=",")

#save your entire history
savehistory(file="copes.Rhistory")

