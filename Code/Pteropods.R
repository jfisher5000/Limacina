#check out the pteropod data from NH05 and 25

#to clear all vars
rm(list=ls())

#to load all your packages
load packages
library(stats)
library(MARSS)
library(forecast)
library(datasets)


#change your working directoy from the File drop-down
setwd("C:/_JF/HMSC/Data/Limacina/R_data")
getwd()

#to import data from a CSV
nh5 <-read.csv("Limacina_NH5_Mo_ts.csv",header=TRUE)
nh25 <-read.csv("Limacina_NH25_Mo_ts.csv",header=TRUE)

#import only the data with good aragonite- 2006 and after- data are 3mo smoothed
nh5.3mo <-read.csv("Arag_Limacina_3mo_NH5.csv",header=TRUE)
nh25.3mo <-read.csv("Arag_Limacina_3mo_NH25.csv",header=TRUE)

#import only the data with good aragonite- 2006 and after- monthly data with %corrosive
nh5 <-read.csv("NH05_Lim_Arag_ForDecomp.csv",header=TRUE)
nh25 <-read.csv("NH25_Lim_Arag_ForDecomp.csv",header=TRUE)

#interpolate over the NaN
library(stinepack)
nh5_interp <- na.stinterp(nh5$Mean.Log10Density)
nh25_interp <- na.stinterp(nh25$Mean.Log10Density)

nh5_interp <- na.stinterp(nh5$X.CorrosiveOf60mWatCol)
nh25_interp <- na.stinterp(nh25$X.CorrosiveOf100mWatCol)

nh5.3mo_AragSat <- na.stinterp(nh5.3mo$AragSatHorDepth)
nh5.3mo_AragSat.40m <- na.stinterp(nh5.3mo$Arag40m)
nh25.3mo_AragSat <- na.stinterp(nh25.3mo$AragSatHorDepth)
nh25.3mo_AragSat.40m <- na.stinterp(nh25.3mo$Arag40m)
nh5.3mo_LogLim <- na.stinterp(nh5.3mo$Log10LimDensity)
nh25.3mo_LogLim <- na.stinterp(nh25.3mo$Log10LimDensity)

#get the min and max dates so that the code can be updated w/ new data
nh5.start.year <- min(nh5$YYYY)
nh5.end.year <- max(nh5$YYYY)
nh5.start.mo <- min(nh5$MM)
nh5.end.mo <- max(nh5$MM)

nh25.start.year <- min(nh25$YYYY)
nh25.end.year <- max(nh25$YYYY)
nh25.start.mo <- min(nh25$MM)
nh25.end.mo <- max(nh25$MM)

#create a time series variable
nh5.ts <- ts(nh5_interp[], start = c(nh5.start.year,nh5.start.mo), end = c(nh5.end.year,nh5.end.mo), frequency = 12)
nh25.ts <- ts(nh25_interp[], start = c(nh25.start.year,nh25.start.mo), end = c(nh25.end.year,nh25.end.mo), frequency = 12)

nh5.3mo.AragSat.ts <- ts(nh5.3mo_AragSat[], start = c(2006,1), end = c(2018,7), frequency = 12)
nh5.3mo.AragSat.40m.ts <- ts(nh5.3mo_AragSat.40m[], start = c(2006,1), end = c(2018,7), frequency = 12)
nh25.3mo.AragSat.ts <- ts(nh25.3mo_AragSat[], start = c(2006,1), end = c(2016,6), frequency = 12)
nh25.3mo.AragSat.40m.ts <- ts(nh25.3mo_AragSat.40m[], start = c(2006,1), end = c(2016,6), frequency = 12)
nh5.3mo.LogLim.ts <- ts(nh5.3mo_LogLim[], start = c(2006,1), end = c(2018,7), frequency = 12)
nh25.3mo.LogLim.ts <- ts(nh25.3mo_LogLim[], start = c(2006,1), end = c(2016,6), frequency = 12)

nh5.Arag.ts <- ts(nh5_interp[], start = c(2006,1), end = c(2018,7), frequency = 12)
nh25.Arag.ts <- ts(nh25_interp[], start = c(2006,1), end = c(2016,6), frequency = 12)


#plot the data
plot(nh5.ts)
plot(nh25.ts)

plot(nh5.Arag.ts)
plot(nh25.Arag.ts)

#check for autocorrelation
acf(nh5.ts)  #doesn't appear autocorrelated
acf(nh25.ts)  #appears autocorellated at 6mo (neg) and 12 mo (pos)

cumsum.nh5 <- cumsum(nh5.ts-mean(nh5.ts))
cumsum.nh25 <- cumsum(nh25.ts-mean(nh25.ts))

cumsum.nh25.Arag <- cumsum(nh25.3mo.AragSat.ts-mean(nh25.3mo.AragSat.ts))
cumsum.nh25.Lim <- cumsum(nh25.3mo.LogLim.ts-mean(nh25.3mo.LogLim.ts))

plot(cumsum.nh5)
plot(cumsum.nh25)

plot(cumsum.nh25.Arag)
plot(cumsum.nh25.Lim)


#decompose your time series into seasonal and long term trends with moving averages
#this stl method is cumbersome cuz it buries the results as weird factors
#decompose seems easier to use but I'd like to still compare the results of the two
stl.nh5 <- stl(nh5.ts, s.window=12)
stl.nh25 <- stl(nh25.ts, s.window=12)

decomp.nh5 <- decompose(nh5.ts)
decomp.mult.nh5 <- decompose(nh5.ts, type="multiplicative")
plot(decomp.nh5)
plot(decomp.mult.nh5)

decomp.nh25 <- decompose(nh25.ts)
plot(decomp.nh25)

######decomp nh25 percent water col undersaturated
decomp.SatHor.nh25 <- decompose(nh25.Arag.ts) #percent of 100m water col undersaturated
plot(decomp.SatHor.nh25)

#decomp nh5
decomp.SatHor.nh5 <- decompose(nh5.Arag.ts) #percent of 100m water col undersaturated
plot(decomp.SatHor.nh5)

#get the data so you can correlate it with env variables
write.csv(decomp.SatHor.nh25$x, file="nh25_Arag_Perc_Corr_decomp_x.csv")
write.csv(decomp.SatHor.nh25$seasonal, file="nh25_Arag_Perc_Corr_decomp_season.csv")
write.csv(decomp.SatHor.nh25$trend, file="nh25_Arag_Perc_Corr_decomp_trend.csv")

write.csv(decomp.SatHor.nh5$x, file="nh5_Arag_Perc_Corr_decomp_x.csv")
write.csv(decomp.SatHor.nh5$seasonal, file="nh5_Arag_Perc_Corr_decomp_season.csv")
write.csv(decomp.SatHor.nh5$trend, file="nh5_Arag_Perc_Corr_decomp_trend2.csv")


##########

decomp.arag40m.nh25 <- decompose(nh25.3mo.AragSat.40m.ts) #this is arag at 40m
decomp.lim.nh25 <- decompose(nh25.3mo.LogLim.ts)

plot(decomp.arag.nh25)
plot(decomp.arag40m.nh25)
plot(decomp.lim.nh25)

#decomp nh5
decomp.arag.nh5 <- decompose(nh5.3mo.AragSat.ts) #this is the sat horizon depth
decomp.arag40m.nh5 <- decompose(nh5.3mo.AragSat.40m.ts) #this is arag at 40m
decomp.lim.nh5 <- decompose(nh5.3mo.LogLim.ts)

plot(decomp.arag.nh5)
plot(decomp.arag40m.nh5)
plot(decomp.lim.nh5)



#plot xy
plot(decomp.nh5$trend,decomp.mult.nh5$trend)
plot(decomp.nh5$seasonal,decomp.mult.nh5$seasonal)
plot(decomp.nh5$random,decomp.mult.nh5$random)

#plot the aragonite trend and the limacina trend
plot(decomp.arag.nh25$trend, lty=2, col="red")
plot(decomp.lim.nh25$trend, lty=1, col="green", lwd=2)

plot(decomp.arag.nh5$trend, lty=2, col="red")
plot(decomp.lim.nh5$trend, lty=1, col="green", lwd=2)

#plot the seasonal arag and limacina
plot(decomp.arag.nh25$seasonal, lty=2, col="red") ##this is confusing since this is the arag horizon depth
plot(decomp.arag40m.nh25$seasonal, lty=2, col="red")
plot(decomp.lim.nh25$seasonal, lty=1, col="green", lwd=2)

plot(decomp.arag40m.nh5$seasonal, lty=2, col="red")
plot(decomp.lim.nh5$seasonal, lty=1, col="green", lwd=2)

#run a correlation betwen 2 time series
cor.test((decomp.arag40m.nh25$seasonal),(decomp.lim.nh25$seasonal),method="pearson") #corr coeff = 0.48
cor.test((decomp.arag40m.nh5$seasonal),(decomp.lim.nh5$seasonal),method="pearson") #corr coeff = -0.36

cor.test((decomp.arag40m.nh25$trend),(decomp.lim.nh25$trend),method="pearson") #corr coeff ns
cor.test((decomp.arag40m.nh5$trend),(decomp.lim.nh5$trend),method="pearson") 

#is it lagged? run a cross-correlation
ccf(decomp.lim.nh25$seasonal , decomp.arag40m.nh25$seasonal, ylab = "Cross-correlation") #limacina lags aragonite
ccf(decomp.lim.nh5$seasonal , decomp.arag40m.nh5$seasonal, ylab = "Cross-correlation") #limacina lags aragonite

#plot in 3 panels
par(mfrow=c(3,1))
plot(decomp.arag40m.nh25$seasonal, lty=1, col="red", lwd=2, ylab = "40m aragonite seasonal")
plot(decomp.lim.nh25$seasonal, lty=1, col="green", lwd=2, ylab = "Limacina density seasonal")
ccf(decomp.lim.nh25$seasonal , decomp.arag40m.nh25$seasonal, ylab = "Cross-correlation")


plot(decomp.arag.nh25$seasonal)
plot(nh25.3mo$Month,nh25.3mo$AragSatHorDepth)

#plot the 2 methods and see what's different
plot(decomp.nh5$random, lty=2, col="red")
lines(decomp.mult.nh5$random, lty=1, col="green", lwd=2)
lines((stl.nh5$time.series==["remainder"]))


plot(stl.nh5)
plot(stl.nh25)

#get the data so you can correlate it with env variables
write.csv(stl.nh5$time.series, file="nh5_decompose.csv")
write.csv(stl.nh25$time.series, file="nh25_decompose.csv")

#use the Mann-Kendall test to see if the long term ternd is increasing or decreasing??
#first test for autocorelation since i think this needs to be done on uncorrelated data
#plot(decompose.nh5$trend)
plot(stl.nh5$trend)
#the decompose results in NaN in the data so only run the autocorrelation on non-Nan data
acf(na.omit(decomp.nh5$trend))#these data are highly autocorrelated so maybe shouldn't run on the long term trend?
acf(na.omit(decomp.nh25$trend))

library(Kendall)
MannKendall(decomp.nh5$trend)
MannKendall(decomp.nh25$trend)


####################################### moving on to GLM ###########################################

library(ggplot2)
library(TMB)
library(lme4)
library(modEvA)

#to clear all vars
rm(list=ls())

#to import data from a CSV
limacina <-read.csv("LimacinaDensity_NH5_25_1996-2018_w_Arag.csv",header=TRUE)

nh5 <-read.csv("LimacinaDensity_NH5_2006-2018_wAragSatHorizon_20mTS.csv",header=TRUE)
nh25 <-read.csv("LimacinaDensity_NH25_1996-2018_wAragSatHorizon_20mTS.csv",header=TRUE)

#to plot 2 rows, 1 column of plots
par(mfrow=c(2,1))

#change char dates to dates that are plottable
limacina$Sample.Date <- as.Date(limacina$Sample.Date, "%Y/%m/%d")

plot(limacina$Sample.Date[limacina$Station=="NH05"],limacina$Limacina.density[limacina$Station=="NH05"],pch=20,lty=1)
plot(limacina$Sample.Date[limacina$Station=="NH25"],limacina$Limacina.density[limacina$Station=="NH25"],pch=20,lty=1)


#glm on year
windows()
glm_1 <- glm(limacina$Limacina.density ~ as.factor(limacina$YYYY), data=limacina)
summary(glm_1)   
plot(glm_1) 
print(glm_1$coefficients)

#glm on year with Log (x+1)
glm_1 <- glm(log(limacina$Limacina.density)+1 ~ as.factor(limacina$YYYY), data=limacina)

glm_1_nh5 <- glm(log((nh5$Limacina.density)+1) ~ as.factor(nh5$YYYY), data=nh5)
glm_1_nh25 <- glm(log((nh25$Limacina.density)+1) ~ as.factor(nh25$YYYY), data=nh25)
anova(glm_1_nh5, test="F")
anova(glm_1_nh25, test="F")
summary(glm_1_nh5) 
Dsquared(glm_1_nh5)

glm_1_nh5 <- glm(((nh5$Limacina.density)) ~ 1+ as.factor(nh5$YYYY) + as.factor(nh5$MM) + (nh5$X.CorrosiveOf60mWatCol) + (nh5$NPGO) ,data=nh5)
glm_bc <- glm(((nh5$Limacina.density)) ~ 1+ (nh5$X.CorrosiveOf60mWatCol),data=nh5)

glm_1_nh5 <- glm(log((nh5$Limacina.density)+0.01) ~ as.factor(nh5$YYYY) + (nh5$X.CorrosiveOf60mWatCol) + (nh5$NPGO),data=nh5)


####################    glm with final sig variables   ############################
glm_1_nh5 <- glm(log((nh5$Limacina.density)+0.01) ~  as.factor(nh5$MM) + as.factor(nh5$YYYY) + nh5$X.CorrosiveOf60mWatCol + nh5$NPGO,data=nh5)
glm_1_nh25 <- glm(log((nh25$Limacina.density)+0.01) ~ as.factor(nh25$YYYY) + as.factor(nh25$MM) + nh25$X.CorrosiveOf100mWatCol + nh25$NPGO, data=nh25)

anova(glm_1_nh5, test="F")
anova(glm_1_nh25, test="F")
summary(glm_1_nh5) 
summary(glm_1_nh25) 
Dsquared(glm_1_nh5)
plot(glm_1_nh5) 

##plot the effects not scaled without the intercept- NH5
maxy <- 200
agx5 <- aggregate(nh5$Limacina.density,by=list(corro = nh5$X.CorrosiveOf60mWatCol), mean)
plot(agx5,
     xlim=c(0,100),
     ylim=c(0,maxy),
     col="darkorchid1", xlab="percent of the water column corrosive", ylab="Limacina density no./m3")#,axes=FALSE)
par(new=TRUE)
plot((1-exp(-0.04063*(0:100)))*100, las=1,xlab="percent of the water column corrosive", 
     ylab="percent decrease in Limacina density",
     xlim=c(0,100),
     type="l", col="darkorchid1", lwd= 2.5,axes=FALSE)
#legend(70,60,legend=c("Shelf NH-5"), lty=1, lwd=2.5, col=c("darkorchid1"))

#If you use, exp(-0.04063*(0:100))*100, this is percent of the density remaining with each incremental increase in corrosiveness. 
#If you use, (1 - exp(-0.04063*(0:100)))*100, this is percent decrease in  the density of limacina with each incremental increase in corrosiveness. 

par(new=TRUE)
agx25 <- aggregate(nh25$Limacina.density,by=list(corro = nh25$X.CorrosiveOf100mWatCol), mean)
plot(agx25, 
     xlim=c(0,100),
     ylim=c(0,maxy),
     col="darkturquoise", xlab="percent of the water column corrosive", ylab="Limacina density no./m3")#axes=FALSE)

##plot the effects not scaled without the intercept- NH25
par(new=TRUE)
plot((1-exp(-0.05287*(0:100)))*100, las=1,xlab="percent of the water column corrosive", 
     xlim=c(0,100),
     ylab="percent decrease in Limacina density",
     type="l", col="darkturquoise", lwd=2.5) #,axes=FALSE)
#legend(70,60,legend=c("Shelf NH-25"), lty=1, lwd=2.5, col=c("darkturquoise"))

legend(70,60,legend=c("Shelf NH-5", "Slope NH-25"), lty=1, lwd=2.5, col=c("darkorchid1","darkturquoise"))

####### end make plot for talk


nh5_ <- exp(-0.04063*(1:100))
nh25_ <- exp(-0.05287*(1:100))
 
#This is the relative difference in the two percent change effects
lines(abs(nh25_-nh5_)/nh5_*100)  

##plot the effects not scaled adding the intercept- NH5
plot(exp(-1.18635 + (-0.04063*(1:100))), las=1,xlab="percent of the water column corrosive", ylab="NH-5 change in Limcina density per m^3",
     type="l")
##plot the effects not scaled adding the intercept- NH25
plot(exp(-0.92115 + (-0.05287*(1:100))), las=1,xlab="percent of the water column corrosive", ylab="NH-25 change in Limcina density per m^3",
     type="l")

###now we'll make an effect plot that is scaled (z-scores) to the center of the data##
glm_1_nh5_scaled <- glm(log((nh5$Limacina.density)+1) ~  as.factor(nh5$MM) + as.factor(nh5$YYYY) + scale(nh5$X.CorrosiveOf60mWatCol) + nh5$NPGO,data=nh5)
summary(glm_1_nh5_scaled)
range(scale(nh5$X.CorrosiveOf60mWatCol))

##plot the scaled effects without the intercept- the scaled center was 36.76568
plot(1:100,100*exp(-0.57627*(0:100-36.76)/33.01)-100, las=1,xlab="percent of the water column corrosive", ylab="percent change in Limacina density",
     type="l")

print(glm_1_nh5$coefficients)

####trying to deal with zeros ###
#trying inverse gaussian dist that only works on non=zero data so I added 1.001 to the log density data- does not work- -AIC nad nothing significant
glm_1_nh5 <- glm(log((nh5$Limacina.density)+1.001) ~  as.factor(nh5$MM) + as.factor(nh5$YYYY) + nh5$X.CorrosiveOf60mWatCol + nh5$NPGO, data=nh5, family=inverse.gaussian)

#glm on month
glm_2 <- glm(limacina$Limacina.density ~ as.factor(limacina$MM), data=limacina)
summary(glm_2)   
plot(glm_2)
print(glm_2$coefficients)
anova(glm_2)

#glm on month
glm_2.5 <- glm(log((limacina$Limacina.density)+1) ~ (limacina$Station) + as.factor(limacina$MM), data=limacina)
summary(glm_2.5)   
plot(glm_2.5)
print(glm_2.5$coefficients)
anova(glm_2.5,test="F")

#glm on month, year, station
glm_3 <- glm(limacina$Limacina.density ~ as.factor(limacina$MM) + as.factor(limacina$YYYY) + (limacina$Station),data=limacina)
summary(glm_3)   
plot(glm_3)
print(glm_3$coefficients)
anova(glm_3)

#glm on month, year, station, aragonite
glm_4 <- glm(log((limacina$Limacina.density)+1) ~ as.factor(limacina$MM) + as.factor(limacina$YYYY) + (limacina$Station) + (limacina$AragoniteSat.40m.), data=limacina)
summary(glm_4)   
plot(glm_4)
print(glm_4$coefficients)
anova(glm_4,test="F")

#glm with 2 random effects- Notice the colon between yr and month
#glmer(eggs ~ 1|yr:month, data=yourData, family=Poisson(link="log"))
glmer1 <- glmer(limacina$Limacina.density ~ 1|limacina$YYYY : limacina$MM, data=limacina)
nh5$MM <- as.factor(nh5$MM)

glmer1 <- glmer(log(Limacina.density+0.00001) ~ (1|MM) + (X.CorrosiveOf60mWatCol), data=nh5)

summary(glmer1)   

glmer2 <- glmer(log(Limacina.density+0.001) ~ (1|YYYY) : (1|MM) + (X.CorrosiveOf60mWatCol) + (NPGO), data=nh5)
summary(glmer2)   
print(glmer2$coefficients)
anova(glmer2,test="F")






