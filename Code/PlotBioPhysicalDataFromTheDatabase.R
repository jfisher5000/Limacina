#######################
#This script downloads a set of biological and physical data from the NWFSC database 
#Computes 3 month running means 
#and plots them as colored time series 
#######################


rm(list=ls())

library(RODBC)
library(doBy)
library(pastecs)


# db<-file.path("Z:/zooplank/Database/ZoopLab_FE_JenniferFisher.accdb") #connect database.
# channel<-odbcConnectAccess2007(db, uid="",pwd="jfisher")
# 
# 
# # This downloads the buoy data (good to get this to make sure we have the right table format)
# channel <- odbcConnect("NWFSC_OCEAN", uid="", pwd="");
# existingData<-sqlQuery(channel, "SELECT TOP 1000 [YYYY]
#                        ,[MM]
#                        ,[DD]
#                        ,[hh]
#                        ,[WD (degT)]
#                        ,[WSPD (m/s)]
#                        ,[GST (m/s)]
#                        ,[WVHT (m)]
#                        ,[DPD (sec)]
#                        ,[APD (sec)]
#                        ,[MWD (degT)]
#                        ,[BAR (hPa)]
#                        ,[ATMP (degC)]
#                        ,[WTMP (degC)]
#                        FROM [NWFSC_OCEAN].[ncc].[46050_Buoy_Data]")
# odbcCloseAll()

#change your working directoy from the File drop-down
setwd("C:/_JF/HMSC/Data/Limacina/R_data")
getwd()

data <-read.csv("qryNoSoBiomass_Richness_w_BasinScaleIndices.csv",header=TRUE)

# get the start and end dates of the data
start.year = min(data$YYYY)
start.mo = min(data$MM)

end.year = max(data$YYYY)
end.mo = max(data$MM)

#####
#loop through all the variablese and calculate 3mo running means
#####

###use this moving average which is a window of averages centered on a middle point with an order = to half your periodicity
library(pastecs)

size <- dim(data)
no.col <- ncol(data)
no.row <- nrow(data)


data.3mo <- matrix(NA, no.row, no.col) #create matrix to store output in
colnames(data.3mo) <- colnames(data)  #name the columns as the data

#turn this into a loop once I get 1 column to work.....
for(i in 1:no.col){
  
  #interpolate over NaNs
  # library(stinepack)
  # data.ONI.nonan = na.stinterp(data$ONI)
  # 
  # #turn your data into a timeseries 
  # ONI.ts <- ts(data.ONI.nonan[], start = c(start.year,start.mo), end = c(end.year,end.mo), frequency = 12)
  
  
  # data.3mo[i] = decaverage(data [ ,i], order = 6) #this creates a time series variable
  # data.3mo = (data.3mo[i] [ ,1]) #1 is the smoothed data, 2 are the residuals
  # data.3mo = as.numeric(data.3mo)   #change the time series to a numeric
  
  #data.3mo[i] <- decaverage(data[ ,i], order = 6) #this creates a time series variable
  avg <- decaverage(data[ ,i], order = 6)
  avg.series <- (avg$series[ ,1]) #1 is the smoothed data, 2 are the residuals
  avg.series.num <- as.numeric(avg.series)   #change from time series to numeric
  data.3mo[ ,i] <- (avg.series.num)   #stuff it back into the blank matrix
}


####
# Let's try filter to also compute a moving average and compare it to decaverage

#ts objects cannot have nans
data.ONI.nonan = na.stinterp(data$ONI)

#turn your data into a timeseries 
ONI.ts <- ts(data.ONI.nonan[], start = c(start.year,start.mo), end = c(end.year,end.mo), frequency = 12)

#now apply the 3mo running average
ONIfilt <- filter(ONI.ts, c(1,1,1)/3, method="convolution", sides=1)

ONI3mo.ts <- ts(data.3mo[,6], start = c(start.year,start.mo), end = c(end.year,end.mo), frequency = 12)


plot(ONIfilt,ONI3mo.ts)
#they are not the same
cor <- cor(ONIfilt,ONI3mo.ts)
#run a correlation betwen 2 time series
cor.test(ONIfilt,ONI3mo.ts,method="pearson")  #corr coeff is 0.93...why are they different?


#####
# okay now let's make some plots
#####

#####
#   ONI   #
#####



