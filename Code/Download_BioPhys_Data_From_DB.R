#######################
#This downloads a set of biological and physical data from the NWFSC database 
#######################


rm(list=ls())

library(RODBC)
library(doBy)


db<-file.path("Z:/zooplank/Database/ZoopLab_FE_JenniferFisher.accdb") #connect database.
channel<-odbcConnectAccess2007(db, uid="",pwd="jfisher")


# This downloads the buoy data (good to get this to make sure we have the right table format)
channel <- odbcConnect("NWFSC_OCEAN", uid="", pwd="");
existingData<-sqlQuery(channel, "SELECT TOP 1000 [YYYY]
                       ,[MM]
                       ,[DD]
                       ,[hh]
                       ,[WD (degT)]
                       ,[WSPD (m/s)]
                       ,[GST (m/s)]
                       ,[WVHT (m)]
                       ,[DPD (sec)]
                       ,[APD (sec)]
                       ,[MWD (degT)]
                       ,[BAR (hPa)]
                       ,[ATMP (degC)]
                       ,[WTMP (degC)]
                       FROM [NWFSC_OCEAN].[ncc].[46050_Buoy_Data]")
odbcCloseAll()
