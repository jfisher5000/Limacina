# Can we bring in data straight from the sql server database
# and insert data back into it?

rm(list=ls())

library(RODBC)
library(doBy)


db<-file.path("Z:/zooplank/Database/ZoopLab_FE_SamZeman.accdb") #connect database.
channel<-odbcConnectAccess2007(db, uid="",pwd="szeman")


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

# Download new data here
#newData <- put code here

# Check similarity of data formats
cbind(sapply(existingData, typeof),sapply(newData, typeof))

# Put the newly dowloaded data back into the SQL Server database
channel <- odbcConnect("NWFSC_OCEAN", uid="", pwd="");
sqlQuery(channel, "insert into [NWFSC_OCEAN].[ncc].[46050_Buoy_Data] select * from newData")
odbcCloseAll()

