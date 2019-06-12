#######################
#This script downloads a set of biological and physical data from the NWFSC database 
#Computes 3 month running means 
#and plots them as colored time series 
#######################


rm(list=ls())

#library(RODBC)      #database
#library(doBy)       #database
library(pastecs)
library(lubridate)  # for working with dates
library(ggplot2)    # for creating graphs
library(ggplotify)
library(scales)     # to access breaks/formatting functions
library(gridExtra)  # for arranging plots
library(grid)       # for arranging plots
#library(plyer)
library(dplyr)
library(stinepack)  #time-series anz
library(lubridate)  #for working with dates
library(ggfortify)
library(zoo)        #working with time series


# db <- file.path("Y:/zooplank/Database/ZoopLab_FE_JenniferFisher.accdb") #connect database.
# channel <- odbcConnectAccess2007(db, uid="",pwd="jfisher")
# # 
# # 
# # This downloads the buoy data (good to get this to make sure we have the right table format)
# channel <- odbcConnect("NWFSC_OCEAN", uid="", pwd="");
# existingData <- sqlQuery(channel, "SELECT TOP 1000 [YYYY]
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

data <- read.csv("qryNoSoBiomass_Richness_w_BasinScaleIndices.csv",header=TRUE)

# get the start and end dates of the data
start.yr = min(data$YYYY)
start.mo = min(data$MM[data$YYYY==start.yr])

end.yr = max(data$YYYY)
end.mo = max(data$MM[data$YYYY==end.yr])

size <- dim(data)
no.col <- ncol(data)
no.row <- nrow(data)


#####
#loop through all the variablese and calculate 3mo running means
#####

data.3mo <- matrix(NA, no.row, no.col) #create matrix to store output in
colnames(data.3mo) <- paste(colnames(data),"3mo", sep="")  #name the columns as the data

#only do for the bio data so start at 7
for(i in 7:no.col){
  
  #turn your data into a timeseries 
  data.ts <- ts(data[ ,i], 
                start = c(start.yr,start.mo), 
                end = c(end.yr,end.mo), 
                frequency = 12)
  
  #interpolate over NaNs
  data.nonan <- na.approx(data[,i],
                          na.rm=FALSE)
  
  #make sure you are using the correct filter function from the stats package
  filter <- stats::filter
  
  #compute the 3mo running mean using filter
  var.3mo <- filter(data.nonan, c(1,1,1)/3, method="convolution", circular = FALSE, sides=2)
  
  #stuff it back into the blank matrix as numeric (not ts object)
  data.3mo[ ,i] <- as.numeric(var.3mo)   
}

#turn it into a data frame which is easier to deal with?
data.3mo <- as.data.frame(data.3mo)

#combine the running means with the original data
data <- bind_cols(data[1:no.col],data.3mo[7:no.col])


# ####
# # Let's compare filter and decaverage to compute 3mo running means
# ####
# 
# #ts objects cannot have nans
# data.ONI.nonan = na.stinterp(data$ONI)
# 
# #turn your data into a timeseries 
# ONI.ts <- ts(data.ONI.nonan[], start = c(start.yr,start.mo), end = c(end.yr,end.mo), frequency = 12)
# 
# #now apply the 3mo running average using 2 methods
# ONIfilt <- filter(ONI.ts, c(1,1,1)/3, method="convolution", sides=2)
# ONIdecavg <- decaverage(ONI.ts,order = 1)
# ONIdecavg.series <- (ONIdecavg$series[ ,1]) #1 are the smoothed data, 2 are the residuals
# 
# plot(ONIfilt,ONIdecavg.series)
# #they are the same
# 
# #run a correlation betwen 2 time series just to test
# cor.test(ONIfilt,ONIdecavg.series,method="pearson")  #corr coeff is 1



#####
# okay now let's make some plots with ggplot
#####

#if you want to plot everything use matplot (matrix plot)
#matplot(data.3mo, type="l")


#deal with the dates
data <- data %>% mutate(date = as.Date(data$SigmaPlotDate, format = '%m/%d/%Y'))
#data.3mo <- data.3mo %>% mutate(date = as.Date(data.3mo$SigmaPlotDate3mo, format = '%m/%d/%Y'))

min.date <- min(data$date) - 30   #pad the start date by 30 days
max.date <- max(data$date) + 30   #pad the end date by 30 days


#add a column with your condition to color the anomalies
data <- data %>% mutate(NoCop.color = ifelse(data$NorthernBiomassAnomaly3mo>0, "pos", "neg"))
data <- data %>% mutate(SoCop.color = ifelse(data$SouthernBiomassAnomaly3mo>0, "pos", "neg"))
data <- data %>% mutate(PDO.color = ifelse(data$PDO>0, "pos", "neg"))

#trying to get the anomalies colored
anom.color <- c("indianred3", "royalblue3")


#### Northerns
pl.no.copes <- ggplot(data = data, aes(x = date, y = NorthernBiomassAnomaly3mo, fill = NoCop.color)) + #fill color based on pos or neg anoms
  geom_bar(stat = "identity", color = "black", size=0.05, na.rm = TRUE) +  #make the bar outline black
  scale_fill_manual(values = anom.color) +  #color the anoms w/ what you want
  
  theme_light() +
  theme(
  legend.position = "none",
  #panel.border = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank()
  ) +
    
  scale_x_date(limits = as.Date(c(min.date,max.date)), 
  date_breaks = "1 years",
  labels = date_format("%y")) +
 
  coord_cartesian(xlim = NULL, ylim = NULL, expand = FALSE) +  #gets rid of extra white space
  
  ggtitle("Northern Copepod Biomass") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Year") +
  ylab("Monthly anomaly\n(Log10 C m-3)")  # \n adds a line break
  
  #geom_line(data = data, aes(x = date, y = NorthernBiomassAnomaly), color = "black", grouping = "YYYY") +
  #geom_ribbon(data = data, aes(x = date, y = NorthernBiomassAnomaly), color = "black")



#### Southerns
pl.so.copes <- ggplot(data = data, aes(x = date, y = SouthernBiomassAnomaly3mo, fill = SoCop.color)) + #fill color based on pos or neg anoms
  geom_bar(stat = "identity", color = "black", size=0.05, na.rm = TRUE) +  #make the bar outline black
  scale_fill_manual(values = rev(anom.color)) +  #color the anoms w/ what you want
  
  theme_light() +
  theme(
    legend.position = "none",
    #panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  
  scale_x_date(limits = as.Date(c(min.date,max.date)), 
               date_breaks = "1 years",
               labels = date_format("%y")) +
  
  coord_cartesian(xlim = NULL, ylim = NULL, expand = FALSE) +  #gets rid of extra white space
  
  ggtitle("Southern Copepod Biomass") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Year") +
  ylab("Monthly anomaly\n(Log10 C m-3)")  # \n adds a line break


#### PDO and ONI
pl.PDO <- ggplot(data = data) + 
  geom_bar(aes(x = date, y = PDO, fill = PDO.color),  #fill color based on pos or neg anoms
           stat = "identity", color = "black", size=0.05, na.rm = TRUE) +  #make the bar outline black
  scale_fill_manual(values = rev(anom.color)) +  #color the anoms w/ what you want
  
  theme_light() +
  theme(
    legend.position = "none",
    #panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  
  scale_x_date(limits = as.Date(c(min.date,max.date)), 
               date_breaks = "1 years",
               labels = date_format("%y")) +
  
  coord_cartesian(xlim = NULL, ylim = NULL, expand = FALSE) +  #gets rid of extra white space
  
  ggtitle("PDO (bars) and ONI (line)") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Year") +
  ylab("SST anomaly (°C)") +

  geom_point(data = data, aes(x = date, y = ONI), color = "black") +
  geom_line(data = data, aes(x = date, y = ONI), color = "black") 
  

#### NH-5 50m T
pl.nh5_50m_T <- ggplot(data = data) +
  geom_bar(aes(x = date, y = X50mTAnom3mo, fill = "anything"),  #this fill is a possible factor- its for some reason needed and then overwritten with scale_fill_manual
           stat = "identity", color = "black", size=0.05, na.rm = TRUE) +  #make the bar outline black
  scale_fill_manual(values = "grey74") +
  
  theme_light() +
  theme(
    legend.position = "none",
    #panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  
  coord_cartesian(xlim = NULL, 
                  ylim = (NULL), expand = FALSE) +  #gets rid of extra white space
  
  scale_x_date(limits = as.Date(c(min.date,max.date)), 
               date_breaks = "1 years",
               labels = date_format("%y")) +
  ylim(-2,3.25)  +  #had to hard code the limits...would like to fix this
  
  ggtitle("NH-5 50-m Temperature") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Year") +
  ylab("Monthly anomaly (°C)")  



#### show the plots on the same page
####grid.draw is better than grid.arrange because it plots a table of plots all sized the same regardless of the size of the axis labels
grid.newpage()
final.plots <- grid.draw(rbind(ggplotGrob(pl.PDO), ggplotGrob(pl.nh5_50m_T),ggplotGrob(pl.no.copes),ggplotGrob(pl.so.copes), size = "last"),
                         recording = TRUE)

#this worked but was really really really frustrating :( gggrrrrr.....
####rrrraaaahhhhhh!!!!! NOW IT DOES NOT WORK :( :( :(!!!
dev.copy(png,'C:/_JF/HMSC/Data/Limacina/Figures/BiophysicalVars_3mo.png', width = 8.5, height = 10, units = "in",
         res = 300)
dev.off()


# None of these worked....
# ggsave("Frustrating!!.png", final.plots)
# 
# #### save the plot- this works for gg objects but the grid.draw is not gg
# ggsave("BiophysicalVars_3mo.png", plot = final.plots, device = "png", path = "C:/_JF/HMSC/Data/Limacina/Figures",
#        width = 8.5, height = 10, units = c("in"), dpi = 300)
# 
# png("C:/_JF/HMSC/Data/Limacina/Figures/BiophysicalVars_3mo2.png", width = 1000, height = 600, units = "px")
# grid.draw(final.plots)
# dev.off()
# 
# png("C:/_JF/HMSC/Data/Limacina/Figures/BiophysicalVars_3mo2.png", width = 8.5, height = 10, units = "in",
#        res = 300)









