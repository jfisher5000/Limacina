#######################
#This script downloads a set of biological and physical data from the NWFSC database 
#Computes 3 month running means 
#and plots them as colored time series 
#######################


rm(list=ls())

#library(RODBC)
#library(doBy)
library(pastecs)
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(plyer)
library(dplyr)


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
start.yr = min(data$YYYY)
start.mo = min(data$MM)

end.yr = max(data$YYYY)
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
#seems best to get the 3mo avg then turn each var into a ts before plotting
#only do for the bio data so start at 7
library(stinepack)

######after all this I think it is easier to create the 3mo running mean and time series variable for each plot...
##### oh you thought it would be so easy....
for(i in 7:no.col){

  #data.3mo[i] <- decaverage(data[ ,i], order = 6)
  #avg <- decaverage(.ts[], order = 6)
  #avg <- decaverage(data[ ,i], order = 6) #this creates a time series variable

 
  #turn your data into a timeseries 
  data.ts <- ts(data[ ,i], start = c(start.yr,start.mo), end = c(end.yr,end.mo), frequency = 12)
  
   #interpolate over NaNs
  #from library(stinepack)
  data.nonan = na.stinterp(data.ts)
  
  
  avg <- decaverage(data.nonan, order = 1) # 2 * order + 1 
  avg.series <- (avg$series[ ,1]) #1 are the smoothed data, 2 are the residuals

  #avg <- filter(data.nonan,c(1,1,1)/3, method = "convolution",sides = 2)
  #avg.series <- (avg$series[ ,1]) #1 are the smoothed data, 2 are the residuals
  #avg.series.num <- as.numeric(avg.series)   #change from time series to numeric
  #data.3mo[ ,i] <- (avg.series.num)   #stuff it back into the blank matrix
  #avg.num <- as.numeric(avg)   #stuff it back into the blank matrix
  data.3mo[ ,i] <- (avg.series)   #stuff it back into the blank matrix
}

data.3mo[ ,1] <- (data$SigmaPlotDate) #stuff it back into the blank matrix
data.3mo[ ,2] <- (data$YYYY)   #stuff it back into the blank matrix
data.3mo[ ,3] <- (data$MM)   #stuff it back into the blank matrix

####
# Let's try filter to also compute a moving average and compare it to decaverage

#ts objects cannot have nans
data.ONI.nonan = na.stinterp(data$ONI)

#turn your data into a timeseries 
ONI.ts <- ts(data.ONI.nonan[], start = c(start.yr,start.mo), end = c(end.yr,end.mo), frequency = 12)

#now apply the 3mo running average using 2 methods
ONIfilt <- filter(ONI.ts, c(1,1,1)/3, method="convolution", sides=2)
ONIdecavg <- decaverage(ONI.ts,order = 1)
ONIdecavg.series <- (ONIdecavg$series[ ,1]) #1 are the smoothed data, 2 are the residuals


plot(ONIfilt,ONIdecavg.series)
#they are the same

#run a correlation betwen 2 time series just to test
cor.test(ONIfilt,ONIdecavg.series,method="pearson")  #corr coeff is 1



#####
# okay now let's make some plots
#####

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(gridExtra) #organize multiple plots
library(ggfortify)
library(stinepack)
library(dplyr)

# Add a column with your condition for the colored anomalies
data <- data %>% mutate(mycolor = ifelse(y>0, "type1", "type2"))

#interpolate over the nas
no.cope.nonan = na.stinterp(data$NorthernBiomassAnomaly)
#create a timeseries object 
no.cope.ts <- ts(no.cope.nonan, start = c(start.yr,start.mo), end = c(end.yr,end.mo), frequency = 12)
#now apply the 3mo running average 
no.cope.3mo <- filter(no.cope.ts, c(1,1,1)/3, method="convolution", sides=2)
no.cope.3mo.num <- as.numeric(no.cope.3mo)

y.pos <- ifelse(data$NorthernBiomassAnomaly>0, data$NorthernBiomassAnomaly, 0)
y.neg <- ifelse(data$NorthernBiomassAnomaly<0, data$NorthernBiomassAnomaly, 0)

#deal with the dates
data <- data %>% mutate(date = as.Date(data$SigmaPlotDate, format = '%m/%d/%Y'))
min.date <- min(data$date)
max.date <- max(data$date)

# Add a column with your condition for the color
data <- data %>% mutate(mycolor = ifelse(data$NorthernBiomassAnomaly>0, "pos", "neg"))



###trying to get the anomalies colored...NOT WORKING in GGPLOT
anom.colors <- c("#ff3db7", "#4699dd")



#now plot with ggplot
pl.no.copes <- ggplot(data = data, aes(x = date, y = NorthernBiomassAnomaly, fill = mycolor)) + #fill color based on pos or neg anoms
  geom_bar(stat = "identity", color = "black", na.rm = TRUE) + #make the bar outline black
  scale_color_manual(values = anom.colors) +
    theme_light() +
  theme(
  legend.position = "none",
  #panel.border = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(limits = (c(min.date,max.date)), date_breaks = "2 years"
               #, labels = "%y" #this is supposed to format the yy but there's a conflict with date_breaks??
               )+               
  #xlim(min(data$date),max(data$date))+
  ggtitle("Northern Copepod Biomass")+
  xlab("Year") +
  ylab("Monthyl biomass anomaly Log10 C m-3")




y.pos <- ifelse(no.cope.3mo>0, no.cope.3mo, 0)
y.neg <- ifelse(no.cope.3mo<0, no.cope.3mo, 0)

autoplot(y.pos, ts.geom = 'bar', fill = 'red')
autoplot(y.neg, ts.geom = 'ribbon', fill = 'dodgerblue4')

#try plotting with library(ggfortify)
autoplot(no.cope.3mo, ts.geom = 'bar', fill = 'dodgerblue4')

ggplot(data, aes(x = SigmaPlotDate, y = SouthernBiomassAnomaly)) +
  geom_ribbon(stat = "identity", color = "red", size=0.75, na.rm = TRUE)


pl.so.copes <- ggplot(data, aes(x = SigmaPlotDate, y = SouthernBiomassAnomaly)) +
  geom_bar(stat = "identity", color = "red", size=0.75, na.rm = TRUE)

pl.PDO <- ggplot(data, aes(x = SigmaPlotDate, y = PDO)) +
  geom_bar(stat = "identity", color = "red", size=0.75, na.rm = TRUE)

pl.ONI <- ggplot(data, aes(x = SigmaPlotDate, y = ONI)) +
  geom_bar(stat = "identity", color = "red", size=0.75, na.rm = TRUE)

# Show the plots on the same page
grid.arrange(pl.so.copes, pl.no.copes, pl.ONI, pl.PDO, ncol = 1, nrow = 4)




#######to make variable color plots- i.e. colored anomalies
#from https://www.r-graph-gallery.com/302-lollipop-chart-with-conditional-color/

# Create data (this takes more sense with a numerical X axis)
x=seq(0, 2*pi, length.out=100)
data=data.frame(x=x, y=sin(x) + rnorm(100, sd=0.2))

# Add a column with your condition for the color
data=data %>% mutate(mycolor = ifelse(y>0, "type1", "type2"))

# plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")


######this code is supposed to put ggplot2 graphs together to make them look good-
# from https://www.r-graph-gallery.com/261-multiple-graphs-on-same-page/ 


# libraries
library(ggplot2)
library(gridExtra)

# Make 3 simple graphics:
g1=ggplot(mtcars, aes(x=qsec)) + geom_density(fill="slateblue")
g2=ggplot(mtcars, aes(x=drat, y=qsec, color=cyl)) + geom_point(size=5) + theme(legend.position="none")
g3=ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=cyl)) + geom_boxplot() + theme(legend.position="none")
g4=ggplot(mtcars , aes(x=factor(cyl), fill=factor(cyl))) +  geom_bar()

# Show the 4 plots on the same page
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)



