setwd("E:/")
dev.off()
rm(list=ls())

library(dplyr)
library(ggplot2)
library(RColorBrewer)

CCI<-read.csv("NMS_NH05_AXES_SCORES_Dec2018.csv")

#subset the data for the Clim, the | is an and...
ClimData<-subset(CCI, (CCI$Year==[1998:2013])

#get the summ stats of the Clim data from 1999 through 2013- %>% this is dplyer syntax- e.g., parentheses
ClimStats <- ClimData %>% 
  group_by(Year, Month) %>%
  dplyr::summarise(min=min(dec2018axis1_neg), max=max(dec2018axis1_neg),mean=mean(dec2018axis1_neg))

#average by month and year for line plots
ClimStats2<-ClimStats %>%
  group_by(Month) %>%
  dplyr::summarise(min=min(min), max=max(max),mean=mean(mean))

#how many years do you want to plot as lines?
subsetyears<-subset(ClimStats, ClimStats$Year>"2013")
#geom_line needs year to be a factor
subsetyears$Year<-as.factor(subsetyears$Year)



ggplot() +geom_ribbon(data=ClimStats2, aes(ymin=min, ymax=max, x=Month,fill="Climatology (1999-2013)"))+  geom_line(data=subsetyears, aes(x=Month, y= mean, color=Year), size =0.8)+scale_fill_manual("",values=alpha(c("red"),0.1))+
 theme_bw(base_size=12) + scale_x_continuous(name="Month", limits=c(1,12),breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+scale_colour_manual("",values=brewer.pal(n=5,"Set1"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "top") + ylab("CCI Index") 


ggsave("CCI.png",width=4,height=3.5)




