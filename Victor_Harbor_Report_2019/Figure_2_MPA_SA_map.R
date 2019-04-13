####
####
#######Victor Harbor Analysis
#######Maps and Summary Analysis

#Open Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(mapproj)
library(tidyr)
library(raster)
library(rgdal)

#upload shark detection data analysis
setwd()
data<- read.csv() #detection data of all sharks 

#convert data to correct date/time format
dt<-as.POSIXlt(strptime(as.character(data$Date.and.Time..UTC.),format="%d/%m/%Y %H:%M"))
if(all(is.na(dt))){data$dt<-as.POSIXlt(strptime(as.character(data$Date.and.Time..UTC.),format="%Y-%m-%d %H:%M"))}
dtnum<-as.POSIXct(dt,origin="1960/01/01",tmz="UTC") 

# Convert to Local Time 
# for SA add 37800 seconds
timediff=37800
dtnumlocal<-dtnum+timediff
data$date<-as.Date(dtnumlocal)#get rid of the HH:MM

#save new dataframe with correct data-time data for future use
fname<-paste("dates_corrected")
fname<-paste(fname,".csv",sep="")
write.csv(data,file=fname)

#calculate summary data
#for example number of detections at each reciever 
PDAsum<-ddply(data, c("Station.Name","date" "Transmitter"), summarise,
              COUNT = n())
#can be modified to get seasonal data 

fname<-paste("days_detected")
fname<-paste(fname,".csv",sep="")
write.csv(PDAsum,file=fname)

##########################
#######MPA Map############
###### Figure 2###########
##########################
#plot MPA map of SA, with receiver locations 

#Load the base map file (.RData file type) from working directory
#map is availabe in the data folder for this project
##This loads an SpatialPolygonsDataFrame object called "aus" 

load(file="AUS_adm1.RData") #load an SpatialPolygonsDataFrame object called "aus" 
summary(aus) #establish projections

map <- fortify(aus)#converts to data frame
map1 <- getData('GADM', country='AUS',level=1)#specify the area of the world to plot
SAshp <- map[map1@data$NAME_1 == "South Australia", ] #subset SA


#import locations of recievers 
REC<- read.csv("SA receiver locations.csv", header = TRUE, sep = ",")
str(REC)

#import shapefile of SA MPAs
df <-readOGR(dsn="U:/Victor Harbour Report/Data/StateMarineParkNW_Zoning_shp",layer = "CONSERVATION_StateMarineParkNW_Zoning")
#available in data folder for this project 

ogrInfo(dsn="C:/Users/s2977661/My Documents/Victor Harbour Report/Data/StateMarineParkNW_Zoning_shp",layer = "CONSERVATION_StateMarineParkNW_Zoning")
summary(df)
# the projection of MPPs is not the same as the base map
#for all data to plot together, the shapefile and the map itself must have the same projection

df_trans<- spTransform(df, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) #transform the MPA shapefile to match the base map

#convert shapefile to dataframe
MPA_df <- as(df_trans, "data.frame")
head(MPA_df)
#SA MPAs is categorised by a number of different factorss including zone type
#see how many different zone types there are by using the unique() function
unique(MPA_df$RESNAME)

# subset reserves in area of interests near Victor Harbor
REC_MPAs <- subset(df_trans, RESNAME %in% c('Neptune Islands Group','Upper Gulf St Vincent','Encounter','Thorny Passage','Gambier Islands Group','Lower Yorke Peninsula','Eastern Spencer Gulf','Sir Joseph Banks Group','Western Kangaroo Island','Southern Spencer Gulf'))
plot(REC_MPAs)

#convert REC_MPAS to a data.frame so that can be read by ggplot
shapefile_df <- fortify(REC_MPAs)

#plot our SA map using geom_map, and overlay this map with the shapefile of the MPAs and points for each receiver.
p <-ggplot(data=REC) +
  geom_map(data=SAshp, map=SAshp,
           aes(x=long, y=lat, map_id=id), color="grey40", fill="#F0DDC5", size=0.6) +
  geom_polygon(data=shapefile_df, aes(x=long, y=lat, group=group), fill="darkolivegreen4", alpha=0.8)+
  #colour= will add lines around each MPA
  coord_map(projection="mercator", xlim=c(136.0,139.00), ylim=c(-34.120126,-35.857921)) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = "lightsteelblue1"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(axis.text=element_text(size=15,vjust=0.5, colour="black"))+
  theme(axis.title.y = element_text(angle=0,vjust=0.5, size=17)) +
  theme(axis.title.x = element_text(vjust=0.5, size=17))+
  geom_point(data=REC, aes(x=Longitude , y=Latitude), colour="black",fill="black", shape=19, size=2)

p
########END##############