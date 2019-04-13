####
####
#######Victor Harbor Analysis#########
#######Speices residency patterns in Victor Harbor
#this code used to create figure 6
#This plot illustrates the number of days each receiver recorded detections and which shark species was detected 
##Authors: Samantha Munroe and Vinay Udyawer


#open libraries 
library(ggplot2)
library(viridis)
library(scales)

################# READ IN FOLDER CONTAINING DATA FILES ######################
#
#********************* Choose Working Directory ******************************
setwd()
#wd includes accoustic detection data folders grouped by receiver, extracted using VUE 

#************************** Set Start and End Date for Monitoring Period***************************
#
startday<-"2016-04-01"
endday<-"2018-11-30"
#
#*************************** Construct Timeline ******************************
#
startday
endday
#
dateset<-as.Date(startday):as.Date(endday)
FinalTable<-data.frame(Date=as.Date(dateset,origin="1970/01/01"))
FinalTable$date<-cut(FinalTable$Date, breaks="day", labels=NULL, start.on.monday=FALSE, right=FALSE)
FinalTable$Date<-NULL
FinalTable
#
#************** Cycle though files within the working directory **************
#************** Create empty folders to fill with results ********************

indi<-list.files()
results<-matrix(NA,nrow=nrow(FinalTable), ncol=length(indi))
lab<-unlist(strsplit(as.character(indi),"[.]"))[2*(1:length(strsplit(as.character(indi),"[.]")))-1]
datalist = list()#create empty list 

#for each receiver 
for (j in 1:length(indi)) {
  tagdata<-read.csv(indi[j], header=TRUE, sep=",")

################### REFINE RAW DATA AND SORT TIME DATA   #####################

names(tagdata)[1] <- "Date.Time"
  
#remove unwanted fields
tagdata$Transmitter<-NULL
tagdata$Transmitter.Name<-NULL
tagdata$Transmitter.Serial<-NULL
tagdata$Sensor.Value<-NULL
tagdata$Sensor.Unit<-NULL
tagdata$Latitude<-NULL
tagdata$Longitude<-NULL
tagdata$Receiver<-NULL
  
#**** This leaves the date-time field, tag code number and receiver numbers **
#***************** Format the date and time correctly for R ******************
#
dt<-as.POSIXlt(strptime(as.character(tagdata$Date.Time),format="%d/%m/%Y %H:%M")) 
if(all(is.na(dt))){dt<-as.POSIXlt(strptime(as.character(tagdata$Date.Time),format="%Y-%m-%d %H:%M"))}
dtnum<-as.POSIXct(dt,origin="1960/01/01",tmz="UTC") 
#
#************************** Convert to Local Time***************************** 
#*************************** for SA add 37800 *******************************
timediff=37800
dtnumlocal<-dtnum+timediff
#
#*************************** save as date data ******************************
#
date<-as.Date(dtnumlocal)

#********** Construct a Frequency Table of number of detections **************
#
abafreq<-data.frame(date,tagdata$Station.Name, tagdata$Species)
abafreq-abafreq[order(abafreq$date),] #sort in chronological order
freqtable<-table(abafreq)
#
#************* Merge Table with Start-End dates with Frequencies **************
#
mergeTable<-merge(FinalTable,freqtable, all.x=TRUE, all.y=FALSE) #merge
mergeTable$Freq[is.na(mergeTable$Freq)]<-0 
mergeTable$tagdata.Station.Name[is.na(mergeTable$tagdata.Station.Name)]<-tagdata$Station.Name[1]
mergeTable$tagdata.Species[is.na(mergeTable$tagdata.Species)]<-tagdata$Species #species
ra<-length(mergeTable$Freq)
for (i in 1:ra){
    if(mergeTable$Freq[i] >=1){mergeTable$Frequency[i]<-j}else{mergeTable$Frequency[i]<-0}
mergeTable$Date<-as.Date(mergeTable$date)
da<-length(mergeTable$Freq)
  for (i in 1:da){
    if(mergeTable$Freq[i] >=1){mergeTable$Index[i]<-1}else{mergeTable$Index[i]<-0} 
  }
datalist[[j]]<-mergeTable
}
}
#
#
#################################### SAVE AND DISPLAY DATA #############################
#
big_data = do.call(rbind, datalist)
big_data[big_data==0]<-NA
big_data$date <- as.Date(big_data$date)

#save data table
fname<-paste("Receiver_Detections")
fname<-paste(fname,".csv",sep="")
write.csv(big_data,file=fname)


ggplot(big_data,aes(y=Frequency,x=date, by=tagdata.Station.Name))+geom_point(aes(shape=tagdata.Species, fill=tagdata.Species, color=tagdata.Species),size=4.3)+
  scale_color_viridis(discrete=TRUE, begin = 0.25, end = .8, option="inferno")+
  scale_x_date(name="Date", expand = c(0, 0), date_breaks = "2 month", date_labels =  "%b\n%Y")+
  theme_bw() +
  theme(panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA))+
  theme(axis.text.x = element_text(size=10, colour="black"),axis.text.y=element_text(size=12, colour="black"),
      panel.grid.minor = element_blank(), axis.title=element_text(size=14))+
  scale_y_continuous(name="Reciever", labels=c("1"="Granite Island", "2"="Kings Head", "3"="Seal Island", "4"="The Bluff"))
  
#################################################################################
################################# END SCRIPT ####################################
#################################################################################
                   
                   
