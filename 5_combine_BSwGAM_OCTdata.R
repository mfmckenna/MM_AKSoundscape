# AK data analysis combining Gambel with Bering strait- October 2015 (19-31)

#added analysis of ice free days for baleen whale occurrence analysis 

library(gridExtra)
library(ggplot2)
library(lubridate)
rm(list=ls())

#--------------------------------
## READ IN DATA-- 
#--------------------------------
# BS site 
#--------------------------------
load("G:\\My Drive\\ActiveProjects\\MM_AKsoundscape\\data\\dataALL_BeringStraitOct2015")
dataBS = dataALL 
dataBS$Day = as.Date(dataBS$DateFstart)
rm(dataALL)
dataBSm = reshape :: melt(dataBS, id.vars = "DateFstart", 
                          measure.vars = c("Bmy","Bal","Ubi","Anth",'nShips'))
dataBSm$hr = hour(dataBSm$DateFstart)
colnames(dataBSm)[1] = "DateTime"
dataBSm$Date = as.Date(dataBSm$DateTime)

#what is the range in the distance of ships?
ds = dataBS[dataBS$nShips >0,]
( unique( round(as.numeric( as.character( ds$minDist) ) /1000 ) ) )
unique(ds$Day)
# 13 days with AIS vessels in area 8-50 km away, only heard on 3 days
dsh = ds[ds$Anth >0,]
unique(dsh$Day)

# GAMBELL- year of data
#--------------------------------
load("G:\\My Drive\\ActiveProjects\\MM_AKsoundscape\\data\\dataSpAShWeIce")
dataG = dataSpAShWeIce
dataG$Day = as.Date(dataG$dataTime)
dataGt = dataG[ dataG$Day >= min(dataBS$Day) & dataG$Day <=max(dataBS$Day) , ]
cols <- sapply(dataG, is.logical)
dataGt[,cols] <- lapply(dataGt[,cols], as.numeric)
dataGtm = reshape :: melt(dataGt, id.vars = "startDateTime", 
                                   measure.vars = c("Bmy","Bal","Ubi","Anth",'nShips'))
dataGtm$hr = hour(dataGtm$startDateTime)
colnames(dataGtm)[1] = "DateTime"
dataGtm$Date = as.Date(dataGtm$DateTime)
unique( dataGtm$Date )
# 3 days with AIS vessels in area 16 km away, not heard

#what is the range in the distance of ships?
ds = dataBS[dataGt$nShips >0,]
( unique( round(as.numeric( as.character( ds$minDist) ) /1000 ) ) )

#What is the ice-free period and whale presence-- Gambell
as.data.frame(colnames(dataG))
#number of days:
length( unique(dataG[dataG$ice_concentration_20km == 0,156]) )#ice free days
length( unique(dataG[dataG$ice_concentration_20km >0,156]) ) #ice  days
length( unique(dataG[,156]) ) #all days
plot(dataG$Day,dataG$ice_concentration_20km) #check ice days
dataG[is.na(dataG$Day),] #no rows with NA

min( unique(dataG[,156]) ) #start date
max( unique(dataG[,156]) ) #end date

#-----------------------------------------------------
#whale occurrence without ice, baleen whale co-occurrence 
#-----------------------------------------------------
length( unique(dataG$Day )) #all days
idxNoIce = subset(dataG,  dataG$ice_concentration_20km == 0)
idxNoIce = subset(idxNoIce, !is.na(idxNoIce$ice_concentration_20km ) ) #ice free days
length( unique(idxNoIce$Day) )

#days with baleen whales
length( unique(dataG[dataG$Bal >0,156]) )        # days with baleen present
length( unique(idxNoIce[idxNoIce$Bal >0, 156]) ) #ice free days with baleen
(( length( unique(idxNoIce[idxNoIce$Bal >0, 156])) ) /
    length( unique(idxNoIce$Day) )) *100 #% ice free days with baleen
unique(idxNoIce[idxNoIce$Bal >0, 156])   #ice free dates with whales

#days with bowhead whales
length( unique(dataG[dataG$Bmy >0,156]) ) #all days
length( unique(idxNoIce[idxNoIce$Bmy >0, 156]) ) #ice free days with baleen
(( length( unique(idxNoIce[idxNoIce$Bmy >0, 156])) ) /
    length( unique(idxNoIce$Day) )) *100 #% ice free days with baleen
unique(idxNoIce[idxNoIce$Bmy >0, 156])   #ice free dates with whales

#OVERLAP--- get unique days for both and compare, otherwise it is hourly comparisons!!
#hourly
length( unique(idxNoIce[idxNoIce$Bmy >0 & idxNoIce$Bal >0, 156] ) ) 
unique(idxNoIce[idxNoIce$Bmy >0 & idxNoIce$Bal >0, 156] ) 
overlapHR = idxNoIce[idxNoIce$Bmy >0 & idxNoIce$Bal >0,]
#daily
uBmy = unique(idxNoIce[idxNoIce$Bmy >0, 156] ) 
uBal = unique(idxNoIce[idxNoIce$Bal >0, 156] ) 
idxM = which(uBal %in% uBmy) 
uBal[idxM]
idxM = which(uBmy %in% uBal) 
uBmy[idxM] 


#-----------------------------------------------------
#whale occurrence ALL DAYS, baleen whale co-occurrence 
#-----------------------------------------------------
nDays = length( unique(dataG$Day )) #all days
dcol = as.data.frame(colnames( dataG))
#days with baleen whales= 24 days
( length( unique(dataG[dataG$Bal >0,156]) ) )      # days with baleen present
(length( unique(dataG[dataG$Bal >0,156]) )  /
  nDays) *100 #% ice free days with baleen
unique(dataG[dataG$Bal >0, 156])   #dates with whales

#days with bowhead whales.. not working missing idxIce???
length( unique(dataG[dataG$Bmy >0,156]) )    #all days
length( unique(idxIce[idxIce$Bmy >0, 156]) ) #ice free days with baleen
(( length( unique(idxIce[idxIce$Bmy >0, 156])) ) /
    length( unique(idxIce$Day) )) *100 #% ice free days with baleen
unique(idxIce[idxIce$Bmy >0, 156])   #ice free dates with whales

#OVERLAP--- get unique days for both and compare, otherwise it is hourly comparisions!!
#hourly
length( unique(dataG[dataG$Bmy >0 & dataG$Bal >0, 156] ) ) 
unique(dataG[dataG$Bmy >0 & dataG$Bal >0, c(50,153,156)] ) 
overlapHR = dataG[dataG$Bmy >0 & dataG$Bal >0,]
#daily
uBmy = unique(dataG[dataG$Bmy >0, 156] ) 
uBal = unique(dataG[dataG$Bal >0, 156] ) 
idxM = which(uBal %in% uBmy) 
uBal[idxM]
idxM = which(uBmy %in% uBal) 
length(uBmy[idxM] )



rm(cols,dataBS,dataG,dataGt,dataSpAShWeIce)
#--------------------------------
## TILE PLOTS- by dat
#--------------------------------
uSource = unique(dataGtm$variable)

# BS site 
#--------------------------------
dayBS = NULL
for (ii in 1:length(uSource)){
  dtmp = dataBSm[dataBSm$variable == uSource[ii],]
  uday = unique(dtmp$Date)
  for (dd in 1:length(uday)){
    dtmp2 = dtmp[dtmp$Date == uday[dd],]
    dayBS = rbind(dayBS, c((as.character( uday[dd])),  as.character(uSource[ii]), sum(as.numeric(dtmp2$value),na.rm = T), nrow(dtmp2)))
  }
  rm(dtmp,uday,dtmp2)
}
colnames(dayBS) = c("Day","variable","total","samples")
dayBS = as.data.frame(dayBS)
dayBS$Day2 = as.Date(dayBS$Day)
dayBS$perSample = (as.numeric(as.character(dayBS$total))/as.numeric(as.character(dayBS$samples)))*100
#reorder axis labels
uvar = unique(dayBS$variable)
uorder = c("e","d","c","b","a")
ulabs = c("AIS ships", "Anthropogenic", "Unknown biologic", "Other baleen whales", "Bowhead" )
for (i in 1:length(uvar)){
  idxO = which(dayBS$variable == uvar[i])
  dayBS$Order[idxO] = paste0(uorder[i],uvar[i])
}

pdayBS = ggplot(dayBS, aes(Day2, Order, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  scale_y_discrete(labels = ulabs )+
  labs(title = "(B) Bering Strait", fill = "% Samples") +
  xlab("") +
  ylab("")

# GM site
#--------------------------------
dayGM = NULL
for (ii in 1:length(uSource)){
  dtmp = dataGtm[dataGtm$variable == uSource[ii],]
  uday = unique(dtmp$Date)
  for (dd in 1:length(uday)){
    dtmp2 = dtmp[dtmp$Date == uday[dd],]
    dayGM = rbind(dayGM, c((as.character( uday[dd])),  as.character(uSource[ii]), sum(as.numeric(dtmp2$value),na.rm = T), nrow(dtmp2)))
  }
  rm(dtmp,uday,dtmp2)
}
colnames(dayGM) = c("Day","variable","total","samples")
dayGM = as.data.frame(dayGM)
dayGM$Day2 = as.Date(dayGM$Day)
dayGM$perSample = (as.numeric(as.character(dayGM$total))/as.numeric(as.character(dayGM$samples)))*100
#reorder axis labels
uvar = unique(dayGM$variable)
uorder = c("e","d","c","b","a")
ulabs = c("AIS ships", "Anthropogenic", "Unknown biologic", "Other baleen whales", "Bowhead" )
for (i in 1:length(uvar)){
  idxO = which(dayGM$variable == uvar[i])
  dayGM$Order[idxO] = paste0(uorder[i],uvar[i])
}

pdayGM = ggplot(dayGM, aes(Day2, Order, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  scale_y_discrete(labels = ulabs )+
  labs(title = "(A) Gambell", fill = "% Samples") +
  xlab("") +
  ylab("")

grid.arrange(pdayGM,pdayBS,nrow = 2, ncol = 1) #FIG. 3 in paper


