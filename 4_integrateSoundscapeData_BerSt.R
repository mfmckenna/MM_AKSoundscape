# Code to integrate WCS-COA soundscape data-- Bering Strait Site
# species detection, sound levels, AIS, wind

rm(list=ls())

# !!!!! IF YOU JUST WANT TO GENERATE PLOTS-- START on line ~391 !!!!

#-------------------------------------
#INFORMATION ON THE INPUT DATA SOURCES
#-------------------------------------
#1) Acoustic data
# 2015.14
#- sample plan: 5  minutes on 55 minutes off, rotating sample 0,10,20,30,40,50 minutes into hour
#- sample rate: 48 kHz
#- start/end dates: 19 Oct 2015 - 06 June 2015
#- timezone: local AK time

# 1a) Species ID tables (see above description of acoustic data)
#Manual identification of all sounds present in the sample

# 1b) Sound pressure levels (see above description of acoustic data)
#LZeq values were calculated over the entire sample, either 5 minutes or 10 minutes in a given hour.

# 2) AIS data
#- row for each ship location in the area- not sequential time
#- time zone is likely GMT- confirmed!!

# 3) Wind data- https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
#- when reported: unique( dataWind$min ), mostly 16,36,56- but some other random minuts
#- 3 measure per hour
#- The times listed at Gambell Airport, AK and for all LCD stations are recorded in Local Standard Time (LST), UTC-10:00

# 4) Ice coverage data

#-----------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(reshape)
library(ggplot2)
library(chron)
library(data.table)
library(openxlsx)
library(vroom)
#-----------------------------------------------------------
## IMPORT AND FORMAT DATA-- time period of interest and only Gambell
#-----------------------------------------------------------
# 1a) ACOUSTIC DATA- species detections
# STILL PROCESSING....
#-----------------------------------------------------------
dirSp  = list.dirs("C:\\Users\\mckenna6\\Box Sync\\WCS_AK_BeringStrait_AUDIO\\copySelTables")
multmerge = function(path){
  filenames = list.files(path=path, pattern = "selections", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
# list.files(path=dirSp, pattern = "selections", full.names=TRUE, recursive = TRUE)
SPS = as.data.frame( multmerge(dirSp) )

#DATE FORMAT- separate out and format date, force tz and start time of 00 seconds
tmp = force_tz( as.POSIXct(SPS$`Begin Date Time`,TZ=GMT), tzone = "GMT")
tmp2 = paste( paste(year(tmp),month(tmp),day(tmp),sep = "/") , paste(hour(tmp),minute(tmp),"00", sep=":") )
SPS$DateFstart = force_tz( as.POSIXct(tmp2 ), tzone = "GMT" )
SPS$DateFend = SPS$DateFstart + (5*60)

SPS$sps2 = tolower(SPS$sps)
SPS$sps2 = trimws(SPS$sps2)
allSounds = unique(trimws(unlist(strsplit(as.character(SPS$sps2), ","))))
allSounds

dataSps = SPS [SPS$sps2 != "example",]
allSounds = unique(trimws(unlist(strsplit(as.character(dataSps$sps2), ","))))
dataSps$sps2= gsub("anthc","tmp" ,dataSps$sps2)
dataSps$sps2= gsub("anth","unk" ,dataSps$sps2)
dataSps$sps2= gsub("tmp","anth" ,dataSps$sps2)
dataSps$sps2= gsub("uni","ubi" ,dataSps$sps2)
allSounds = unique(trimws(unlist(strsplit(as.character(dataSps$sps2), ","))))
allSounds 

#redo species labels with grepl (see code above)
dataSps$Ubi  = grepl("ubi", dataSps$sps2)  #unknown biologica- fish!!
dataSps$Unk  = grepl("unk", dataSps$sps2)  #unknown source  
dataSps$Bal  = grepl("bal", dataSps$sps2) #baleen
dataSps$Anth = grepl("anth",dataSps$sps2) #anthropogenic
dataSps$Bmy  = grepl("bmy", dataSps$sps2)   #bowhead
dataSps$Rain = grepl("rain",dataSps$sps2) 

cols <- sapply(dataSps, is.logical)
dataSps[,cols] <- lapply(dataSps[,cols], as.numeric)
dataSps$Sounds = rowSums(dataSps[,cols])
rm(tmp,tmp2,dirSP,dirSp,SPS,cols)

#-----------------------------------------------------------
# 1b) ACOUSTIC DATA- SPLs
#-----------------------------------------------------------
dirs   = list.dirs("C:\\Users\\mckenna6\\Dropbox\\WCS_COA AK ship noise project")
dirSPL = dirs[ grepl("AMBIENT NOISE DATA", dirs) ]

SPL_201514 = list.files(dirSPL,pattern = "2015.14.xlsx" ,full.names = TRUE)
dSPL11 = read_xlsx(SPL_201514[1],col_names = F)
dSPL11 = as.data.frame( t(dSPL11) )
colnames(dSPL11) = as.character(dSPL11[1,])
dSPL11 = dSPL11[2:nrow(dSPL11),]
colnames(dSPL11)[38:41] = c( "clipped","Num","dBA","Day1Nigh2")
colnames(dSPL11)[1]  = "Date"
dSPL11$Date = force_tz(convertToDateTime(dSPL11$Date,origin="1900-01-01"), tzone = "GMT")
dataSPL = dSPL11
#FORMAT date time
dataSPL$yr  = year(dataSPL$Date)
dataSPL$mth = month(dataSPL$Date)
dataSPL$day = day(dataSPL$Date)
dataSPL$hr  = hour(dataSPL$Date)
dataSPL$min = minute(dataSPL$Date)
dataSPL$sec = second(dataSPL$Date) #a bit too exact!

dataSPL = dataSPL[dataSPL$yr > 2000,] #this gets rid of the 10,50,90 columns in the 2015.2 deployment data
dataSPL = Filter(function(x)!all(is.na(x)), dataSPL) #remove any rows with all NANs- should not be any because data cleaned up before this step
rm(dSPL11,dirs,dirSPL,SPL_201514)

#truncate to just data for October
dataSPL = dataSPL[dataSPL$mth == "10",]

#-----------------------------------------------------------
# 2) AIS sat (data were compiled using readAIS_HarrisSat.R)- this are raw ship locations, not summarized by any time
#-----------------------------------------------------------
load("E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\AIS_Sat_Harris\\AISsMod.RData")
dataAISs = dataOut
rm(dataOut)
#remove ships > 50 km from Gambell acoustic monitoring station
dataAISs = dataAISs[dataAISs$dist2Ber < 50000,]
dataAISs$dataTime1 = paste(dataAISs$year,dataAISs$month,dataAISs$day, sep = "/")
dataAISs$dataTime2 = paste(dataAISs$hour,dataAISs$min,dataAISs$sec, sep = ":")
dataAISs$dataTimeGMT  = as.POSIXlt (chron( dates = dataAISs$dataTime1, time = dataAISs$dataTime2, format = c("Y/m/d","h:m:s")), tz = "GMT")
#TIME CONVERSION: since all other times are formatted in GMT, but actually AK standard time- 
# going to convert AIS time AK standard by subtracting 10 hours and keeping GMT lable
#dataAISs$dataTimeGMT[1]
#dataAISs$dataTimeGMT[1] - (10*60*60)
#force_tz( dataAISs$dataTimeGMT[1] - (10*60*60),tzone = "GMT")
dataAISs$dataTime = force_tz( dataAISs$dataTimeGMT - (10*60*60),tzone = "GMT")
#remove rows with all NAs
dataAISs = Filter(function(x)!all(is.na(x)), dataAISs)

#-----------------------------------------------------------
# 3) wind data
#-----------------------------------------------------------
dir = "E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\"
fil = "BS_weatherLand2015.csv"
dataWind = as.data.frame( read.csv(paste0(dir,fil)) )
dataWind = Filter(function(x)!all(is.na(x)), dataWind)
#formate the date-- https://www.ndbc.noaa.gov/measdes.shtml -- it is UTC
dataWind$DateF = ymd_hms( paste(dataWind$X.YY,dataWind$MM,dataWind$DD, dataWind$hh,dataWind$mm,0,sep=" "  ))
dataWind$Previous = shift(dataWind$DateF) 
rm(fil, dir)
#ggplot(dataWind, aes(DateF, as.numeric(as.character(WSPD)), color = MM ))+
  #geom_point()
dataWind = dataWind[63416:70838,]

#-----------------------------------------------------------
# 4) Ice coverage-- daily percent coverage
#-----------------------------------------------------------
ice = read.csv("E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\recordings_ice_concentration.csv")
ice = ice[complete.cases(ice), ]
dataIce = ice[ice$deployment_id == "2015.14",]
dataIce$day2 = as.Date(dataIce$day,format = "%m/%d/%Y")
dataIce = dataIce[1:298,]
rm(ice)

#-----------------------------------------------------------
# 5) Tide- hourly data at sation in Nome, Norton Sound AK
# https://tidesandcurrents.noaa.gov/waterlevels.html?id=9468756&units=standard&bdate=20160701&edate=20160731&timezone=GMT&datum=MLLW&interval=h&action=
#-----------------------------------------------------------
filTide = list.files("E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data",pattern = "CO-OPS",full.names = TRUE)
dataTide <- vroom(filTide)
dataTide$Hour  = paste0((dataTide$`Time (GMT)`)/(60*60),":00:00")
dataTide$Date2 = ymd_hms(paste(dataTide$Date, dataTide$Hour, sep = " ")) 
dataTide = dataTide[order(dataTide$Date2),]
#plot(dataTide$Date2)
dataTide$Diff  = c(NA, diff(dataTide$`Verified (ft)`,na.omit = FALSE))
dataTide$Sign  = c(0,!!diff(sign(dataTide$Diff)))

#not working- trying to lable phase of tide, but diff will work
#find colums with 1 and if sign of previous is negative- low tide ; if sign of previous is positive = high tide
#dataTide$Tide = NULL
#for (ii in 3:nrow(dataTide)) {
  #if(dataTide$Diff[ii] < 0 ) {dataTide$Tide[ii] = "Ebb"}
  #if(dataTide$Diff[ii] > 0 ) {dataTide$Tide[ii] = "Flow"}
  #if(dataTide$Sign[ii] == 1 && dataTide$Diff[ii-1] < 1 ) {dataTide$Tide[ii] = "L"}
  #if(dataTide$Sign[ii] == 1 && dataTide$Diff[ii-1] > 1 ) {dataTide$Tide[ii] = "H"}
#}

#compare with plot on website: https://tidesandcurrents.noaa.gov/map/index.html
#ggplot(dataTidetrim, aes(Date2,`Verified (ft)`)) +
 # geom_line() #data matches 
dataTide = dataTide[2209:2952,]
#dataTide$Date[2209]
#dataTide$Date[2952]
#rm(dataTidetrim)
rm(filTide)

#-----------------------------------------------------------
#-----------------------------------------------------------
## SUMMARY PLOTS
#-----------------------------------------------------------
#-----------------------------------------------------------
pSp = ggplot(dataSps ,(aes(DateFstart,Sounds) ))+
  geom_point() + ggtitle("Audits")
pSPL = ggplot(dataSPL , aes(Date,`125`))+
  geom_point()+
  ggtitle("SPL 125Hz")
pAIS = ggplot(dataAISs , aes(dataTime,dist2Ber))+
  geom_point()+
  ggtitle("AIS locations")
pIce = ggplot(dataIce , aes(day2,ice_concentration_20km) )+
  geom_point()+
  ggtitle("Ice Coverage")
pTide = ggplot(dataTide , aes(Date,Diff))+
  geom_point()+
  ggtitle("Tide change")
pWind = ggplot(dataWind , aes(DateF,as.numeric( as.character(WSPD) )) )+
  geom_point()+
  ggtitle("Wind Speed")
library(gridExtra)
grid.arrange(pSp,pSPL,pAIS, pTide,pIce,pWind, ncol = 3,nrow =2)

#-----------------------------------------------------------
#-----------------------------------------------------------
## COMBINE DATA-- clean up as I go
#-----------------------------------------------------------
#-----------------------------------------------------------
#SPECIES WITH SPL
#-----------------------------------------------------------
tmp = dataSPL$Date
tmp2 = paste( paste(year(tmp),month(tmp),day(tmp),sep = "/") , paste(hour(tmp),minute(tmp),"00", sep=":") )
dataSPL$DateFstart = force_tz( as.POSIXct(tmp2 ), tzone = "GMT" )
dataSPL$DateFstart
dataSps$DateFstart

dataSpsSPL = merge(dataSps,dataSPL, by = "DateFstart")

dCols = data.frame(colnames(dataSpsSPL)) 
dCols
dataSpsSPL = dataSpsSPL[c(1,14:27,29:67)]
dCols = data.frame(colnames(dataSpsSPL)) 
dCols

#dataSpsSPL with Ships-- for each SPL find ships present
#-----------------------------------------------------------
shipInfo = NULL 
for (ii in 1 : nrow(dataSpsSPL)){ # ii = 106-- 1 ship, ii = 111 -- 5 ships, ii = 5--no speed?
  
  dateSTP = as.character(dataSpsSPL$DateFstart[ii])
  tst2 = nrow( dataAISs[between(dataAISs$dataTime, dataSpsSPL$DateFstart[ii], dataSpsSPL$DateFend[ii] ) ,] ) 
  tmp1 =       dataAISs[between(dataAISs$dataTime, dataSpsSPL$DateFstart[ii], dataSpsSPL$DateFend[ii] ) ,]  
  
  if (tst2 > 0 ){ 
    nShips = length(unique(tmp1$MMSI))
    #cat("Ships present in: ", as.character(dataSpsSPL$DateFstart[ii]), " in row ", ii, "(points: ", tst2, "/ships: ", nShips, ")\n" )
    if(nShips > 1) { #more than one ship present, need to combine into one row
      info = NULL
      for (ss in 1:length(unique(tmp1$MMSI)) ){
        stmp = tmp1[tmp1$MMSI == unique(tmp1$MMSI)[ss],]
        info = rbind(info, c(unique(stmp$MMSI), tst2, 
                             mean(stmp$dist2Ber,na.rm = T), max(stmp$dist2Ber,na.rm = T), min(stmp$dist2Ber,na.rm = T),
                             mean(as.numeric(as.character(stmp$SOG,na.rm = T))),
                             stmp$Latitude[stmp$dist2Ber  == min(stmp$dist2Ber)], 
                             stmp$Longitude[stmp$dist2Ber == min(stmp$dist2Ber)] ) )
        #sometimes this loop has awarning because values are the same... so ignores the second value
      }
      ship = gsub('\\"',"", toString(shQuote(info[,1])))
      pts  = gsub('\\"',"", toString(shQuote(info[,2])))
      ud   = gsub('\\"',"", toString(shQuote(info[,3])))
      mxd  = gsub('\\"',"", toString(shQuote(info[,4])))
      mnd  = gsub('\\"',"", toString(shQuote(info[,5])))
      usp  = gsub('\\"',"", toString(shQuote(info[,6])))
      mlat  = gsub('\\"',"", toString(shQuote(info[,7])))
      mlon  = gsub('\\"',"", toString(shQuote(info[,8])))
      mspd  = mean(as.numeric( info[,6] ), na.rm = TRUE)
      mdist = min(as.numeric(  info[,5] ), na.rm = TRUE)
    
      shipInfo = rbind(shipInfo, c(dateSTP,nShips,ship,pts,ud,mxd,mnd,usp,mlat,mlon,mspd,mdist) )
      rm(ship,pts,ud,mxd,mnd,usp,mlat,mlon)
      
    }else if (nShips == 1){
      #unique ships, MMMSI, points, average distance, max distance, min distance, average SOG, lat/lon of minimum distance, heading
      shipInfo = rbind(shipInfo, c(dateSTP,nShips, unique(tmp1$MMSI), tst2, 
                                    mean(tmp1$dist2Ber,na.rm = T),max(tmp1$dist2Ber,na.rm = T), min(tmp1$dist2Ber,na.rm = T),
                                    mean(as.numeric(as.character(tmp1$SOG,na.rm = T))),
                                    tmp1$Latitude[tmp1$dist2Ber  == min(tmp1$dist2Ber)][1], 
                                    tmp1$Longitude[tmp1$dist2Ber == min(tmp1$dist2Ber)][1],
                                    mean(as.numeric(as.character(tmp1$SOG,na.rm = T))), mean(tmp1$dist2Ber,na.rm = T)) )
                                    }
    
  } else { #no ships present
    shipInfo = rbind(shipInfo, c(dateSTP,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA) )    }
}

shipInfo = data.frame(shipInfo)
colnames(shipInfo) = c("DateTime","nShips","MMSI", "nLocations","uDist","mxDist","mnDist","uSOG","CPAlat","CPAlon","mSpd","minDist")

#HOW MANY time steps with ships... some checks to make sure data are alining correctly
shipInfo$nShips = as.numeric( as.character(shipInfo$nShips) )
nrow(shipInfo[shipInfo$nShips > 0,]) #185 days
nrow(shipInfo[shipInfo$nShips > 1,]) #6 days
#tmpS = (shipInfo[shipInfo$nShips > 1,]) #used this to check if times matched, row 688

dataSpsSPLShip = cbind(dataSpsSPL, shipInfo)
dataSpsSPLShip$Ship = as.numeric( as.character( dataSpsSPLShip$nShips ) )
dataSpsSPLShip$Ship[is.na(dataSpsSPLShip$Ship)] = 0
dCols = data.frame(colnames(dataSpsSPLShip)) 
dCols

#-----------------------------------------------------------
#SPLships with wind
dataWind$dataTime = force_tz( dataWind$DateF, tzone="GMT")
dataWind$WSPD2     = as.numeric( as.character(dataWind$WSPD)) #blanks become NAs- ignore warning!

weatherInfo = NULL
for (ii in 1 : nrow(dataSpsSPLShip) ){ #ii = 1
  #selects data 15 min before and 15 min after the 5 minute sample so 30 minute average
  tst2 = nrow( dataWind[ between(dataWind$dataTime, dataSpsSPLShip$DateFstart[ii]-(15*60), dataSpsSPLShip$DateFstart[ii]+(15*60) ),])
  tmp1 =       dataWind[ between(dataWind$dataTime, dataSpsSPLShip$DateFstart[ii]-(15*60), dataSpsSPLShip$DateFstart[ii][ii]+(15*60) ),]
  dim(tmp1)
  if (tst2 == 1 )       { weatherInfo = rbind(weatherInfo,tmp1  )
  } else if (tst2 == 0 ){ 
    weatherInfo = rbind(weatherInfo, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  } else if (tst2 > 1 ){  
    tmp1$WSPD2[1] = mean(tmp1$WSPD2 , na.rm = T) #get mean windspeed for time period
    weatherInfo   = rbind(weatherInfo,tmp1[1,]  ) }
}
weatherInfo$Previous = shift(weatherInfo$WSPD2)
dataSpsSPLShipWspd = cbind(dataSpsSPLShip, weatherInfo)
rm(ii, tmp1, tst2)
# tm = dataSpAShWe[3330,]
hist(dataSpsSPLShipWspd$WSPD2)
dCols = data.frame(colnames(dataSpsSPLShipWspd)) 
dCols
dataSpsSPLShipWspd = dataSpsSPLShipWspd[c(1:54,56:67,74,89,87)]
dCols = data.frame(colnames(dataSpsSPLShipWspd)) 
dCols

#-----------------------------------------------------------
#SPLshipsWind with ice-- all zeros so not relevent to this application!
iceCom = NULL
dataSpsSPLShipWspd$Day = as.Date(dataSpsSPLShipWspd$DateFstart, format = "%m/%d/%Y")
for (ii in 1:nrow(dataSpsSPLShipWspd)){
  idx = match(dataSpsSPLShipWspd$Day[ii], dataIce$day2 )
  iceCom = rbind(iceCom, dataIce [idx,])}
dataSpsSPLShipWspdIce = cbind(dataSpsSPLShipWspd,iceCom)
dCols = data.frame(colnames(dataSpsSPLShipWspdIce)) 
dCols
dataSpsSPLShipWspdIce = dataSpsSPLShipWspdIce[c(1:69,71,74,75)]
dCols = data.frame(colnames(dataSpsSPLShipWspdIce)) 
dCols

#-----------------------------------------------------------
#SPLshipsWindIce with tide
tideCom = NULL
#make a new column with date on the top of the hour.... to match with tide data
dataSpsSPLShipWspdIce$Date2 = dataSpsSPLShipWspdIce$DateFstart
dataSpsSPLShipWspdIceTid = merge(dataSpsSPLShipWspdIce ,dataTide, all = FALSE, all.x = TRUE, by = "Date2" ) 

rm(ii, idx,stmp,dateSTP ,mdist, ss, nShips, tideCom, mspd, 
   info,iceCom,shipInfo,weatherInfo)

dCols = data.frame(colnames(dataSpsSPLShipWspdIceTid)) 
dCols
dataSpsSPLShipWspdIceTid = dataSpsSPLShipWspdIceTid[c(1:73,76:78,80)]
dCols = data.frame(colnames(dataSpsSPLShipWspdIceTid)) 
dCols

#-----------------------------------------------------------
#add date formats columns and save out!
dataALL     = dataSpsSPLShipWspdIceTid[c(2:77)]
dataALL$yr  = year(dataALL$DateFstart)
dataALL$mth = month(dataALL$DateFstart)
dataALL$day = day(dataALL$DateFstart)
dataALL$hr  = hour(dataALL$DateFstart)
dataALL$Date = as.Date(dataALL$DateFstart)
# add columns to indicate number of sources present in a sample (helps filter for plotting)
dataALL$Sounds
dataALL$nShips
dataALL$SoundsTotal = dataALL$Sounds + ifelse(dataALL$nShips>0, 1, 0)

save(dataALL, file = "E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataALL_BeringStraitOct2015")
# summary of data over the time period of interest
#------------------------------------------------------------------
grid.arrange(pSp,pSPL,pAIS, pTide,pIce,pWind, ncol = 3,nrow =2)

#-----------------------------------------------------------
#-----------------------------------------------------------
## BERING STRAIT PLOTS
#-----------------------------------------------------------
#-----------------------------------------------------------
load("E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataALL_BeringStraitOct2015")
dCols = data.frame(colnames(dataALL)) 
dCols

# source over the month period-- tile plot
#------------------------------------------------------------------
dataALLm = reshape2 :: melt(dataALL, id.vars = "Date", measure.vars = c("Ubi","Unk","Bal","Anth","Bmy","nShips" ))
uSource = unique(dataALLm$variable)
daySum = NULL
for (ii in 1:length(uSource)){
  dtmp = dataALLm[dataALLm$variable == uSource[ii],]
  uday = unique(dtmp$Date)
  for (dd in 1:length(uday)){
    dtmp2 = dtmp[dtmp$Date == uday[dd],]
    daySum = rbind(daySum, c((as.character( uday[dd])),  as.character(uSource[ii]), sum(as.numeric(dtmp2$value),na.rm = T), nrow(dtmp2)))
  }
  rm(dtmp,uday,dtmp2)
}
colnames(daySum) = c("Day","variable","total","samples")
daySum = as.data.frame(daySum)
daySum$Day2 = as.Date(daySum$Day)
daySum$perSample = (as.numeric(as.character(daySum$total))/as.numeric(as.character(daySum$samples)))*100
pDay = ggplot(daySum, aes(Day2, variable, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  #scale_y_discrete(labels = c("Anthropogenic (4)","Baleen whale (24)","Bowhead (157)","Beluga (88)", "Bearded seal (136)","Ribbon seal (1)",
                              #"Ice (100)","Walrus (155)","Unknown Biological(149)","Unknown (78)"))+
  labs(title = "Summary of Identified Sounds by Day at Bering Strait", fill = "% Samples") +
  xlab("") +
  ylab("Sound Source (days with calls)")
pDay

# source over the hourly summary period-- tile plot
#------------------------------------------------------------------
dataALLhr = reshape2 :: melt(dataALL, id.vars = "hr", measure.vars = c("Ubi","Unk","Bal","Anth","Bmy","nShips" ))
uSource = unique(dataALLhr$variable)
HrSum = NULL
for (ii in 1:length(uSource)){
  dtmp = dataALLhr[dataALLhr$variable == uSource[ii],]
  uhr = unique(dtmp$hr)
  for (dd in 1:length(uhr)){
    dtmp2  = dtmp[dtmp$hr == uhr[dd],]
    HrSum = rbind(HrSum, c((as.character( uhr[dd])),  as.character(uSource[ii]), sum(as.numeric(dtmp2$value),na.rm = T), nrow(dtmp2)))
  }
  rm(dtmp,dtmp2)
}
colnames(HrSum) = c("Hr","variable","total","samples")
HrSum = as.data.frame(HrSum)
HrSum$perSample = (as.numeric(as.character(HrSum$total))/as.numeric(as.character(HrSum$samples)))*100
HrSum$Hr = as.numeric(as.character(HrSum$Hr ))
pHr = ggplot(HrSum, aes(Hr, variable, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  #scale_y_discrete(labels = c("Anthropogenic (4)","Baleen whale (24)","Bowhead (157)","Beluga (88)", "Bearded seal (136)","Ribbon seal (1)",
  #"Ice (100)","Walrus (155)","Unknown Biological(149)","Unknown (78)"))+
  labs(title = "Summary of Identified Sounds by Hour at Bering Strait", fill = "% hours") +
  xlab("") +
  ylab("Sound Source (days with calls)")
grid.arrange(pDay,pHr, ncol = 2,nrow =1)

# SPL with different wind speeds, sources present
#------------------------------------------------------------------
pS= ggplot(dataALL, aes(as.numeric(as.character(WSPD)),`1000`, color = SoundsTotal)) +
  geom_point()+
  xlab("Wind Speed")+
  ylab("SPL 1000 Hz OTB")
  #theme(legend.position=c(.8, 75))
dataALLamb = dataALL[dataALL$SoundsTotal == 0,]
pA = ggplot(dataALLamb, aes(as.numeric(as.character(WSPD)),`1000`)) +
  geom_point()+
  xlab("Wind Speed")+
  ylab("SPL 1000 Hz OTB (no other sources)")
grid.arrange(pS,pA, ncol =2, nrow =1)

# Ships and bio above ambient
#------------------------------------------------------------------
colFreq = c(16,48)
quants <- c(.10,0.50,.90)
quants1 = .5

#AMBIENT-- do not have a variation in wind speed!!
dataALLamb = dataALL[dataALL$SoundsTotal == 0,] #no other sounds present!
names( dataALLamb )[colFreq[1]:colFreq[2]]  =  c(6.3, 8, 10, 12.5, 16, 20, 25, 31.5, 40, 50, 63, 80, 100, 125, 
                                                 160, 200, 250, 315, 400, 500, 630, 800, 1000, 
                                                 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000)

unique(as.Date( dataALLamb$DateFstart) )
src = "Ambient"
freq  = (names(dataALLamb[colFreq[1]:colFreq[2]]) ) 
mAmb  = apply( dataALLamb[colFreq[1]:colFreq[2]], 2 ,quantile, probs = quants1, na.rm = TRUE )
mAmb3 = apply( dataALLamb[colFreq[1]:colFreq[2]],2 ,quantile, probs = quants, na.rm = TRUE )
mAmb3 = cbind(as.data.frame(t(mAmb3)),freq)
colnames(mAmb3) = c('p10th',"p50th","p90th","freq")
pAmb = ggplot( mAmb3) +
  geom_point(aes(y=p50th, x = as.numeric( as.character(freq))) ) + 
  geom_line(aes(y=p50th,  x = as.numeric( as.character(freq))),size=1.5  )+
  geom_line(aes(y=p10th,  x = as.numeric( as.character(freq))),color = "gray")+
  geom_line(aes(y=p90th,  x = as.numeric( as.character(freq))),color = "gray")+
  scale_x_log10() +
  ylim( c(70,130)) +
  xlab("Frequency") +
  ylab(expression( paste("1/3 Octave Band SPL dB re: 1",mu,"Pa")) )+
  #annotate("text", x=10, y=128, label= "(C)", size=5) + 
  annotate("text", x=150, y=70, label= paste(src, " only samples (N = ", nrow(dataALLamb), " on ", 
                                             length(unique(dataALLamb$Date) ), " days)", sep=""), size=5 )  
pAmb
#average windspeed for ambient-- add to FIG.S2 legend
max(as.numeric(as.character(dataALLamb$WSPD)), na.rm = T)

#AIS SHIPS-- 11 samples

src = "AIS Ship" #only at windspeeds less than 10 knots
dataALLShip = dataALL[ !grepl("ubi|unk|bal|bmy|rain", dataALL$sps2) & dataALL$nShip > 0,]
dataALLShip = dataALLShip[!is.na(dataALLShip$`8`),] #remove rows with NA for acoustic values
freq = (names(dataALLShip[colFreq[1]:colFreq[2]]) ) 
mShip  = apply( dataALLShip[colFreq[1]:colFreq[2]], 2,quantile, probs = quants1, na.rm = TRUE )
mShip3 = apply( dataALLShip[colFreq[1]:colFreq[2]],2 ,quantile, probs = quants, na.rm = TRUE )
mShip3 = cbind(as.data.frame(t(mShip3)),freq)
colnames(mShip3) = c('p10th',"p50th","p90th","freq")
mean(as.numeric( as.character(dataALLShip$WSPD2) ),na.rm =TRUE)
max(as.numeric( as.character(dataALLShip$Diff) ),na.rm =TRUE)

pShip = ggplot( mShip3) +
  geom_point(aes(y=p50th, x = as.numeric( as.character(freq))) ) + 
  geom_line(aes(y=p50th,  x = as.numeric( as.character(freq))),size=1.5  )+
  geom_line(aes(y=p10th,  x = as.numeric( as.character(freq))),color = "gray")+
  geom_line(aes(y=p90th,  x = as.numeric( as.character(freq))),color = "gray")+
  scale_x_log10() +
  ylim( c(70,130)) +
  xlab("Frequency") +
  ylab(expression( paste("1/3 Octave Band SPL dB re: 1",mu,"Pa")) )+
  #annotate("text", x=10, y=128, label= "(C)", size=5) + 
  annotate("text", x=150, y=70, label= paste(src, " only samples (N = ", nrow(dataALLShip), " on ", 
                                             length(unique(dataALLShip$Date) ), " days)", sep=""), size=5 )  
#just ships heard-- FIG.S2
dataALLShipC = dataALL[ !grepl("ubi|unk|bal|bmy|rain", dataALL$sps2) ,]
dataALLShipC = dataALLShipC[dataALLShipC$Anth > 0, ] 
# dataALL[dataALL$Anth > 0, ] # three days with ships heard!
# uAIS = dataALL[dataALL$nShips > 0, ] unique( uAIS$Date )# three days with ships heard!

freq    = (names(dataALLShipC[colFreq[1]:colFreq[2]]) ) 
mShipC  = apply( dataALLShipC[colFreq[1]:colFreq[2]], 2,quantile, probs = quants1, na.rm = TRUE )

mShipC3 = cbind(as.data.frame(t(mShip3)),freq)
colnames(mShip3) = c('p10th',"p50th","p90th","freq")
mean(as.numeric( as.character(dataALLShip$WSPD2) ),na.rm =TRUE)
max(as.numeric(  as.character(dataALLShip$Diff) ),na.rm =TRUE)

#COMPARISON of median spectra for source of interest
mSOIs = reshape2::melt( rbind(mShipC, mAmb))
mSOIs$Var2 = as.numeric( as.character( mSOIs$Var2))
pMed = ggplot(mSOIs, aes(x=Var2, y = value, color = Var1))+
  geom_line(size = 1)+
  geom_point(size = 1)+
  scale_x_log10() +
  scale_color_manual(labels = c("Ships heard", "Ambient") , values = c("#F8766D", "black") ) +
  labs(color = "Sources present")+
  #annotate("text", x=10, y=128, label= "(B)", size=5) + 
  xlab("Frequency") + ylab(expression( paste("Median SPL (Leq) dB re: 1",mu,"Pa"))) +
  theme(legend.position = c(0.8, 0.2))+
  #theme_minimal()
  ylim(c(70,100))+
  theme_minimal()

pMed
grid.arrange(pAmb, pMed,pShip,nrow=1,ncol=3)

#CHECK WINDSPEED LESS THAN 10 Knts: 
mean(as.numeric( as.character(dataALLamb$WSPD2) ),na.rm =TRUE)
mean(as.numeric( as.character(dataALLamb$Diff) ) ,na.rm =TRUE)

mean(as.numeric( as.character(dataALLShip$WSPD2) ),na.rm =TRUE)
mean(as.numeric( as.character(dataALLShip$Diff) ) ,na.rm =TRUE)

#difference from ambient 
