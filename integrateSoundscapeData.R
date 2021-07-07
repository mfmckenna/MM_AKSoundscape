# Code to integrate WCS-COA soundscape data
# species detection, sound levels, AIS, wind, tide

rm(list=ls())

#-------------------------------------
#INFORMATION ON THE INPUT DATA SOURCES
#-------------------------------------
#1) Acoustic data
#2015.2
#- sample plan: 10 minutes on 50 minutes off, rotating sample 0,10,20,30,40,50 minutes into hour
#- sample rate: 20 kHz
#- start/end dates: 21 June 2015 - 14 October 2015
#- timezone: local AK time
# 2015.11
#- sample plan: 5 minutes on 55 minutes off, always at the top of the hour
#- sample rate: ??
#- start/end dates: 15 October 2015 - 01 July 2016
#- timezone: local AK time

# 1a) Species ID tables (see above description of acoustic data)
#Manual identification of all sounds present in the sample

# 1b) Sound pressure levels (see above description of acoustic data)
#LZeq values were calculated over the entire sample, either 5 minutes or 10 minutes in a given hour.

# 2) AIS data
#- row for each ship location in the area- not sequential time
#- time zone is likely GMT- but not confirmed!!

# 3) Wind data- https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
#- when reported: unique( dataWind$min ), mostly 16,36,56- but some other random minuts
#- 3 measure per hour
#- The times listed at Gambell Airport, AK and for all LCD stations are recorded in Local Standard Time (LST), UTC-10:00


#-------------------------------------
#QUESTIONS because data are not linging up!
#-------------------------------------
# - What was the duration of the hourly recordings? What the data says is not matching with reported values in Emily's paper, Table 2.
# 2015.2:  sampled 10 minutes every hour, with rotating start time in the hour (0,1020,30,40,50)
# 2015.11: sampled 5 minutes every hour, always at the top of the hour (0 minute)
# 
# - What are the correct time stamps for samples in deployment 2015.2? For this deployment, the hourly timestamps for species ID do not line up with the hourly timestamps for the ambient SPLs. The ambient data only has 5 values per day with pretty random start times, instead of 24 per day with rotating hourly start times (0,10,20,30,40,50) in the species ID.
# 
# - What is the sample rate for 2015.11? In Emily's paper it says 48kHz (Table 2) and in Brandon's paper (Table 1) it says 20kHz. If they are different for the deployments, how would this impact the data products- for example Emily, did you mark species in the higher frequencies for this deployment?   

#trouble shooting the data- COMPARING HOURLY SAMPLES- what is the start time of the samples?
##deployment 2015.2, 2015-07-07
#ex2015p2sp = dataSp[8720:8751,] #2015-07-07
#dataSp$start_datetime[8720:8751]
##(not top of hour- rotates from 00,10,20,30,40,50 AND 8 minute samples)
# dataSPL2 = arrange(dataSPL, dataTime)
#dataSPL2$dataTime[51:60] #2015-07-07
##(at the top of the hour, so always 00)

#deployment 2015.11, 2015-11-28
#ex2015p11 = dataSp[13002:13028,] #2015-11-28
#dataSp$start_datetime[13002:13028]
# (always at the top of the hour and 4 minute samples)
#dataSPL$dataTime[2432:2455] #2015-11-28
#(at the top of the hour, so always 00)

#-----------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(reshape)
library(ggplot2)
library(chron)
library(data.table)

#-----------------------------------------------------------
## IMPORT AND FORMAT DATA-- time period of interest and only Gambell
#-----------------------------------------------------------

#-----------------------------------------------------------
#AIS sat (data were compiled using readAIS_HarrisSat.R)- this are raw ship locations, not summarized by any time
#-----------------------------------------------------------
load("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\AIS_Sat_Harris\\AISsMod.RData")
dataAISs = dataOut
rm(dataOut)
#remove ships > 50 km from Gambell acoustic monitoring station
dataAISs = dataAISs[dataAISs$dist2Gam < 50000,]
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
# read in wind data from NCEI
#-----------------------------------------------------------
## https://gis.ncdc.noaa.gov/maps/ncei/lcd; https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# Speed of the wind at the time of observation given in miles per hour (mph)
# Amount of precipitation in inches to hundredths over the past hour. For certain automated
#stations, precipitation will be reported at sub-hourly intervals (e.g. every 15 or 20 minutes) as an accumulated
#amount of all precipitation within the preceding hour. A "T" indicates a trace amount of precipitation
dir = "D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\"
fil = "2224848_WindData_Gambel2015-2016.csv"

dataWind = as.data.frame( read.csv(paste0(dir,fil)) )
dataWind = Filter(function(x)!all(is.na(x)), dataWind)
#hist(as.numeric( as.character(dataWind$HourlyWindSpeed)) )

#hist(as.numeric( as.character(dataWind$HourlyPrecipitation)) )

#formate the date-- LOCAL TIME, but formatted at UTC
dataWind$year  = substr( dataWind$DATE,1,4)
dataWind$month = substr( dataWind$DATE,6,7)
dataWind$day   = substr( dataWind$DATE,9,10)
dataWind$hour  = substr( dataWind$DATE,12,13)
dataWind$min   = substr( dataWind$DATE,15,16)
dataWind$sec   = substr( dataWind$DATE,18,19)
dataWind$DateF = ymd_hms( paste(dataWind$year,dataWind$month,dataWind$day,dataWind$hour, dataWind$min, dataWind$sec,sep=" "  ))
#check time intervals
#max(dataWind$DateF )
dataWind$Previous = shift(dataWind$DateF) 
#hist(as.double(dataWind$Previous - dataWind$DateF, units = "mins"))

rm(fil, dir)

#why no data for April-May 2016??
#tmp = dataWind[dataWind$year >2015,]
#uMonth = unique( tmp$month)

#tmpJune =  tmp[tmp$month == "06",]
# tmpMay  =  tmp[tmp$month == "04",]
#-----------------------------------------------------------
#AMBIENT DATA
#-----------------------------------------------------------
#only reading in data from Gambell (2015.2 and 2015.11) and the LZeq values (not MSL)
dirs   = list.dirs("C:\\Users\\mckenna6\\Dropbox\\WCS_COA AK ship noise project")
dirSPL = dirs[ grepl("AMBIENT NOISE DATA", dirs) ]

#2015.11, second deployment, top of the hour
SPL_201511 = list.files(dirSPL,pattern = "2015.11.xlsx" ,full.names = TRUE)
dSPL11 = read_xlsx(SPL_201511[1],col_names = F)
dSPL11 = as.data.frame( t(dSPL11) )
colnames(dSPL11) = as.character(dSPL11[1,])
dSPL11 = dSPL11[2:nrow(dSPL11),]
colnames(dSPL11)[35:38] = c( "clipped","Num","dBA","Day1Nigh2")
colnames(dSPL11)[1]  = "Date"
dSPL11$Date = force_tz(convertToDateTime(dSPL11$Date,origin="1900-01-01"), tzone = "GMT")

#2015.2, first deployment, these are still monthly files, so need to combine when reading in the data
SPL_201502 = list.files(dirSPL, pattern = "LZeq_1258" ,full.names = TRUE)
dSPL02 = NULL
for (ii in 1:length(SPL_201502)){
  #data are in two separate worksheets... sheet 1 = day, sheet 2 = night
  tmp  = as.data.frame ((  read_xlsx(SPL_201502[ii], col_names = F, sheet = "Sheet1") ) ) 
  tmp2 = as.data.frame ((  read_xlsx(SPL_201502[ii], col_names = F, sheet = "Sheet2") ) ) 
  
  #note last three columns are 10,50,90- will filter these out in next step
  tmp  = as.data.frame( t(tmp) )
  tmp2 = as.data.frame( t(tmp2) )
  colnames(tmp)  = as.character(tmp[1,])
  colnames(tmp2) = as.character(tmp2[1,])
  tmp  = tmp[2:nrow(tmp),]
  tmp2 = tmp2[2:nrow(tmp2),]
  
  #monthly data has extra FQ bands and not 2222,3333, remove 12500 and 16000 Hz
  tmp  = cbind ( tmp[,1:34],tmp[,37:38] )
  tmp2 = cbind ( tmp2[,1:34],tmp2[,37:38] )
  
  tmp$dBA = NA
  tmp2$dBA = NA
  tmp$Day1Nigh2 = NA
  tmp2$Day1Nigh2 = NA
  
  colnames(tmp)[35:38] =  c( "clipped","Num","dBA","Day1Nigh2")
  colnames(tmp2)[35:38] = c( "clipped","Num","dBA","Day1Nigh2")
  
  colnames(tmp)[1]   = "Date"
  colnames(tmp2)[1]  = "Date"
  
  tmp$Date  = force_tz(convertToDateTime(tmp$Date,origin="1900-01-01"), tzone = "GMT")
  tmp2$Date = force_tz(convertToDateTime(tmp2$Date,origin="1900-01-01"), tzone = "GMT")
  rownames( tmp )= NULL
  rownames( tmp2 )= NULL
  
  #comine day/night data
  tmpA = rbind(tmp,tmp2)
  tmpA = tmpA[order(tmpA$Date),] #double checked the data stamp and dB values with origional excel sheet
  
  #combine with previous months
  dSPL02 = rbind(dSPL02, (tmpA))
  rm(tmp)
}
dSPL02 = dSPL02[order(dSPL02$Date),] 

#BIND delploymenst togethers
dataSPL =  rbind(dSPL02,dSPL11)
names(dataSPL) = names( dSPL02 )
rownames( dataSPL )= NULL
rm(dSPL11,dSPL02,ii,SPL_201502,SPL_201511, tmp2,tmpA)

#Some checks: hist(dataSPL$`6.3`)
# (sum( is.na(dataSPL$clipped) ))/(nrow(dataSPL)) #18% are unknown
# (sum( (dataSPL$clipped == 1) ,na.rm = T) ) /(nrow(dataSPL)) #0.2% of date are clipped

#FORMAT date time
dataSPL$yr  = year(dataSPL$Date)
dataSPL$mth = month(dataSPL$Date)
dataSPL$day = day(dataSPL$Date)
dataSPL$hr  = hour(dataSPL$Date)
dataSPL$min = minute(dataSPL$Date)
dataSPL$sec = second(dataSPL$Date) #a bit too exact!

dataSPL = dataSPL[dataSPL$yr > 2000,] #this gets rid of the 10,50,90 columns in the 2015.2 deployment data
dataSPL = Filter(function(x)!all(is.na(x)), dataSPL) #remove any rows with all NANs- should not be any because data cleaned up before this step
  
#-----------------------------------------------------------
#SPECIES DETECTIONS
#-----------------------------------------------------------
dirSp  = dirs[ grepl("GAMBELL SITE/ANIMAL DETECTION DATA", dirs) ]
dataSp = read.csv ( list.files(dirSp,pattern="location.csv",full.names = T)[1] )

#DATE FORMAT- separate out and format date
dataSp$start_datetime = as.character(dataSp$start_datetime)
dataSp$end_datetime   = as.character(dataSp$end_datetime)
dateTmp = NULL
for(ii in 1:nrow(dataSp)) {
  tmpB = unlist( strsplit(dataSp$start_datetime[ii], "T") )
  tmpE = unlist( strsplit(dataSp$end_datetime[ii], "T") )
  
  dateTmp = rbind(dateTmp,c(tmpB,tmpE))
  rm(tmpB,tmpE)
}
colnames(dateTmp) = c("dateStart","timeStart","dateEnd","timeEnd")
dateTmp= as.data.frame(dateTmp)
#add details on the time stamps for each selection
dateTmp$dateStartF = ymd(dateTmp$dateStart)
dateTmp$timeStartF = hms(dateTmp$timeStart)
dateTmp$dateEndF   = ymd(dateTmp$dateEnd)
dateTmp$timeEndF   = hms(dateTmp$timeEnd)
dateTmp$yrS        = year(dateTmp$dateStartF)
dateTmp$mthS       = month(dateTmp$dateStartF)
dateTmp$dayS       = day(dateTmp$dateStartF)
dateTmp$yrE        = year(dateTmp$dateEndF)
dateTmp$mtE        = month(dateTmp$dateEndF)
dateTmp$dayE       = day(dateTmp$dateEndF)
dateTmp$startDateTime = ymd_hms(paste(dateTmp$dateStart ,dateTmp$timeStart , sep = " ")  ) 
dateTmp$minS          = minute(dateTmp$startDateTime)
dateTmp$endDateTime   = ymd_hms(paste(dateTmp$dateEnd ,dateTmp$timeEnd , sep = " ")  )
dateTmp$minE          = minute(dateTmp$endDateTime)
dateTmp$durSec = difftime( dateTmp$endDateTime , dateTmp$startDateTime) #difference in time, in seconds
dataSp = cbind(dataSp,dateTmp)
rm(dateTmp,ii)
#filter for only date of interest
datatmp  = dataSp[ dataSp$yrS == 2015 , ] # unique(datatmp$year)
datatmp  = datatmp[ as.numeric(as.character(datatmp$mthS)) >= 7 , ]     # only July-December in 2015
datatmp2 = dataSp[ dataSp$yrS  == 2016 , ] # unique(datatmp$year)
datatmp2 = datatmp2[ as.numeric(as.character(datatmp2$mthS)) <= 6 , ]   # only January to June in 2016
dataSpt  = rbind(datatmp,datatmp2)
rm(datatmp,datatmp2,dataSp)


#CLEAN up labels!!
dataSpt$sps2 = tolower(dataSpt$sps)
dataSpt$sps2 = trimws(dataSpt$sps2)
allSounds <- unique(trimws(unlist(strsplit(as.character(dataSpt$sps2), ","))))
allSounds

dataSpt$sps2= gsub("eba dle","eba, dle", dataSpt$sps2)
dataSpt$sps2= gsub("ice/mbo","ice, mbo", dataSpt$sps2)
dataSpt$sps2= gsub("mbo or ice","ice, mbo", dataSpt$sps2)
dataSpt$sps2= gsub("dle. bmy","dle, bmy", dataSpt$sps2)
dataSpt$sps2= gsub("oro. bmy","oro, bmy", dataSpt$sps2)
dataSpt$sps2= gsub("oor","oro", dataSpt$sps2)
dataSpt$sps2= gsub("icwe","ice", dataSpt$sps2)
dataSpt$sps2= gsub("mno","mbo", dataSpt$sps2)
dataSpt$sps2= gsub("ubi or feedback?","unk", dataSpt$sps2)
dataSpt$sps2= gsub("ubi or unk?","unk", dataSpt$sps2)
dataSpt$sps2= gsub("mbo or ubi?","unk", dataSpt$sps2)
allSounds <- unique(trimws(unlist(strsplit(as.character(dataSpt$sps2), ","))))
allSounds
#??? unknown labels: ubi, mbo, uba, erb, bac... confirmed with Emily

#HOW many sounds present in a sample- split by comma and count
dataSpt$Sounds = sapply(strsplit(dataSpt$sps2,","),FUN=function(x){length(x[x!="Null"])}) # samples with 0 sounds, ambient samples
plot(dataSpt$startDateTime,dataSpt$Sounds)

#LABEL samples based on sounds present (ambient, bio only, phys only, antho only, unuseable, combo)
dataSpt$Feedback = grepl("feed",dataSpt$sps2) #un-useable samples
dataSpt$Unk  = grepl("unk",dataSpt$sps2) #unknown source  
dataSpt$Ice  = grepl("ic",dataSpt$sps2)  #ice
dataSpt$Anth = grepl("anth",dataSpt$sps2) #anthropogenic
#biological sounds
dataSpt$Bmy  = grepl("bmy",dataSpt$sps2)#bowhead
dataSpt$Dle  = grepl("dle",dataSpt$sps2)#beluga
dataSpt$Oro  = grepl("oro",dataSpt$sps2)#walrus
dataSpt$Eba  = grepl("eba|erb",dataSpt$sps2)#bearded seal, also Erb
dataSpt$Hfa  = grepl("hfa",dataSpt$sps2)#ribbon seal
dataSpt$Mbo  = grepl("mbo",dataSpt$sps2)#humpback
dataSpt$Bac  = grepl("bac",dataSpt$sps2)#minke whale
dataSpt$Uba  = grepl("uba",dataSpt$sps2)#unknown baleen
dataSpt$Bal  = grepl("uba|mbo|bac",dataSpt$sps2)# other baleen whale
dataSpt$Ubi  = grepl("ubi",dataSpt$sps2)#unknown biological
# codes: feed|unk|ic|bmy|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi
#samples un-useable- remove from matrix
dataSptc = dataSpt[dataSpt$Feedback == FALSE,] #only 4!

##****TROUBLESHOOT***** repeated values for some of the samples--- FIXED
# check: nrow(dataSptc) - length(unique(dataSptc$recording_id)) #970 are repeated
# unique(dataSptc$recording_id)
dupSP = dataSptc[duplicated(dataSptc$recording_id) | duplicated(dataSptc$recording_id, fromLast=TRUE),]
dupRecords = unique(dupSP$recording_id)
dupFix = NULL
for (ii in 1:length(dupRecords)){
  tmp = dataSptc[dataSptc$recording_id == dupRecords[ii],]
  
  #check to see if the same start and end time same, compare durations-- 
  #report out all the checks to help build the "fix"
  #combine columns of interest- species column
  if (nrow(tmp) == 2){
    ck =  identical( tmp$durSec[1],  tmp$durSec[2]) 
    #cat( ii, ":", ck,":", nrow(tmp), ":", dupRecords[ii], ":", paste(tmp$sps2[1], tmp$sps2[2],sep = ","), "\n") 
    tmp2 = unique( c(tmp$sps2[1], tmp$sps2[2] )) 
    tmp2 = tmp2[tmp2 != ""]
    tmp$Sounds[1] = length(tmp2)
    tmp3 = toString(tmp2)
    if (length(tmp2) < 1){ tmp$sps2[1] =  "" }else{ (tmp$sps2[1] =  tmp3) }
    dupFix = rbind(dupFix,tmp[1,])
  }
  if (nrow(tmp) == 3){
    ck =  identical( tmp$durSec[1],  tmp$durSec[2] ,tmp$durSec[3]) 
    #cat( ck,":", nrow(tmp), ":", dupRecords[ii], ":", paste(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3],sep = ","), "\n")
    tmp2 = unique( c(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3] )) 
    tmp2 = tmp2[tmp2 != ""]
    tmp$Sounds[1] = length(tmp2)
    tmp3 = toString(tmp2)
    if (length(tmp2) < 1){ tmp$sps2[1] =  "" }else{ (tmp$sps2[1] =  tmp3) }
    dupFix = rbind(dupFix,tmp[1,])
  }
  if (nrow(tmp) == 4){
    ck =  identical( tmp$durSec[1],  tmp$durSec[2] ,tmp$durSec[3], tmp$durSec[4]) 
    #cat( ck,":", nrow(tmp), ":", dupRecords[ii], ":", paste(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4],sep = ","), "\n")
    tmp2 = unique( c(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4] )) 
    tmp2 = tmp2[tmp2 != ""]
    tmp$Sounds[1] = length(tmp2)
    tmp3 = toString(tmp2)
    if (length(tmp2) < 1){ tmp$sps2[1] =  "" }else{ (tmp$sps2[1] =  tmp3) }
    dupFix = rbind(dupFix,tmp[1,])
  }
  if (nrow(tmp) == 5){
    ck =  identical( tmp$durSec[1],  tmp$durSec[2] ,tmp$durSec[3],tmp$durSec[4],tmp$durSec[5]) 
    #cat( ck,":", nrow(tmp), ":", dupRecords[ii], ":", paste(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4], tmp$sps2[5],sep = ","), "\n")
    tmp2 = unique( c(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4], tmp$sps2[5] )) 
    tmp2 = tmp2[tmp2 != ""]
    tmp$Sounds[1] = length(tmp2)
    tmp3 = toString(tmp2)
    if (length(tmp2) < 1){ tmp$sps2[1] =  "" }else{ (tmp$sps2[1] =  tmp3) }
    dupFix = rbind(dupFix,tmp[1,])
  }
  if (nrow(tmp) == 6){
    ck =  identical( tmp$durSec[1],  tmp$durSec[2] ,tmp$durSec[3],tmp$durSec[4],tmp$durSec[5],tmp$durSec[6])
    #cat( ck,":", nrow(tmp), ":", dupRecords[ii], ":", paste(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4], tmp$sps2[5], tmp$sps2[6],sep = ","), "\n")
    tmp2 = unique( c(tmp$sps2[1], tmp$sps2[2], tmp$sps2[3], tmp$sps2[4], tmp$sps2[5],tmp$sps2[6] )) 
    tmp2 = tmp2[tmp2 != ""]
    tmp$Sounds[1] = length(tmp2)
    tmp3 = toString(tmp2)
    if (length(tmp2) < 1){ tmp$sps2[1] =  "" }else{ (tmp$sps2[1] =  tmp3) }
    dupFix = rbind(dupFix,tmp[1,])
  }
  rm(tmp, tmp2)
}
#rebuild the species ID dataframe: remove any records that were duplicates and replace with above matrix
#remove all records that have a dublicate (length(dupRecords))
dataSptd = dataSptc[!dataSptc$recording_id %in% dupRecords, ]
#ck: nrow(dataSpt) - length(dupRecords)*2 (reasonable remove of records, because some had more than 2 enteries)
dataSptd = rbind(dataSptd, dupFix)
dataSptd = dataSptd[order(dataSptd$startDateTime),]
#head(dataSptd)
#redo species labels with grepl (see code above)
dataSptd$Feedback = grepl("feed",dataSptd$sps2) #un-useable samples
dataSptd$Unk  = grepl("unk",dataSptd$sps2) #unknown source  
dataSptd$Ice  = grepl("ic",dataSptd$sps2)  #ice
dataSptd$Anth = grepl("anth",dataSptd$sps2) #anthropogenic
#biological sounds
dataSptd$Bmy  = grepl("bmy",dataSptd$sps2)#bowhead
dataSptd$Dle  = grepl("dle",dataSptd$sps2)#beluga
dataSptd$Oro  = grepl("oro",dataSptd$sps2)#walrus
dataSptd$Eba  = grepl("eba|erb",dataSptd$sps2)#bearded seal, also Erb
dataSptd$Hfa  = grepl("hfa",dataSptd$sps2)#ribbon seal
dataSptd$Mbo  = grepl("mbo",dataSptd$sps2)#humpback
dataSptd$Bac  = grepl("bac",dataSptd$sps2)#minke whale
dataSptd$Uba  = grepl("uba",dataSptd$sps2)#unknown baleen
dataSptd$Bal  = grepl("uba|mbo|bac",dataSptd$sps2)# other baleen whale
dataSptd$Ubi  = grepl("ubi",dataSptd$sps2)#unknown biological

rm(ck,dupRecords,tmp3,dupFix,dupSP,ii, dataSpt,dataSptc)

#summary of JUST biological samples (not ambient, antho, feedback, unk, ice)
#dataSpt$BioA = all(dataSpt$Bmy, dataSpt$Dle, dataSpt$Oro, dataSpt$Eba,dataSpt$Hfa ) #all biological sources of interest present
dataSptd$BioOnly  = !grepl("feed|unk|ic|anth",dataSptd$sps2) & dataSptd$Sounds>0
bioSamples = sum(dataSptd$BioOnly, na.rm = TRUE)/nrow(dataSptd)  #44% wow!
ambSamples = sum(dataSptd$Sound==0, na.rm = TRUE)/nrow(dataSptd) #46% wow!
iceSamples = sum(dataSptd$Ice,na.rm = TRUE) #might have other sounds present!
unuSamples = sum(dataSptd$Feedback,na.rm = TRUE)


##****TROUBLESHOOT***** duration of species id data- FOUND ROWS with short durations, but did not remove
dataSptd$durId = (dataSptd$endDateTime  - dataSptd$startDateTime)/60  
hist(as.numeric( as.character( dataSptd$durId) ), main = "Duration of species ID [min]")
ckSp = (dataSptd[ dataSptd$durId < 1 ,])
#did not do anything about it yet... only 133 selections with duration less than 5 minutes, will just buffer alignment window for these samples
# because assuming it was a mistake

##****TROUBLESHOOT***** date formate for aligning data... trying to remove error
#dataSptd$startDateTime[1]
#force_tz(dataSptd$startDateTime[1],tzone = "GMT" )
#dataSptd$endDateTime[1]
#force_tz(dataSptd$endDateTime[1],tzone = "GMT" )
dataSptd$startDateTime = force_tz(dataSptd$startDateTime,tzone = "GMT" )
dataSptd$endDateTime = force_tz(dataSptd$endDateTime,tzone = "GMT" )

#PLOT of presence of sounds in each time step- tile plot in ggplot
#-------------------------------------------------------------------
dataSPtcm = reshape :: melt(dataSptd, id.vars = "startDateTime", measure.vars = c("Bmy","Dle","Oro","Eba","Hfa","Bal","Ice","Unk","Anth","Ubi" ))
#summarize by day- count of samples with individual source present
dataSPtcm$Day = as.Date(dataSPtcm$startDateTime)
uSource = unique(dataSPtcm$variable)
daySum = NULL
for (ii in 1:length(uSource)){
  dtmp = dataSPtcm[dataSPtcm$variable == uSource[ii],]
  uday = unique(dtmp$Day)
  for (dd in 1:length(uday)){
    dtmp2 = dtmp[dtmp$Day == uday[dd],]
    daySum = rbind(daySum, c((as.character( uday[dd])),  as.character(uSource[ii]), sum(as.numeric(dtmp2$value),na.rm = T), nrow(dtmp2)))
  }
  rm(dtmp,uday,dtmp2)
}
colnames(daySum) = c("Day","variable","total","samples")
daySum = as.data.frame(daySum)
daySum$Day2 = as.Date(daySum$Day)
daySum$perSample = as.numeric(as.character(daySum$total))/as.numeric(as.character(daySum$samples))
#% of days with each source
uSource = unique(dataSPtcm$variable)
SourceCnt = NULL
for (ii in 1:length(uSource)) {
  tmp =  daySum[daySum$variable == uSource[ii],]
  tmp2 = sum(as.numeric( as.character(tmp$total ))> 0)
  SourceCnt = rbind( SourceCnt, c(as.character(uSource[ii]), tmp2, tmp2/nrow(tmp)))
  rm(tmp,tmp2)
}
colnames(SourceCnt)= c("source","samples","percentDays")
SourceCnt = as.data.frame(SourceCnt)
SourceCnt$Name = c("Bowhead","Beluga","Walrus", "Bearded seal","Ribbon seal","Baleen whale","Ice","Unknown","Anthropogenic","Unknown Biological")
SourceCnt
ggplot(daySum, aes(Day2, variable, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  scale_y_discrete(labels = c("Anthropogenic (4)","Baleen whale (24)","Bowhead (157)","Beluga (88)", "Bearded seal (136)","Ribbon seal (1)",
                              "Ice (100)","Walrus (155)","Unknown Biological(149)","Unknown (78)"))+
  labs(title = "Summary of Identified Sounds by Day at Gambell", fill = "% Samples") +
  xlab("") +
  ylab("Sound Source (days with calls)")

rm(daySum,dataSPtcm, dataSp,
   dd, iceSamples,ii,unuSamples,uSource, ambSamples,bioSamples,ckSp)

#find examples of species calls for Audit- times to find in acoustc data
#tmp = dataSptd[ (dataSptd$Ice == TRUE) , ]
#tmp = tmp[ tmp$mthS == 1,]
#copy start time

#-----------------------------------------------------------
## COMBINE DATA SETS-- for each species selection period combine with ambient, ships, wind
#-----------------------------------------------------------

#-----------------------------------------------------------
# 1)COMBINE AMBIENT dBA and specta to species detection times
#-----------------------------------------------------------
#NOTE: some time steps do not have SPL data- maybe just use the samples with corresponding 5 min SPL??
dataSptd$dataTime = dataSptd$startDateTime
#dataSptd$dataTime[1]
dataSPL$dataTime  = dataSPL$Date
#THIS ASSUMES THAT THE DATE/TIME IS THE SAME-- seems to work now that all 
tst = merge(dataSptd, dataSPL, all = FALSE, all.x = TRUE, by = "dataTime" )
tst = Filter(function(x)!all(is.na(x)), tst) # no rows with all NA... this error was fixed  :)
dataSpA = tst
rm(tst) 

#CHECKS
#durations- same as before
tmp = dataSpA[ as.numeric( dataSpA$durId )< 4,]

#-----------------------------------------------------------
# 2) COMBINE SHIPS... ships near by for each species detection time period
#-----------------------------------------------------------
#this loop takes awhile.... not sure how to speed up!
#ignore the time warning- GMT and UTC are the same, and timezones were fixed when reading in the data
shipInfo = NULL 
for (ii in 1:nrow(dataSpA)){ # ii = 690 nrow(dataSpA)
  tst2 = ( nrow( dataAISs[ between(dataAISs$dataTime, dataSpA$dataTime[ii], dataSpA$endDateTime[ii] ),]) )
  tmp1 =         dataAISs[ between(dataAISs$dataTime, dataSpA$dataTime[ii], dataSpA$endDateTime[ii] ),]
  
  #cat("Ships present in: ", as.character(dataSpA$dataTime[ii]), " in row ", ii, "(points: ", tst2, "/ships: ", ")\n" )
  if (tst2 > 0 ){ 
    nShips = length(unique(tmp1$MMSI))
    #tStamp = min( tmp1$dataTime )
    #some time steps have more than one ship present!!
    #cat("Ships present in: ", as.character(dataSptc$dataTime[ii]), " in row ", ii, "(points: ", tst2, "/ships: ", nShips, ")\n" )
    if(nShips > 1) {
      info = NULL
      for (ss in 1:length(unique(tmp1$MMSI)) ){
        stmp = tmp1[tmp1$MMSI == unique(tmp1$MMSI)[ss],]
        info = rbind(info, c(unique(stmp$MMSI), tst2, 
                             mean(stmp$dist2Gam,na.rm = T), max(stmp$dist2Gam,na.rm = T), min(stmp$dist2Gam,na.rm = T),
                             mean(as.numeric(as.character(stmp$SOG,na.rm = T))),
                             stmp$Latitude[stmp$dist2Gam  == min(stmp$dist2Gam)], 
                             stmp$Longitude[stmp$dist2Gam == min(stmp$dist2Gam)] ) )
        #sometimes this loop has awarning because values are the same... so ignores the second value ii = 690
      }
      ship = gsub('\\"',"", toString(shQuote(info[,1])))
      pts  = gsub('\\"',"", toString(shQuote(info[,2])))
      ud   = gsub('\\"',"", toString(shQuote(info[,3])))
      mxd  = gsub('\\"',"", toString(shQuote(info[,4])))
      mnd  = gsub('\\"',"", toString(shQuote(info[,5])))
      usp  = gsub('\\"',"", toString(shQuote(info[,6])))
      mlat  = gsub('\\"',"", toString(shQuote(info[,7])))
      mlon  = gsub('\\"',"", toString(shQuote(info[,8])))
      shipInfo = rbind(shipInfo, c(nShips,ship,pts,ud,mxd,mnd,usp,mlat,mlon) )
      rm(ship,pts,ud,mxd,mnd,usp,mlat,mlon)
    }else if (nShips == 1){
      #unique ships, MMMSI, points, average distance, max distance, min distance, average SOG, lat/lon of minimum distance, heading
      shipInfo = rbind(shipInfo, c( nShips, unique(tmp1$MMSI), tst2, 
                                    mean(tmp1$dist2Gam,na.rm = T),max(tmp1$dist2Gam,na.rm = T), min(tmp1$dist2Gam,na.rm = T),
                                    mean(as.numeric(as.character(tmp1$SOG,na.rm = T))),
                                    tmp1$Latitude[tmp1$dist2Gam  == min(tmp1$dist2Gam)][1], 
                                    tmp1$Longitude[tmp1$dist2Gam == min(tmp1$dist2Gam)][1]))   }
    
  } else { 
    shipInfo = rbind(shipInfo, c(0,NA,NA,NA,NA,NA,NA,NA,NA) )    }
}

shipInfo = data.frame(shipInfo)
colnames(shipInfo) = c("nShips","MMSI", "nLocations","uDist","mxDist","mnDist","uSOG","CPAlat","CPAlon")

#HOW MANY time steps with ships... some checks to make sure data are alining correctly
shipInfo$nShips = as.numeric( as.character(shipInfo$nShips) )
nrow(shipInfo[shipInfo$nShips > 0,]) #185 days
nrow(shipInfo[shipInfo$nShips > 1,]) #6 days
tmpS = (shipInfo[shipInfo$nShips > 1,]) #used this to check if times matched, row 688

# APPEND SHIPs to species data
dataSpASh = cbind(dataSpA,shipInfo)
# CREATE column with ship presence
dataSpASh$Ship = as.numeric( dataSpASh$nShips )
#unique((dataSptcs$Ship))
dataSpASh$Ship[is.na(dataSpASh$Ship)] = 0
# ADD TO TOTAL SOURCES PRESENT column (unique(dataSptcs$Sounds))
dataSpASh$Sounds2 = dataSpASh$Sounds +  dataSpASh$Ship 
#(unique(dataSptcs$Sounds2)) #number does not change because ship not present when 5 sources!!
# SUMMARY of ships
#sum( !is.na(dataSpASh$nShips))                    # times with ships present = 212
dtmp = dataSpASh$uDist[(!is.na(dataSpASh$uDist) )] # CPA distances
distSum = NULL
for (dd in 1:length(dtmp)){ # dd = 146
  tmp = sapply( strsplit(as.character(dtmp[dd]),",") ,"[",2)
  if ( is.na(tmp) ){
    t1 = as.numeric (as.character( sapply( strsplit(as.character(dtmp[dd]),","),"[",1)))
    distSum = rbind( distSum , t1 )
  }else {
    t1 = as.numeric (as.character( sapply( strsplit(as.character(dtmp[dd]),","),"[",1)))
    t2 = as.numeric (as.character( sapply( strsplit(as.character(dtmp[dd]),","),"[",2)))
    distSum = rbind( distSum , t1,t1 )
  }
}
hist(distSum/1000,main = paste("CPA distances (N = " , as.character(length(distSum)), " samples)"),xlab = "distance (km)")

rm(ii, ss, t1, t2, tmp,tst2, dtmp, tmp1,stmp, info, distSum, dd, nShips)

#-----------------------------------------------------------
#3) COMBINE WIND WITH Species, SPL, and AIS...
#-----------------------------------------------------------
dataWind$dataTime = force_tz( dataWind$DateF, tzone="GMT")
dataWind$HourlyWindSpeed = as.numeric( as.character(dataWind$HourlyWindSpeed)) #blanks become NAs- ignore warning
# hist(dataWind$HourlyWindSpeed)
#NOTE: this loop takes awhile.... not sure how to speed up!
# found an issue with the second deployment species ID- start:end :00 to :04, and wind data are recorded :16,:36,:56
# want to use the :16 data point because an average of the beginning of the hour, so added time to the search window +- 15 minutes
weatherInfo = NULL
for (ii in 1:nrow(dataSpASh)){ # ii = 4160  ii = 3337 new deployment on 10-14-2015, dataSpASh$dataTime[3331:3350] tmp = dataWind[18960:19035,] 
  
  if (dataSpASh$deployment_id[ii] == "2015.2"){
    tst2 = nrow( dataWind[ between(dataWind$dataTime, dataSpASh$dataTime[ii]-(15*60), dataSpASh$endDateTime[ii]+(15*60) ),])
    tmp1 = dataWind[ between(      dataWind$dataTime, dataSpASh$dataTime[ii]-(15*60), dataSpASh$endDateTime[ii]+(15*60) ),]
    
    if (tst2 == 1 )       { weatherInfo = rbind(weatherInfo,tmp1  )
    } else if (tst2 == 0 ){ weatherInfo = rbind(weatherInfo, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    } else if (tst2 > 1 ){  
      #cat("row " , ii, " has multiple wind data", "\n")
      tmp1$HourlyWindSpeed[1] = mean(tmp1$HourlyWindSpeed , na.rm = T)
      weatherInfo = rbind(weatherInfo,tmp1[1,]  ) }
    
  }else if (dataSpASh$deployment_id[ii] == "2015.11"){
    tst2 = nrow( dataWind[ between( dataWind$dataTime, dataSpASh$dataTime[ii]-(15*60), dataSpASh$endDateTime[ii]+(15*60) ),])
    tmp1 =  dataWind[ between(      dataWind$dataTime, dataSpASh$dataTime[ii]-(15*60), dataSpASh$endDateTime[ii]+(15*60) ),]
    if (tst2 == 1 )       {
      weatherInfo = rbind(weatherInfo,tmp1  )
    } else if (tst2 == 0 ){ 
      weatherInfo = rbind(weatherInfo, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    } else if (tst2 > 1 ){  
      #cat("row " , ii, " has multiple wind data", "\n")
      tmp1$HourlyWindSpeed[1] = mean(tmp1$HourlyWindSpeed , na.rm = T)
      weatherInfo = rbind(weatherInfo,tmp1[1,]  )  }
  }
}
dataSpAShWe = cbind(dataSpASh, weatherInfo)
rm(ii, tmp1, tst2)
# tm = dataSpAShWe[3330,]
hist(dataSpAShWe$HourlyWindSpeed)

save(dataSpAShWe, file = "D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\dataSpAShWe.RData")
#-----------------------------------------------------------
#LOTS OF COLS!!! helpful to know for plotting
#-----------------------------------------------------------

load("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\dataSpAShWe.RData")
data.frame(colnames(dataSpAShWe)) 
#Species cols[1:52]
#SPL     cols[53,90],   OCTAVE BANDS cols[54:86]
#Ship    cols[91,111],  nShips col[97]
#Wind    cols[112,141], wind speed [128]

#-----------------------------------------------------------
#PLOTs
#-----------------------------------------------------------
colFreq = c(58,90)
#-----------------------------------------------------------
#PLOT spectra with specific source present... no other sources!
#-----------------------------------------------------------
# codes: feed|unk|ic|bmy|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi
# !grepl("feed|unk|ic|anth",dataSpt$sps2) & dataSpt$Sounds>0
quants <- c(0.50)
#------change these for source of interest------#
src = "Baleen whales"
SOI = dataSpAShWe[ !grepl("feed|unk|ic|dle|oro|eba|erb|hfa|ubi",dataSpAShWe$sps2)& dataSpAShWe$Ship == 0 & dataSpAShWe$Sounds > 0 ,]
#SOI = dataSptcs[dataSptcs$Bmy > 0,]     #species of interest sounds present
#unique(SOI$sps2)
mBaleen = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )

src = "Ambient"
SOI = dataSpAShWe[dataSpAShWe$Sounds == 0 & dataSpAShWe$Ship == 0,] #no sounds present unique(tst$Sounds)
unique(SOI$sps2)
mAmbient = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )

src = "Ships"
SOI = dataSpAShWe[ !grepl("feed|unk|ic|bmy|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi",dataSpAShWe$sps2) & dataSpAShWe$Ship > 0,]
unique(SOI$sps2)
mShips = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
#need to remove clipped samples:  length(dataSptcs$clipped == 1)

#------#
SOI = SOI[!is.na(SOI$`8`),] #remove rows with NA for acoustic values
freq = (names(SOI[colFreq[1]:colFreq[2]]) )
long_SOI = reshape2::melt(SOI, id.vars = "dateStart", measure.vars = freq)

#!!!!!!!!!!!!!!!!!!!!!FIGURE(S) TO SHARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#box plot of sound levels for samples with a given source present
unique(long_SOI$dateStart)
ggplot(long_SOI, aes(x=variable, y = value))+
  geom_boxplot()+
  labs(title = paste(src, " samples (N = ", nrow(SOI), " on ", length(unique(long_SOI$dateStart)), " days)", sep="") ) +
  xlab("Frequency") +
  ylim(c(20,140))

#!!!!!!!!!!!!!!!!!!!!!FIGURE TO SHARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#comparison of median spectra for source of interest
mSOIs = reshape2::melt( rbind(mShips,mAmbient,mBaleen))
ggplot(mSOIs, aes(x=Var2, y = value, color = Var1))+
  geom_line(size = 1)+
  scale_x_log10() +
  labs(title = "Recieved median sound levels for sources of interest",color = "Sources present")+
  xlab("Frequency") + ylab("Median Sound Pressure Level (Leq)")+
  theme_minimal()
#ylim(c(20,140))

#!!!!!!!!!!!!!!!!!!!!!FIGURE TO SHARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# median spectra for different wind speeds... 
#entries have some weird formatting- need to fix so I can bin by different wind speeds
#as.numeric( gsub("s","", as.character( dataWind$HourlyWindSpeed[23878])) )
hist ( as.numeric( gsub("s","", as.character(dataSpAShWe$HourlyWindSpeed) ) ), main = "Wind Speed Distribution" )
dataSpAShWe$HourlyWindSpeed2 = as.numeric( gsub("s","", as.character(dataSpAShWe$HourlyWindSpeed) ) )
dataSpAShWe$HourlyWindSpeed2r = round(as.numeric( gsub("s","", as.character(dataSpAShWe$HourlyWindSpeed) )),digits =-1) 
ambData = dataSpAShWe[dataSpAShWe$Sounds == 0,]
#need to average by wind speed category for each frequency bin
# unique(dataSpAShWe$HourlyWindSpeed2r)... something is going on with the columns I am averaging
WS0 = ambData[ambData$HourlyWindSpeed2r == 0,]
WS0 = apply( WS0[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
WS10 = ambData[ambData$HourlyWindSpeed2r == 10,]
WS10 = apply( WS10[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
WS20 = ambData[ambData$HourlyWindSpeed2r == 20,]
WS20 = apply( WS20[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
WS30 = ambData[ambData$HourlyWindSpeed2r == 30,]
WS30 = apply( WS30[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
WS40 = ambData[ambData$HourlyWindSpeed2r == 40,]
WS40 = apply( WS40[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
WSsum = rbind(WS0,WS10,WS20,WS30,WS40)
WSsum2 = reshape :: melt(WSsum)
ggplot(WSsum2, aes(x=(X2), y=value, color = X1) )+
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_x_log10() +
  labs(title = "Comparision of SPL with different wind speeds",color = "Wind speed")+
  xlab("Frequency") + ylab("Median Sound Pressure Level (Leq)")+
  ylim(c(50,140))+
  theme_minimal()

#-----------------------------------------------------------
#PLOT median spectra for differen number of source present--- not working b/c tst gone!
#-----------------------------------------------------------
#RESULT: not about how many sources are present that driver higher sound levels
# hist( dataSpAShWe$Sounds )
S0 = dataSpAShWe[dataSpAShWe$Sounds == 0,]
S0 = apply( S0[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
S1 = dataSpAShWe[dataSpAShWe$Sounds == 1,]
S1 = apply( S1[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
S2 = dataSpAShWe[dataSpAShWe$Sounds == 2,]
S2 = apply( S2[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
S3 = dataSpAShWe[dataSpAShWe$Sounds == 3,]
S3 = apply( S3[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
S4 = dataSpAShWe[dataSpAShWe$Sounds == 4,]
S4 = apply( S4[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
S5 = dataSpAShWe[dataSpAShWe$Sounds == 5,]
S5 = apply( S5[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants , na.rm = TRUE )
Sqsum = rbind(S0,S1,S2,S3,S4,S5)
Sqsum2 = reshape :: melt(Sqsum)
Sqsum2$X22 = as.numeric(Sqsum2$X2)
#!!!!!!!!!!!!!!!!!!!!!FIGURE TO SHARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ggplot(Sqsum2, aes(x=(X2), y=value, color = X1) )+
  geom_line(size = 1) + 
  geom_point(size = 1)+
  scale_x_log10() +
  labs(title = "Comparision of SPL with different number of sources present",color = "Sources present")+
  #scale_fill_continuous(name = "Sources present", labels = c("0", "1", "2", "3","4","5"))+
  xlab("Frequency") + ylab("Median Sound Pressure Level (Leq)")+
  ylim(c(50,140))+
  theme_minimal()
#xlim(c(0,10000))

#!!!!!!!!!!!!!!!!!!!!!FIGURE TO SHARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#PLOT tile graphic of different sources present-- added ships to it
#reorganize data as (date,source,present/absent)
cols <- sapply(dataSpAShWe, is.logical)
dataSpAShWe[,cols] <- lapply(dataSpAShWe[,cols], as.numeric)
dataSpAShWem = reshape :: melt(dataSpAShWe, id.vars = "startDateTime", 
                               measure.vars = c("Bmy","Dle","Oro","Eba","Hfa","Bal","Ice","Unk","Anth","Ubi",'nShips'))

#-----------------------------------------------------------
#summarize by day- count of samples with individual source present
#-----------------------------------------------------------
dataSpAShWem$Day = as.Date(dataSpAShWem$startDateTime)
uSource = unique(dataSpAShWem$variable)
daySum = NULL
for (ii in 1:length(uSource)){
  dtmp = dataSpAShWem[dataSpAShWem$variable == uSource[ii],]
  uday = unique(dtmp$Day)
  for (dd in 1:length(uday)){
    dtmp2  = dtmp[dtmp$Day == uday[dd],]
    daySum = rbind(daySum, c( (as.character( uday[dd])), as.character(uSource[ii]), 
                              sum(as.numeric(dtmp2$value),na.rm = T), 
                              nrow(dtmp2)) )
  }
  rm(dtmp,uday,dtmp2)
}
# find how many day with source present to add to y-label on graphic
uSource = unique(dataSpAShWem$variable)
daySum = as.data.frame(daySum)
SourceCnt = NULL
for (ii in 1:length(uSource)) {
  tmp =  daySum[daySum$variable == uSource[ii],]
  tmp2 = sum(as.numeric( as.character(tmp$total ))> 0)
  SourceCnt = rbind( SourceCnt, c(as.character(uSource[ii]), tmp2, tmp2/nrow(tmp)))
  rm(tmp,tmp2)
}
#NOTE fewer days on the ship spectra plot because those are only clean samples!!!
daySum$Day2 = as.Date(daySum$Day)
daySum$perSample = as.numeric(as.character(daySum$total))/as.numeric(as.character(daySum$samples))
ggplot(daySum, aes(Day2, variable, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  scale_y_discrete(labels = c("Anthropogenic (4)", "Baleen whale (24)", "Bowhead (157)", "Beluga (88)", "Bearded seal (136)","Ribbon seal (1)",
                              "Ice (100)", "AIS ships (73)", "Walrus (155)", "Unknown Biological (149)", "Unknown (78)")) +
  labs(title = "Summary of Sounds by Day at Gambell", fill = "% Samples") +
  xlab("") +
  ylab("Sound Source")

#-----------------------------------------------------------
#plot of daily wind speed and dBA to add to bottom of above graphic
#-----------------------------------------------------------
#wind--- mean for the day
uday = unique(dataSpAShWe$dateStart)
daySumWind = NULL
for (dd in 1:length(uday)){ # dd = 108
  dtmp2  = dataSpAShWe[dataSpAShWe$dateStart == uday[dd],]
  daySumWind = rbind(daySumWind, c( as.character(uday[dd]), mean(as.numeric(dtmp2$HourlyWindSpeed2), na.rm = T),
                                    mean(as.numeric(dtmp2$HourlyWindSpeed2r),na.rm = T),        nrow(dtmp2)) )
}
colnames(daySumWind) = c("Day","mean","meanR", "samples")
daySumWind = as.data.frame(daySumWind)
daySumWind$Day2 = as.Date(daySumWind$Day)
ggplot(daySumWind, aes(Day2, as.numeric(as.character(mean))) ) +
  geom_point()+
  geom_linerange(aes(x=Day2, ymax=as.numeric(as.character(mean)), ymin=0),
                 position = position_jitter(height = 0L, seed = 1L)) +
  xlab("") +
  ylab("Daily Average Wind Speed")+
  theme_minimal()

#Ambient noise-- median 100 Hz SPL
uday = unique(dataSpAShWe$dateStart)
daySumDBA = NULL
for (dd in 1:length(uday)){
  dtmp2  = dataSpAShWe[dataSpAShWe$dateStart == uday[dd],]
  daySumDBA = rbind(daySumDBA, c( as.character(uday[dd]), median(as.numeric(dtmp2$`200`),na.rm = T),
                                  nrow(dtmp2)) )
}
colnames(daySumDBA) = c("Day","mean", "samples")
daySumDBA = as.data.frame(daySumDBA)
daySumDBA$Day2 = as.Date(daySumDBA$Day)
ggplot(daySumDBA, aes(Day2, as.numeric(as.character(mean))) ) +
  geom_point()+
  geom_linerange(aes(x=Day2, ymax=as.numeric(as.character(mean)), ymin=0),
                 position = position_jitter(height = 0L, seed = 1L)) +
  xlab("") +
  ylab("Median daily SPL [dB @ 100 Hz]")+
  theme_minimal()


#ships-- number of ships in area
daySumShips = NULL
for (dd in 1:length(uday)){
  dtmp2  = dataSpAShWe[dataSpAShWe$dateStart == uday[dd],]
  daySumShips = rbind(daySumShips, c( as.character(uday[dd]), sum(as.numeric(dtmp2$nShips),na.rm = T),
                                      nrow(dtmp2)) )
}
colnames(daySumShips) = c("Day","mean", "samples")
daySumShips = as.data.frame(daySumShips)
daySumShips$Day2 = as.Date(daySumShips$Day)
ggplot(daySumShips, aes(Day2, as.numeric(as.character(mean))) ) +
  geom_point()+
  geom_linerange(aes(x=Day2, ymax=as.numeric(as.character(mean)), ymin=0),
                 position = position_jitter(height = 0L, seed = 1L)) +
  xlab("") +
  ylab("Hours with ships <30 km away")+
  theme_minimal()

data.frame(colnames(dataSpAShWe)) 
colnames(dataSpAShWe)[70] = "Fq_100Hz"
save(dataSpAShWe, file = "D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataSpAShWe")
#-----------------------------------------------------------
#predictive model for 100Hz
#-----------------------------------------------------------

rm(list=ls())
load("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataSpAShWe")
# predictor variables-- reformat, distribution, values
dataSpAShWe$mthS   = as.numeric(as.character(dataSpAShWe$mthS ))
hist(dataSpAShWe$mthS )
dataSpAShWe$Sounds = as.numeric(as.character(dataSpAShWe$Sounds  ))
hist(dataSpAShWe$Sounds)
dataSpAShWe$nShips = as.numeric(as.character(dataSpAShWe$nShips ))
hist(dataSpAShWe$nShips ) 
unique( dataSpAShWe$nShips )
dataSpAShWe$nShips[is.na(dataSpAShWe$nShips)] <- 0

dataSpAShWe$Bmy    = as.numeric(as.character( dataSpAShWe$Bmy ))
hist(dataSpAShWe$Bmy )
unique(dataSpAShWe$Bmy )
dataSpAShWe$Eba    = as.numeric(as.character(dataSpAShWe$Eba ))
hist(dataSpAShWe$Eba )
unique(dataSpAShWe$Eba  )
hist( dataSpAShWe$HourlyWindSpeed2 )
unique(dataSpAShWe$HourlyWindSpeed2)

# model-- gamm
library(mgcv)
ctrl = list(nthreads=6)
global.GammT = gamm(Fq_100Hz ~ s(mthS)+ (Sounds)+ (Eba)+ (nShips)+ s(HourlyWindSpeed2), 
                    data=dataSpAShWe, method="REML", na.action=na.omit)
summary(global.GammT$gam)
plot(global.GammT$gam)

#still driven/predicted by wind, not biological or human activity


