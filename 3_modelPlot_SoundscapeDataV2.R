# Uses output of integrateSoundsscapeData.R to create plots and models for AK soundscape project
rm(list=ls())

#general
library(dplyr)
library(data.table)
library(ggplot2)
#for model-- start line 354
library(suncalc)
library(mgcv)
library(zoo)
library(MuMIn)
library(visreg)
library(corrplot)
#for map
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)


#IMPORT data for Gambell site for entire year 2015-2016
#--------------------------------------------------------------------------------
#includes ambient levels, presence of sounds, AIS ships, and wind data
load("G:\\My Drive\\ActiveProjects\\MM_AKsoundscape\\data\\dataSpAShWeTiIce")
#load("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataSpAShWe")
#load("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\dataSpAShWeIce")
dataSpAShWeIce = dataSpAShWeTiIce

#AIS ship ranges is 50 km too big?
#--------------------------------------------------------------------------------
as.data.frame( colnames(dataSpAShWeIce) )
plot( dataSpAShWeIce$nShips )

OneShip = dataSpAShWeIce[dataSpAShWeIce$nShips == 1,]
#203 samples
TwoShip = dataSpAShWeIce[dataSpAShWeIce$nShips > 1,]
#9 samples
#super low number of samples with AIS ships present
ZeroShip = dataSpAShWeIce[dataSpAShWeIce$nShips == 0,]
#8906 samples
# Not important because so few samples... not to say that on those days important?

#range of distances
dist1 = (as.numeric(as.character(OneShip$mnDist)))/1000
hist(dist1)
dist2=NULL
for ( ii in 1:nrow(TwoShip) ) {
  tp1 = as.numeric( sapply(strsplit(as.character( TwoShip$mxDist[ii]),","), `[`, 1) )
  tp2 = as.numeric( sapply(strsplit(as.character( TwoShip$mxDist[ii]),","), `[`, 2) )
  dist2 = rbind(dist2,tp1,tp2)
}
distA = rbind(as.matrix(dist1),dist2/1000)
hist(distA)
sum( distA>20 )
sum( distA<20 )

#re-code nship as 0 if both distances are >20 km


#PLOT MAP OF REGION-- Figure 1
#--------------------------------------------------------------------------------
#world map (map with ships see rdAIS_HarrisSat.r)
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")
class(world)
# ggplot(data = world )  +
#   geom_sf() +
#   geom_rect(xmin = -170, xmax = -166, ymin = 64, ymax = 66, 
#             fill = NA, colour = "black", size = 1.5)

AKprog = 3467 # c(-4596612.39 -2250856.49) , c( 2,024,122.30 4,364,571.46)
WGS84proj = 4326
sites <- st_as_sf(data.frame( latitude = c(65.69976,63.8178), longitude = c(-168.38855,-171.6915) ), 
                  coords = c("longitude", "latitude"), crs = WGS84proj, 
                  agr = "constant")# Bering/Gambell

# AK map for context with bounding box (add labels in illustrator)
ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  geom_sf(data = sites, size = 3, shape = 23, fill = "darkred") +
  coord_sf(crs = st_crs(AKprog), 
           xlim = c(-1800000, 800000), 
           ylim = c(240000, 2500000), expand = FALSE, datum = NA) +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA)) 


##label in illustrator, scale bar not working
#annotate(geom = "text", x = 64, y = -151, label = "Gulf of Mexico", 
#fontface = "italic", color = "grey22", size = 6) +
#scalebar(sites, dist = 100,  dist_unit = "km", transform = TRUE, model = "WGS84", location="bottomright") +

#PLOT Sound sources-- Figure 2 (see integreateSoundscapeData. R to generate this figure)
#--------------------------------------------------------------------------------
cols <- sapply(dataSpAShWeIce, is.logical)
dataSpAShWeIce[,cols] <- lapply(dataSpAShWeIce[,cols], as.numeric)
dataSpAShWeIceem = reshape :: melt(dataSpAShWeIce, id.vars = "startDateTime", 
                                   measure.vars = c("Bmy","Dle","Oro","Eba","Hfa","Bal","Ice","Unk","Anth","Ubi",'nShips'))
dataSpAShWeIceem$Day = as.Date(dataSpAShWeIceem$startDateTime)

uSource = unique(dataSpAShWeIceem$variable)
daySum = NULL
for (ii in 1:length(uSource)){
  dtmp = dataSpAShWeIceem[dataSpAShWeIceem$variable == uSource[ii],]
  #all days with source of interest
  uday = unique(dtmp$Day)
  for (dd in 1:length(uday)){ #for each day total up the samples with source and total samples
    dtmp2  = dtmp[dtmp$Day == uday[dd],]
    daySum = rbind(daySum, c( (as.character( uday[dd])), as.character(uSource[ii]), 
                              sum(as.numeric(dtmp2$value), na.rm = T), 
                              nrow(dtmp2)) )
  }
  
  rm(dtmp,uday,dtmp2)
}

# find how many day with source present to add to y-label on graphic
uSource = unique(dataSpAShWeIceem$variable)
daySum = as.data.frame(daySum)
SourceCnt = NULL
for (ii in 1:length(uSource)) {
  tmp =  daySum[daySum$V2 == uSource[ii],]
  tmp2 = sum(as.numeric( as.character(tmp$V3 ))> 0)
  SourceCnt = rbind( SourceCnt, c(as.character(uSource[ii]), tmp2, tmp2/nrow(tmp)))
  rm(tmp,tmp2)
}

#tile plot-- all sound occurence
colnames(daySum) = c("Day","variable","samples","total")
daySum$Day2 = as.Date(daySum$Day)
daySum$perSample = as.numeric(as.character(daySum$samples))/as.numeric(as.character(daySum$total))*100
uvar = unique(daySum$variable)
uorder = c("k","h","j","i","f","g","d","a","c","e","b" ) #c(1,4,2,3,6,5,8,11,9,7,10 )
ulabs = c("Unknown (78)", "AIS ships (73)", "Anthropogenic (4)", "Ice (100)", 
          "Unknown biologic (149)","Ribbon seal (1)", "Other baleen whales (24)","Beluga (88)", "Bearded seal (136)",  "Walrus (155)", "Bowhead (157)" )
for (i in 1:length(uvar)){
  idxO = which(daySum$variable == uvar[i])
  daySum$Order[idxO] = paste0(uorder[i],uvar[i])
}

pS = ggplot(daySum, aes(Day2, Order, fill= as.numeric(perSample))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  scale_y_discrete(labels = ulabs) +
  labs(fill = "% Daily Samples") +
  xlab("") +
  ylab("") 

#tile plot-- just biological
biom = reshape :: melt(dataSpAShWeIce, id.vars = "startDateTime", 
                       measure.vars = c("Bmy","Dle","Oro","Eba","Hfa","Bal","Ubi"))
biom$Day = as.Date(biom$startDateTime)
uBSource = unique(biom$variable)
BdaySum = NULL
for (ii in 1:length(uBSource)){
  dtmp = biom[biom$variable == uBSource[ii],]
  #all days with source of interest
  uday = unique(dtmp$Day)
  for (dd in 1:length(uday)){ #for each day total up the samples with source and total samples
    dtmp2  = dtmp[dtmp$Day == uday[dd],]
    BdaySum = rbind(BdaySum, c( (as.character( uday[dd])), as.character(uBSource[ii]), 
                                sum(as.numeric(dtmp2$value), na.rm = T), 
                                nrow(dtmp2)) )
  }
  
  rm(dtmp,uday,dtmp2)
}
colnames(BdaySum) = c("Day","variable","samples","total")
BdaySum = as.data.frame(BdaySum)
BdaySum$Day2 = as.Date(BdaySum$Day)
BdaySum$perSample = as.numeric(as.character(BdaySum$samples))/as.numeric(as.character(BdaySum$total))*100
uvar = unique(BdaySum$variable)

#tile plot-- windspeed, ice coverage, tide- get daily mean for these variables
dCols = data.frame(colnames(dataSpAShWeTiIce)) 
dCols
colI = c(153,147,132)
uDays = unique(dataSpAShWeTiIce$dateStart)
abioDay = NULL
for (ii in 1:length(uDays)){
  tmp = colMeans (dataSpAShWeTiIce[ dataSpAShWeTiIce$dateStart == uDays[ii],colI] , na.rm = TRUE)
  abioDay = rbind(abioDay, c(as.character(uDays[ii]), tmp) )
}
abioDay = as.data.frame( abioDay )
colnames(abioDay) = c("day","ice","tide","wind")
abioDay$day = as.Date(abioDay$day )
abioDay$ice = as.numeric(as.character((abioDay$ice )))
abioDay$tide = as.numeric(as.character((abioDay$tide )))
abioDay$wind = as.numeric(as.character((abioDay$wind )))
abioDaym = reshape :: melt(abioDay, id.vars = "day", 
                                   measure.vars = c("wind","ice","tide"))

abioDaymWind = abioDaym[ abioDaym$variable == "wind", ]
pW = ggplot(abioDaymWind, aes(day, variable, fill= as.numeric(value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  labs(fill = "Wind [kts]") +
  xlab("") +
  ylab("") 
pWl = ggplot(abioDaymWind, aes(day, value) )+ 
  geom_line() +
  xlab("") +
  ylab("Wind speed")+
  theme(    axis.text.x = element_blank() )

abioDaymTide = abioDaym[ abioDaym$variable == "tide", ]
pT = ggplot(abioDaymTide, aes(day, value) )+ 
  geom_line() +
  xlab("") +
  ylab("tide change") +
  theme(    axis.text.x = element_blank() )

abioDaymIce = abioDaym[ abioDaym$variable == "ice", ]
pI = ggplot(abioDaymIce, aes(day, variable, fill= (as.numeric(value)*100))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  labs(fill = "Ice coverage [%]") +
  xlab("") +
  ylab("") 
pIl = ggplot(abioDaymIce, aes(day, value*100) )+ 
  geom_line() +
  xlab("") +
  ylab("Ice coverage") 


library(gridExtra)
#grid.arrange(pS, pW, pT, pI, ncol=1, nrow = 4)

# FIGURE 2-- yearly ourence with abiotic conditions
# TO FINISH: created combined graphic in illustrator because I could not line up the axes- maybe if I removed the legend
PA = grid.arrange(pWl, pT, pIl, ncol=1, nrow = 3)
pS


#PLOT Sound sources-- Figure 4 A)Windspeed B) Above ambient C) biological samples variations D)AIS ships variation 
#--------------------------------------------------------------------------------
# codes: feed|unk|ic|bmy|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi
colFreq = c(58,90)
quants <- c(.10,0.50,.90)
quants1 = .5

#------change these for source of interest------#
#BIOLOGICAL-- (C) in FIG. 4
src = "Biological" #only baleen whales "feed|unk|ice|dle|oro|eba|erb|hfa" Bowhead-- "feed|unk|ice|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi"
SOI = dataSpAShWeIce[ !grepl("feed|unk|ice|dle|oro|eba|erb|hfa",dataSpAShWeIce$sps2) & dataSpAShWeIce$Ship == 0 & dataSpAShWeIce$Sounds > 0 ,]
SOI = SOI[!is.na(SOI$`8`),] #remove rows with NA for acoustic values
names( SOI )[colFreq[1]:colFreq[2]]  =  c(6.3, 8, 10, 12.5, 16, 20, 25, 31.5, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000)
freq = (names(SOI[colFreq[1]:colFreq[2]]) ) 
mBio=          apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
mBiological  = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants  , na.rm = TRUE )
mBiological2 = cbind(as.data.frame(t(mBiological)),freq)
colnames(mBiological2) = c('p10th',"p50th","p90th","freq")
pBio = ggplot( mBiological2) +
  geom_point(aes(y=p50th, x = as.numeric( as.character(freq))),size=2 ) + 
  geom_line(aes (y=p50th, x = as.numeric( as.character(freq))),size=1.5  )+
  geom_line(aes (y=p10th, x = as.numeric( as.character(freq))),color = "gray")+
  geom_line(aes (y=p90th,  x = as.numeric( as.character(freq))),color = "gray")+
  scale_x_log10() +
  ylim( c(70,130)) +
  xlab("Frequency") +
  ylab(expression( paste("1/3 Octave Band SPL dB re: 1",mu,"Pa")) )+
  annotate("text", x=10, y=128, label= "(C)", size=5) + 
  annotate("text", x=150, y=75, label= paste(src, " only samples (N = ", nrow(SOI), " on ", length(unique(SOI$dateStart) ), " days)", sep=""), size=5 )  
pBio
rm(src,SOI)

#AIS SHIPS-- (D) in FIG. 4
src = "AIS Ship" #(D)
dsts = 10 #how close do ships need to be? in km
SOI = dataSpAShWeIce[ !grepl("feed|unk|ice|bmy|dle|oro|eba|erb|hfa|mbo|bac|uba|ubi", dataSpAShWeIce$sps2) & dataSpAShWeIce$Ship > 0,]
SOI = SOI[!is.na(SOI$`8`),] #remove rows with NA for acoustic values
#need to deal with cells with multiple ship values...
SOI = SOI[as.numeric(as.character(SOI$mnDist)) >= dsts*1000 ,]  #only ships within 10 km
SOI = SOI[as.numeric(as.character(SOI$uSOG))   >= 5 ,]          #only ships speed > 5 kts
SOI = SOI[as.numeric(as.character(SOI$HourlyWindSpeed)) <= 15 ,]#only ships in < 10 knot winds
names( SOI )[colFreq[1]:colFreq[2]]  =  c(6.3, 8, 10, 12.5, 16, 20, 25, 31.5, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000)
freq = (names(SOI[colFreq[1]:colFreq[2]]) )
mSh     = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
mShips  = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants  , na.rm = TRUE )
mShips2 = cbind(as.data.frame(t(mShips)),freq)
colnames(mShips2) = c('p10th',"p50th","p90th","freq")
pShi = ggplot( mShips2) +
  geom_point(aes(y=p50th, x = as.numeric( as.character(freq))) ) + 
  geom_line(aes(y=p50th,  x = as.numeric( as.character(freq))),size=1.5  )+
  geom_line(aes(y=p10th,  x = as.numeric( as.character(freq))),color = "gray")+
  geom_line(aes(y=p90th,  x = as.numeric( as.character(freq))),color = "gray")+
  scale_x_log10() +
  ylim( c(70,130)) +
  xlab("Frequency") +
  ylab(expression( paste("1/3 Octave Band SPL dB re: 1",mu,"Pa")) )+
  annotate("text", x=10, y=128, label= "(D)", size=5) + 
  annotate("text", x=150, y=75, label= paste(src, " only samples within ", dsts, " km (N = ", nrow(SOI), " on ", length(unique(SOI$dateStart) ), " days)", sep=""), size=5)  
pShi
rm(src,SOI)

#WIND SPEED sound levels-- (A) in FIG. 4
src = "Ambient" # unique( dataSpAShWeIce$HourlyWindSpeed  )
SOI = dataSpAShWeIce[dataSpAShWeIce$Sounds == 0 & dataSpAShWeIce$Ship == 0 & dataSpAShWeIce$HourlyWindSpeed < 10, ,] #no sounds present unique(tst$Sounds)
SOI = SOI[!is.na(SOI$`8`),] #remove rows with NA for acoustic values
mAmbient = apply( SOI[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
dataSpAShWeIce$HourlyWindSpeed2 = as.numeric( gsub("s","", as.character(dataSpAShWeIce$HourlyWindSpeed) ) )
dataSpAShWeIce$HourlyWindSpeed2r = round(as.numeric( gsub("s","", as.character(dataSpAShWeIce$HourlyWindSpeed) )),digits =-1) 
SOI = dataSpAShWeIce[dataSpAShWeIce$Sounds == 0 & dataSpAShWeIce$Ship == 0 ,] #no sounds present unique(tst$Sounds)
ambData = SOI[!is.na(SOI$`8`),] #remove rows with NA for acoustic values
names( ambData )[colFreq[1]:colFreq[2]]  =  c(6.3, 8, 10, 12.5, 16, 20, 25, 31.5, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000)
freq = (names(ambData[colFreq[1]:colFreq[2]]) )
WS0 = ambData[ambData$HourlyWindSpeed2r == 0,]
nrow(WS0)
WS0 = apply( WS0[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
WS10 = ambData[ambData$HourlyWindSpeed2r == 10,]
nrow(WS10)
WS10 = apply( WS10[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
WS20 = ambData[ambData$HourlyWindSpeed2r == 20,]
nrow(WS20)
WS20 = apply( WS20[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
WS30 = ambData[ambData$HourlyWindSpeed2r == 30,]
nrow(WS30)
WS30 = apply( WS30[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
WS40 = ambData[ambData$HourlyWindSpeed2r == 40,]
nrow(WS40)
WS40 = apply( WS40[colFreq[1]:colFreq[2]] , 2 , quantile , probs = quants1 , na.rm = TRUE )
WSsum = rbind(WS0,WS10,WS20,WS30,WS40)
WSsum2 = reshape :: melt(WSsum)
library(scales)
pWS = ggplot(WSsum2, aes(x=(X2), y=value, color = X1) )+
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_x_log10() +
  labs(color = "Wind speed [kts]")+
  scale_color_manual(labels = c("0", "10", "20","30","40") , values = hue_pal()(5) ) +
  xlab("Frequency") + ylab(expression( paste("Median SPL (Leq) dB re: 1",mu,"Pa"))) +
  annotate("text", x=10, y=128, label= "(A)", size=5) + 
  ylim(c(70,130)) +
  theme(legend.position = c(0.8, 0.2))
pWS


#COMPARE of median spectra for source of interest (B) in FIG. 4
mSOIs = reshape2::melt( rbind(mBio, mSh, mAmbient))
mSOIs$Var2 = as.numeric( as.character( mSOIs$Var2))
pMed = ggplot(mSOIs, aes(x=Var2, y = value, color = Var1))+
  geom_line(size = 1)+
  geom_point(size = 1)+
  scale_x_log10() +
  scale_color_manual(labels = c("Biological only", "AIS ships only", "Ambient") , values = c("#00BFC4","#F8766D","black") ) +
  labs(color = "Sources present")+
  annotate("text", x=10, y=128, label= "(B)", size=5) + 
  xlab("Frequency") + ylab(expression( paste("Median SPL (Leq) dB re: 1",mu,"Pa"))) +
  theme(legend.position = c(0.8, 0.2))+
  ylim(c(70,130))
pMed

#FIG. 4 A-D
library(gridExtra)
grid.arrange(pWS, pMed, pBio, pShi, ncol=2, nrow = 2)

## RESPONSE variable-- ambient sound levels in octave bands
#--------------------------------------------------------------------------------
colnames(dataSpAShWeIce)[71] = "Fq_125Hz"
colnames(dataSpAShWeIce)[74] = "Fq_250Hz"
colnames(dataSpAShWeIce)[77] = "Fq_500Hz"
colnames(dataSpAShWeIce)[80] = "Fq_1000Hz"
colnames(dataSpAShWeIce)[83] = "Fq_2000Hz"
colnames(dataSpAShWeIce)[89] = "Fq_8000Hz"

##PREDICTOR variables-- reformat, distribution, values
#--------------------------------------------------------------------------------
par(mfrow=c( 3,3))
#1) MONTH- as indicator of season 
dataSpAShWeIce$mthS   = as.numeric(as.character(dataSpAShWeIce$mthS ))
hist(dataSpAShWeIce$mthS,main="Month" )
#2) NUMBER of sounds present
dataSpAShWeIce$Sounds = as.numeric(as.character(dataSpAShWeIce$Sounds  ))
hist(dataSpAShWeIce$Sounds,main="#Sounds")
#3) NUMBER of ships present
dataSpAShWeIce$nShips = as.numeric(as.character(dataSpAShWeIce$nShips ))
#unique( dataSpAShWeIce$nShips )
dataSpAShWeIce$nShips[is.na(dataSpAShWeIce$nShips)] <- 0
hist(dataSpAShWeIce$nShips,main="#Ships" ) 
#4) PRESENCE OF SOURCES
#4a) Bowhead
dataSpAShWeIce$Bmy    = as.numeric(as.character( dataSpAShWeIce$Bmy ))
hist(dataSpAShWeIce$Bmy,main="bowhead")
#unique(dataSpAShWeIce$Bmy )
#4b) Bearded seal
dataSpAShWeIce$Eba    = as.numeric(as.character(dataSpAShWeIce$Eba ))
#hist(dataSpAShWeIce$Eba )
#unique(dataSpAShWeIce$Eba  )
#4c Unknown biologiacl
dataSpAShWeIce$Ubi =  as.numeric(as.character(dataSpAShWeIce$Ubi ))
#5d Baleen whale
dataSpAShWeIce$Bal =  as.numeric(as.character(dataSpAShWeIce$Bal ))
#5) WINDSPEED
hist( dataSpAShWeIce$HourlyWindSpeed2 ,main = "wind")
#unique(dataSpAShWeIce$HourlyWindSpeed2)
#6) ICE coverage for the day
dataSpAShWeIce$ice_concentration_20km = as.numeric( as.character(dataSpAShWeIce$ice_concentration_20km) )
hist (dataSpAShWeIce$ice_concentration_20km,main="ice")
#7) SUN altitude- day light related to biological activity??
gambell = c(63.8227,171.6758)
tmp = getSunlightPosition(dataSpAShWeIce$dataTime,gambell[1],gambell[2])
dataSpAShWeIce$sunAlt = tmp$altitude
#8) JULIAN day
dataSpAShWeIce$Jul = as.numeric( as.character( ( yday(dataSpAShWeIce$dataTime) )))
#9) HOUR of the day
dataSpAShWeIce$hour2 = as.numeric( as.character( hour(dataSpAShWeIce$dataTime) ))
#10) TIDE
dataSpAShWeIce$WaterLevelDiff = as.numeric( as.character( (dataSpAShWeIce$WaterLevelDiff) ))
hist(dataSpAShWeIce$WaterLevelDiff )

dCols = data.frame(colnames(dataSpAShWeIce)) 
dCols

##SOME CHECKS MODEL INPUTS
#--------------------------------------------------------------------------------
par(mfrow=c(1,1))
#1) autocorrlation term- how correlated is SPL to previous hour?- very!!!
dataSpAShWeIce$Fq_125HzShift = shift(dataSpAShWeIce$Fq_125Hz,fill = NA)
plot(dataSpAShWeIce$Fq_125HzShift, dataSpAShWeIce$Fq_125Hz)
corACI = cor(dataSpAShWeIce$Fq_125HzShift,dataSpAShWeIce$Fq_125Hz, method = "pearson",use="complete.obs")
#2) predictor variable correlation-- only use month, not Julian day
newdata <- dataSpAShWeIce[c("mthS" , "Jul", "sunAlt", 
                            "ice_concentration_20km" ,"HourlyWindSpeed2", 
                            "Eba", "Ubi", "Bal", "nShips")]
corr = cor(newdata)
corrplot(corr)
# multicollinearityy: https://datascienceplus.com/multicollinearity-in-r/
# likely causing issue with Eba beging a negative predictor...

#2) NA values-- models can't deal with
idx =  apply(is.na(dataSpAShWeIce[,c("Jul", "sunAlt", "ice_concentration_20km", "HourlyWindSpeed2", "Sounds", "Eba", "Bmy", "Bal","Ubi" )]),1,any) 
remRows = nrow(dataSpAShWeIce) - nrow(dataSpAShWeIce[!idx,])
remRows - length (which(is.na(dataSpAShWeIce[,("HourlyWindSpeed2")] ) ) )
nrow(dataSpAShWeIce[!idx,])/ nrow(dataSpAShWeIce) #54% of data NA so removed--yikes!!
#interoloate because all wind- missing wind  (what is the optimal gap-4 samples for an hour)
dataNA  = dataSpAShWeIce[!idx,] #matrix of the missing wind data...
intrpNA = na.approx(dataSpAShWeIce$HourlyWindSpeed2,maxgap=(6),na.rm = FALSE )
# NAs still left- ugh
length(which(is.na(intrpNA )))
length(which(is.na(intrpNA )))/ nrow(dataSpAShWeIce) #38% of data NA so removed
par(mfrow=c( 2,1))
plot(intrpNA, pch = 20, col="blue", cex=.5, main = "Wind speed with interpolation max gap = 6")
plot(dataSpAShWeIce$HourlyWindSpeed2,pch = 20,col="blue", cex=.5, main = "Wind speed")
dataSpAShWeIce$HourlyWindSpeed2int = intrpNA
#remove rows with NAs-- mostly from wind (ugh!)
dataModel = dataSpAShWeIce[!is.na(intrpNA),]
#check other variables for NAs...
unique(dataModel$hour2)
ck = dataModel[is.na(dataModel$hour2),]
unique(dataModel$Jul)
unique(dataModel$sunAlt)
unique(dataModel$ice_concentration_20km)
unique(dataModel$HourlyWindSpeed2int)
unique(dataModel$Eba)
unique(dataModel$Bmy)
unique(dataModel$Bal)
unique(dataModel$Ubi)
unique(dataModel$WaterLevelDiff)
idxWL = which(!is.na(dataModel$WaterLevelDiff))
dataModel = dataModel[idxWL,]
dataModel = dataModel[dataModel$WaterLevelDiff < 1.5 ,]

#HYPOTHESIS: enviroment still driven/predicted by wind and biological, not human activity
# add back in when variables set: correlation=corCAR1(value=corACI,form=~dataTime)
ctrl = list(nthreads=6)
rm(newdata,idx,remRows,tmp,idxWL)

##MODEL-- gam
#--------------------------------------------------------------------------------
#smooth term selection using select=TRUE, which penalizes wiggliness and removes terms with poor fit from the model
#We also fit all models with gamma=1.4, which further restricts wiggliness
#smooth terms: https://www.rdocumentation.org/packages/mgcv/versions/1.8-33/topics/smooth.terms
#get variable names
dCols = data.frame(colnames(dataModel)) 
dCols

#-------------------------
##1) 125 Hz octave band model
#-------------------------
#a. GLOBAL model-- all possible variables
options(na.action = "na.omit")
global.Gamm125 = gam(Fq_125Hz ~ s(Jul,bs = "cr") + 
                       s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") + 
                       s(WaterLevelDiff) +
                       s(sunAlt,bs = "cr") + (nShips) +
                       (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa),
                     correlation=corCAR1(value=corACI,form=~dataTime),
                     data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)

#model evaluation
summary(global.Gamm125)
#anova(global.Gamm125)#anova provides whole term significance for parametric terms
#par(mfrow=c( 3,4))
#visreg(global.Gamm125) #all variables
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm125,"pearson")^2)/df.residual(global.Gamm125)
#autocorrelation
#pacf(residuals(global.Gamm125)) #why does this not change when I add in corelation term to model?

#b. all significant variables- supplementary info graphics
pJulian = visreg(global.Gamm125,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (125 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=31, y=125, label="(A)") + theme_bw()
pWind  = visreg(global.Gamm125,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=3, y=125, label="(B)") + theme_bw()
pTide  = visreg(global.Gamm125,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pIce = visreg(global.Gamm125,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(C)") + theme_bw()
pSun = visreg(global.Gamm125,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125, label="(E)") + theme_bw()
pBmy = visreg(global.Gamm125,"Bmy", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bowhead") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pBal = visreg(global.Gamm125,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
pEba = visreg(global.Gamm125,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(H)") + theme_bw()
pOro = visreg(global.Gamm125,"Oro", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Walrus") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(I)") + theme_bw()
pUbi = visreg(global.Gamm125,"Ubi", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Unknown biologic") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(J)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun, pBmy, pBal, pEba, pOro, pUbi, ncol=5, nrow =2)  #figure S2.1
#removed--
#pHour = visreg(global.Gamm125,"hour2", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
#ylab( "") +  xlab( "Hour") + ylim(c(70,125))+ annotate("text", x=2, y=125, label="(D)") + theme_bw()

#best model-- use DREDGE function to rank by AIC 
# did not use b/c similar results to global model and want to evalute model for all variables
#options(na.action = "na.fail")
#global.Gamm125_dredge = dredge(global.Gamm125, rank=AIC)
#global.Gamm125_dredge 
#subset(global.Gamm125_dredge, subset=delta<2 ) #2 models
#WOCR1<- get.models(global.Gamm125_dredge, subset = 1)[[508]] #best model
#summary(WOCR1)

#c. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters: global.Gamm125$sp, so we can hold the smoothing terms fixed.
global.Gamm125$sp #check correct sp used in mode\
#-- Abiotic
mPhysic = gam(Fq_125Hz ~ s(ice_concentration_20km,bs = "cr",sp = global.Gamm125$sp[3:4] ) + 
                         s(HourlyWindSpeed2int,bs = "cr",   sp = global.Gamm125$sp[5:6] ) +
                         s(WaterLevelDiff,                  sp = global.Gamm125$sp[7:8] ),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic)
devPhysic = 1-(mPhysic$deviance/mPhysic$null.deviance)  
#-- Bioti
mBiolog = gam(Fq_125Hz ~ (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog = 1-(mBiolog$deviance/mBiolog$null.deviance)  
#-- Anthro
mAnthro = gam(Fq_125Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm125$sp[9:10] ) + (nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance)  
#-- Season
mSeason = gam(Fq_125Hz ~ s(Jul,bs = "cr",sp = global.Gamm125$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)  
# ( c("125Hz", 1-(global.Gamm125$deviance/global.Gamm125$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ) )

ModComp = NULL
ModComp = rbind(ModComp, c("125Hz", 1-(global.Gamm125$deviance/global.Gamm125$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ))
# NOTE: used this https://stats.stackexchange.com/questions/325832/gam-mgcv-aic-vs-deviance-explained instead of this...
#ddevPhysic = deviance(global.Gamm125)- deviance(mPhysic)
#ddevBiolog = deviance(global.Gamm125)- deviance(mBiolog)
#ddevAnthro = deviance(global.Gamm125)- deviance(mAnthro)

#d. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq125_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm125))
plot(dataModel$Fq_125Hz, predict(global.Gamm125), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq125_ConditionalPlots.png")
par(mfrow=c(3,4))
visreg(global.Gamm125,main="125Hz")
dev.off()

#-------------------------
##2) 250 Hz octave band
#-------------------------
#a. GLOBAL model-- all possible variables
options(na.action = "na.omit")
global.Gamm250 = gam(Fq_250Hz ~ s(Jul,bs = "cr") +
                       s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") + 
                       s(WaterLevelDiff) +
                       s(sunAlt,bs = "cr") + (nShips) +
                       (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa),
                     correlation=corCAR1(value=corACI,form=~dataTime),
                     data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
#model evaluation
summary(global.Gamm250)
# anova(global.Gamm250)#anova provides whole term significance for parametric terms
#par(mfrow=c( 3,3))
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm250,"pearson")^2)/df.residual(global.Gamm250)
#autocorrelation
#pacf(residuals(global.Gamm250)) #why does this not change when I add in corelation term to model?

#b.all significant variables- supplementary info
pJulian = visreg(global.Gamm250,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (250 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=25, y=125,  label="(A)") + theme_bw()
pWind = visreg(global.Gamm250,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2.5, y=125, label="(B)") + theme_bw()
pTide = visreg(global.Gamm250,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pIce = visreg(global.Gamm250,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125,  label="(C)") + theme_bw()
pSun = visreg(global.Gamm250,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125,label="(E)") + theme_bw()
pBal = visreg(global.Gamm250,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pEba = visreg(global.Gamm250,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
pUbi = visreg(global.Gamm250,"Ubi", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Unknown biologic") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(H)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun, pBal, pEba, pUbi, ncol=4, nrow =2)  #figure S2.2


#b. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters: global.Gamm250$sp, so we can hold the smoothing terms fixed.
global.Gamm250$sp
#--
mPhysic = gam(Fq_250Hz ~ s(ice_concentration_20km,bs = "cr",sp = global.Gamm250$sp[3:4] ) + 
                         s(HourlyWindSpeed2int,   bs = "cr",sp = global.Gamm250$sp[5:6] ) +
                         s(WaterLevelDiff,                  sp = global.Gamm250$sp[7:8] ),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic)
devPhysic = 1-(mPhysic$deviance/mPhysic$null.deviance)  
#--
mBiolog = gam(Fq_250Hz ~ (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog= 1-(mBiolog$deviance/mBiolog$null.deviance)  
#--
mAnthro = gam(Fq_250Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm250$sp[9:10]) +(nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance)  
#--
mSeason = gam(Fq_250Hz ~ s(Jul,bs = "cr",sp = global.Gamm250$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)  
#--
c("250Hz", 1-(global.Gamm250$deviance/global.Gamm250$null.deviance), devPhysic, devSeason, devBiolog, devAnthro)
ModComp = rbind(ModComp, c("250Hz", 1-(global.Gamm250$deviance/global.Gamm250$null.deviance), devPhysic, devSeason, devBiolog, devAnthro))

#c. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq250_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm250))
plot(dataModel$Fq_250Hz, predict(global.Gamm250), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq250_ConditionalPlots.png")
par(mfrow=c( 3,4))
visreg(global.Gamm250,main="250Hz")
dev.off()

#-------------------------
##3) 500 Hz octave band
#-------------------------
#a. GLOBAL model-- all possible variables
options(na.action = "na.omit")
global.Gamm500 = gam(Fq_500Hz ~ s(Jul,bs = "cr")+
                       s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") + 
                       s(WaterLevelDiff) +
                       s(sunAlt,bs = "cr") + (nShips) +
                       (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa)+ (Dle),
                     correlation=corCAR1(value=corACI,form=~dataTime),
                     data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
#model evaluation
summary(global.Gamm500) # anova(global.Gamm500)#anova provides whole term significance for parametric terms
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm500,"pearson")^2)/df.residual(global.Gamm500)
#autocorrelation
#pacf(residuals(global.Gamm500)) #why does this not change when I add in corelation term to model?
#visreg(global.Gamm500)

#b. all significant variables- supplementary info graphics
pJulian = visreg(global.Gamm500,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (500 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=31, y=125, label="(A)") + theme_bw()
pWind  = visreg(global.Gamm500,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=3, y=125, label="(B)") + theme_bw()
pTide  = visreg(global.Gamm500,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pIce = visreg(global.Gamm500,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(C)") + theme_bw()
pSun = visreg(global.Gamm500,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125, label="(E)") + theme_bw()
pBmy = visreg(global.Gamm500,"Bmy", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bowhead") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pBal = visreg(global.Gamm500,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
pEba = visreg(global.Gamm500,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(H)") + theme_bw()
pUbi = visreg(global.Gamm500,"Ubi", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Unknown biologic") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(I)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun, pBmy, pBal, pEba, pUbi, ncol=5, nrow =2) #figure S2.3


#c. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters: global.Gamm500$sp, so we can hold the smoothing terms fixed.
global.Gamm500$sp
#--
mPhysic = gam(Fq_500Hz ~ s(ice_concentration_20km, bs = "cr",sp = global.Gamm500$sp[3:4] ) + 
                s(HourlyWindSpeed2int,bs = "cr",sp = global.Gamm500$sp[5:6]) +
                s(WaterLevelDiff,               sp = global.Gamm500$sp[7:8] ), 
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic)
devPhysic = 1-(mPhysic$deviance/mPhysic$null.deviance)  
#--
mBiolog = gam(Fq_500Hz ~ (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa)+ (Dle),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog = 1-(mBiolog$deviance/mBiolog$null.deviance)  
#--
mAnthro = gam(Fq_500Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm500$sp[9:10]) +(nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance)  
#--
mSeason = gam(Fq_500Hz ~ s(Jul,bs = "cr",sp = global.Gamm500$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)  
#--
c("500Hz", 1-(global.Gamm500$deviance/global.Gamm500$null.deviance), devPhysic, devSeason, devBiolog, devAnthro )
ModComp = rbind(ModComp, c("500Hz", 1-(global.Gamm500$deviance/global.Gamm500$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ))

#c. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq500_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm500))
plot(dataModel$Fq_500Hz, predict(global.Gamm500), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq500_ConditionalPlots.png")
par(mfrow=c( 3,4))
visreg(global.Gamm500,main="500Hz")
dev.off()

#-------------------------
##4) 1000 Hz octave band
#-------------------------
#a. GLOBAL model-- all possible variables
options(na.action = "na.omit")
global.Gamm1000 = gam(Fq_1000Hz ~ s(Jul,bs = "cr") + 
                        s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") +
                        s(WaterLevelDiff) +
                        s(sunAlt,bs = "cr") + (nShips) +
                        (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa)+ (Dle),
                      correlation=corCAR1(value=corACI,form=~dataTime),
                      data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
#model evaluation
summary(global.Gamm1000) #anova(global.Gamm1000)#anova provides whole term significance for parametric terms
#par(mfrow=c( 3,3))
#visreg(global.Gamm1000)
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm1000,"pearson")^2)/df.residual(global.Gamm1000)
#autocorrelation
#pacf(residuals(global.Gamm1000)) #why does this not change when I add in corelation term to model?

#b. all significant variables- supplementary info
pJulian = visreg(global.Gamm1000,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (1000 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=25, y=125, label="(A)") + theme_bw()
pWind = visreg(global.Gamm1000,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2.5, y=125, label="(B)") + theme_bw()
pIce = visreg(global.Gamm1000,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(C)") + theme_bw()
pTide = visreg(global.Gamm1000,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pSun = visreg(global.Gamm1000,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125, label="(E)") + theme_bw()
pBal = visreg(global.Gamm1000,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pEba = visreg(global.Gamm1000,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun, pBal, pEba, ncol=4, nrow =2)  #figure S2.4

#b. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters: global.Gamm1000$sp, so we can hold the smoothing terms fixed.
global.Gamm1000$sp
#--
mPhysic = gam(Fq_1000Hz ~ s(ice_concentration_20km,bs = "cr",sp = global.Gamm1000$sp[3:4] ) + 
                s(HourlyWindSpeed2int,   bs = "cr",sp = global.Gamm1000$sp[5:6] ) +
                s(WaterLevelDiff,                  sp = global.Gamm1000$sp[7:8] ),
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic) 
#--
mBiolog = gam(Fq_1000Hz ~ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa)+ (Dle),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog = 1-(mBiolog$deviance/mBiolog$null.deviance)  
#--
mAnthro = gam(Fq_1000Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm1000$sp[9:10]) +(nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance) 
#--
mSeason = gam(Fq_1000Hz ~ s(Jul,bs = "cr",sp = global.Gamm1000$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)
#--
c("1000Hz", 1-(global.Gamm1000$deviance/global.Gamm1000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro )
ModComp = rbind(ModComp, c("1000Hz", 1-(global.Gamm1000$deviance/global.Gamm1000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ))

#c. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq1000_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm1000))
plot(dataModel$Fq_1000Hz, predict(global.Gamm1000), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq1000_ConditionalPlots.png")
par(mfrow=c( 3,4))
visreg(global.Gamm1000,main="1000Hz")
dev.off()

#-------------------------
##5) 2000 Hz octave band
#-------------------------
#a. GLOBAL model-- all possible variables
options(na.action = "na.omit")
global.Gamm2000 = gam(Fq_2000Hz ~ s(Jul,bs = "cr") + 
                        s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") +
                        s(WaterLevelDiff) +
                        s(sunAlt,bs = "cr") + (nShips) +
                        (Bal)+ (Eba) + (Ubi)+ (Hfa)+ (Dle),
                      correlation=corCAR1(value=corACI,form=~dataTime),
                      data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
#model evaluation
summary(global.Gamm2000) #anova(global.Gamm2000)#anova provides whole term significance for parametric terms
#par(mfrow=c( 3,3))
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm2000,"pearson")^2)/df.residual(global.Gamm2000)
#autocorrelation
#pacf(residuals(global.Gamm2000)) #why does this not change when I add in corelation term to model?

#b. all significant variables- supplementary info
pJulian = visreg(global.Gamm2000,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (2000 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=25, y=125,  label="(A)") + theme_bw()
pWind = visreg(global.Gamm2000,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2.5, y=125, label="(B)") + theme_bw()
pIce = visreg(global.Gamm2000,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125,  label="(C)") + theme_bw()
pTide = visreg(global.Gamm2000,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pSun = visreg(global.Gamm2000,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125,label="(E)") + theme_bw()
pBal = visreg(global.Gamm2000,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pEba = visreg(global.Gamm2000,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun, pBal, pEba, ncol=4, nrow =2)  #figure S2.5


#c. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters: global.Gamm2000$sp, so we can hold the smoothing terms fixed.
global.Gamm2000$sp
#--
mPhysic = gam(Fq_2000Hz ~ s(ice_concentration_20km,bs = "cr",sp = global.Gamm2000$sp[3:4] ) + 
                s(HourlyWindSpeed2int,   bs = "cr",sp = global.Gamm2000$sp[5:6] ) +
                s(WaterLevelDiff,                  sp = global.Gamm2000$sp[7:8] ),
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic)
devPhysic = 1-(mPhysic$deviance/mPhysic$null.deviance)  
#--
mBiolog = gam(Fq_2000Hz ~(Bal)+ (Eba) + (Ubi)+ (Hfa)+ (Dle),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog = 1-(mBiolog$deviance/mBiolog$null.deviance)  
#--
mAnthro = gam(Fq_2000Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm2000$sp[9:10]) +(nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance)  
#--
mSeason = gam(Fq_2000Hz ~ s(Jul,bs = "cr",sp = global.Gamm2000$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)  
#--
c("2000Hz", 1-(global.Gamm2000$deviance/global.Gamm2000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro )
ModComp = rbind(ModComp, c("2000Hz", 1-(global.Gamm2000$deviance/global.Gamm2000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ))

#c. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq2000_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm2000))
plot(dataModel$Fq_2000Hz, predict(global.Gamm2000), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq2000_ConditionalPlots.png")
par(mfrow=c( 3,4))
visreg(global.Gamm2000,main="2000Hz")
dev.off()

#-------------------------
##6) 8000 Hz octave band
#-------------------------
#a. GLOBAL model-- all possible variables
global.Gamm8000 = gam(Fq_8000Hz ~ s(Jul,bs = "cr") +
                        s(ice_concentration_20km,bs = "cr") + s(HourlyWindSpeed2int,bs = "cr") + 
                        s(WaterLevelDiff) +
                        s(sunAlt,bs = "cr") + (nShips) +
                        (Bal)+ (Eba) + (Ubi)+ (Dle),
                      correlation=corCAR1(value=corACI,form=~dataTime),
                      data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
#model evaluation
summary(global.Gamm8000) #anova(global.Gamm8000)#anova provides whole term significance for parametric terms
#par(mfrow=c( 3,3)) #visreg(global.Gamm8000)
#dispersion parameter a la Fox and Weisberg 2019 Companion to Applied Regression, dispersion < 1, model not overdispersed (no random variables, so not relevant??)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#sum(residuals(global.Gamm8000,"pearson")^2)/df.residual(global.Gamm8000)
#autocorrelation
#pacf(residuals(global.Gamm8000)) #why does this not change when I add in corelation term to model?

#b. all significant variables- supplementary info
pJulian = visreg(global.Gamm8000,"Jul", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (8000 Hz model)") + xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=25, y=125,  label="(A)") + theme_bw()
pWind = visreg(global.Gamm8000,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2.5, y=125, label="(B)") + theme_bw()
pIce = visreg(global.Gamm8000,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125,  label="(C)") + theme_bw()
pTide = visreg(global.Gamm8000,"WaterLevelDiff", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Tide") + ylim(c(70,125))+
  annotate("text", x=-.6, y=125, label="(D)") + theme_bw()
pSun = visreg(global.Gamm8000,"sunAlt", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Sun") + ylim(c(70,125))+
  annotate("text", x=-0.7, y=125,label="(E)") + theme_bw()
pEba = visreg(global.Gamm8000,"Eba", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Bearded seal") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(F)") + theme_bw()
pDle = visreg(global.Gamm8000,"Dle", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Beluga") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(G)") + theme_bw()
grid.arrange(pJulian, pWind, pIce, pTide, pSun,  pEba, pDle, ncol=4, nrow =2)  #figure S2.6


#c. DETERMINE how much of the variability in the dependent variable, using the global model
# explained by each term in the model https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html 
# first version, all models include seasonal component... decided to separate out these terms
#smoothing parameters:global.Gamm8000$sp, so we can hold the smoothing terms fixed.
global.Gamm8000$sp
#--
mPhysic = gam(Fq_8000Hz ~ s(ice_concentration_20km,bs = "cr",sp = global.Gamm8000$sp[3:4] ) + 
                s(HourlyWindSpeed2int,   bs = "cr",sp = global.Gamm8000$sp[5:6] ) +
                s(WaterLevelDiff,                  sp = global.Gamm8000$sp[7:8] ),
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mPhysic)
devPhysic = 1-(mPhysic$deviance/mPhysic$null.deviance)  
#--
mBiolog = gam(Fq_8000Hz ~  (Bal)+ (Ubi)+ (Dle),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mBiolog)
devBiolog = 1-(mBiolog$deviance/mBiolog$null.deviance)  
#--
mAnthro = gam(Fq_8000Hz ~ s(sunAlt,bs = "cr",sp = global.Gamm8000$sp[9:10]) +(nShips),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mAnthro)
devAnthro = 1-(mAnthro$deviance/mAnthro$null.deviance) 
#--
mSeason = gam(Fq_8000Hz ~ s(Jul,bs = "cr",sp = global.Gamm8000$sp[1:2]),  
              correlation=corCAR1(value=corACI,form=~dataTime),
              data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(mSeason)
devSeason = 1-(mSeason$deviance/mSeason$null.deviance)  
#--
c("8000Hz", 1-(global.Gamm8000$deviance/global.Gamm8000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro )
ModComp = rbind(ModComp, c("8000Hz", 1-(global.Gamm8000$deviance/global.Gamm8000$null.deviance), devPhysic, devSeason, devBiolog, devAnthro ))

#c. PLOTS to save out for each FQ band
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq8000_modelPerfo.png",
    width=600, height=350,pointsize = 12)
par(mfrow=c( 1,1))
pSPL = (predict(global.Gamm8000))
plot(dataModel$Fq_8000Hz, predict(global.Gamm8000), main = "Predicted vs Actual")
dev.off()
png(file="E:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\figures\\Fq8000_ConditionalPlots.png")
par(mfrow=c( 3,4))
visreg(global.Gamm8000,main="8000Hz")
dev.off()


# FIGURE 5-- 500 and 8000 results
p1 = visreg(global.Gamm8000,"Jul",                    gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (8000 Hz model)") +  xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=20, y=125, label="(A)") + theme_bw()
p2 = visreg(global.Gamm8000,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2, y=125, label="(B)") + theme_bw()
p3 = visreg(global.Gamm8000,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(C)") + theme_bw()
pDle = visreg(global.Gamm8000,"Dle", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Beluga") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(D)") + theme_bw()

pJulian5 = visreg(global.Gamm1000,"Jul",  gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "Predicted SPL (1000 Hz model)") +  xlab( "Julian day") + ylim(c(70,125)) + 
  annotate("text", x=20, y=125, label="(A)") + theme_bw()
pIce5 = visreg(global.Gamm1000,"ice_concentration_20km", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Ice coverage*") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(C)") + theme_bw()
pWind5 = visreg(global.Gamm1000,"HourlyWindSpeed2int"   , gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Wind speed [kts]") + ylim(c(70,125)) + 
  annotate("text", x=2, y=125, label="(B)") + theme_bw()
pBal5 = visreg(global.Gamm1000,"Bal", gg=TRUE, points=list(size=.2,alpha = .2),line=list(col="black")) + 
  ylab( "") +  xlab( "Other baleen") + ylim(c(70,125))+
  annotate("text", x=.1, y=125, label="(D)") + theme_bw()

grid.arrange(pJulian5, pWind5, pIce5, pBal5, p1,p2,p3,pDle, ncol=4, nrow = 2)



#-------------------------
##PLOT model resuts as a funciton of frequeny band to show which bands are dominated by what source category
#-------------------------
colnames(ModComp) = c("Frequency","Global","Abiotic","Seasonal","Biological","Anthropogenic")
ModComp = as.data.frame(ModComp)
ModComp$Global = as.numeric(as.character( ModComp$Global) )
ModComp$Abiotic = as.numeric(as.character( ModComp$Abiotic) )
ModComp$Seasonal = as.numeric(as.character( ModComp$Seasonal) )
ModComp$Biological = as.numeric(as.character( ModComp$Biological) )
ModComp$Anthropogenic = as.numeric(as.character( ModComp$Anthropogenic) )
means.long<-reshape2::melt(ModComp,id.vars="Frequency")
positions <- c("125Hz", "250Hz", "500Hz", "1000Hz", "2000Hz", "8000Hz")

#side-by-side-- use this!
ggplot(means.long,aes(x=Frequency,y=as.numeric(as.character(value)),fill=factor(variable)))+  
  geom_bar(stat="identity",position="dodge",colour="black")+
  scale_x_discrete(limits = positions) +
  xlab("")+ ylab("Deviance explained")+
  scale_fill_manual(name="Soundscape category models", values = c("black", "dark gray","#A3A500","#00BFC4","#F8766D"))+
  theme(legend.position = c(0.15, 0.8))
#pink, green, blue, red
c("#00BFC4","#F8766D","black")
hue_pal()(5)

#Copy to spreadsheet for mabuscript tables
#Global model results
summary(global.Gamm125)
summary(global.Gamm250)
summary(global.Gamm500)
summary(global.Gamm1000)
summary(global.Gamm2000)
summary(global.Gamm8000)

#-------------------------
# exploring other model strutures with temporal variables as "by" variable
#-------------------------
dataModel$month
global.Gamm125 = gam(Fq_125Hz ~ 
                       s(ice_concentration_20km, bs = "cr", by = (mthS)) + s(HourlyWindSpeed2int, bs = "cr", by = mthS) + 
                       s(sunAlt,bs = "cr") + (nShips) +
                       (Bmy)+ (Bal)+ (Eba) + (Oro) + (Ubi)+ (Hfa),
                     correlation=corCAR1(value=corACI,form=~dataTime),
                     data=dataModel, method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
summary(global.Gamm125)
visreg(global.Gamm125,main="8000Hz")
plot(global.Gamm125)
#REMOVED: s(Jul,bs = "cr") + s(hour2, bs="cc")+