# Code to explore WCS-COA ship traffic data
rm(list=ls())

#-----------------------------------------------------------
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(dplyr)
#-----------------------------------------------------------
#some mapping options:
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#"ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world )  +
  geom_sf()

#-----------------------------------------------------------
## IMPORT AIS DATA
data = read.csv("D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\AIS_Sat_Harris\\tbl_AIS_data.csv")
#data[1,1:10]
#head(data)

#-----------------------------------------------------------
##DATA CLEAN UP
dataAIS=as.data.frame(data)
dataAIS$Latitude = as.numeric(dataAIS$Latitude )
dataAIS$Longitude = as.numeric(dataAIS$Longitude )
uships = unique(dataAIS$MMSI)
dataAIS$year = substr( dataAIS$Time,1,4)
dataAIS$month = substr( dataAIS$Time,5,6)
dataAIS$day = substr( dataAIS$Time,7,8)
dataAIS$hour = substr( dataAIS$Time,10,11)
dataAIS$min = substr( dataAIS$Time,12,13)
dataAIS$sec = substr( dataAIS$Time,14,15)
uyrs = unique( dataAIS$year )
cat("there are ", as.character(length(uships)), " unique ships from ", min(dataAIS$year), "to", max(dataAIS$year) )

#-----------------------------------------------------------
##PLOT ALL DATA
dataShip = dataAIS[dataAIS$MMSI == uships[1],]
hist(as.numeric( dataShip$month) )
dataAISt = dataAIS[(!is.na(dataAIS$Latitude)), ]
g1 <- ggplot() + 
  geom_point(data = dataAISt, aes(x = Longitude, y = Latitude, 
             color = year),  size = .1 )
g1
# just 2015-2015 data
dataOI = dataAISt[ dataAISt$year == "2014" | dataAISt$year == "2015", ]
g1 <- ggplot() + 
  geom_point(data = dataOI, aes(x = Longitude, y = Latitude, 
                                  color = year),  size = .1 )
g1

rm(dataAIS, data, dataOI)

#-----------------------------------------------------------
## SUMMARY OF unique ships per year (saved to DateCompare.xlsx table)
out = NULL
uyrs = as.numeric(uyrs)
for (yy in 1:length(uyrs)){
  tmp = dataAISt[ dataAISt$year == uyrs[yy],]
  out = rbind( out, c(uyrs[yy],length(unique(tmp$MMSI)) ) )
  cat(uyrs[yy], ": unique ships- ", 
      length(unique(tmp$MMSI)), ": unique months- ",unique(tmp$month),"\n" )  
}
 
#-----------------------------------------------------------
## PLOT for report- data from November 2016
#example: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
dataOI = dataAISt[ dataAISt$year == "2015", ]
unique( dataOI$month )
dataOI = dataOI[ dataOI$month == "11", ]
#colnames(dataOI)
#max(dataOI$Latitude)
#min(dataOI$Latitude)
#max(dataOI$Longitude)
#min(dataOI$Longitude)

#focus area with ships
ships <- st_as_sf(data.frame( longitude = (dataOI$Longitude), latitude = (dataOI$Latitude ) ), 
                  coords = c("longitude", "latitude"), crs = 4326, 
                  agr = "constant")
sites <- st_as_sf(data.frame( latitude = c(65.69976,63.8178), longitude = c(-168.38855,-171.6915) ), 
                  coords = c("longitude", "latitude"), crs = 4326, 
                  agr = "constant")# Bering/Gambell

# larger map for context
(ggulf1 <- ggplot(data = world) +
    geom_sf(aes(fill = region_wb)) +
    geom_sf(data = sites, size = 1, shape = 23, fill = "darkred") +
    geom_sf(data = ships, size = .1, shape = 1, fill = "black") +
    coord_sf(crs = st_crs(3467), 
             xlim = c(-1800000, 800000), 
             ylim = c(240000, 2500000), 
             expand = FALSE, datum = NA) +
    scale_fill_viridis_d(option = "E") +
    theme(legend.position = "none", axis.title.x = element_blank(), 
          axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
          panel.border = element_rect(fill = NA)))
# focus area
(ggulf2 <- ggplot(data = world) +
    geom_sf(aes(fill = region_wb)) +
    geom_sf(data = sites, size = 1, shape = 23, fill = "darkred") +
    geom_sf(data = ships, size = .1, shape = 1, fill = "grey") +
    coord_sf(crs = st_crs(3467), 
             xlim = c(-1500000, 900000), 
             ylim = c(100000, 2200000), 
             expand = FALSE, datum = NA) +
    scale_fill_viridis_d(option = "E") +
    theme(legend.position = "none", axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          panel.background = element_rect(fill = "azure"), 
          panel.border = element_rect(fill = NA) )
  )

#-----------------------------------------------------------
## CHECK, number of ships near St. Lawerance Island/Gambell site
#calculate distance to gambell site-- for each point to each site
#copy outDist results to dataCompare.xlsx 
dataAISt$gambLat = 63.81783
dataAISt$gamLon  =  -171.69151
dataAISt$berbLat = 65.70179
dataAISt$berLon  =  -168.42581
for(ii in 1:nrow(dataAISt) ){
  dataAISt$dist2Gam[ii] = distm(c(dataAISt$Longitude[ii],dataAISt$Latitude[ii]) , c(dataAISt$gamLon[ii],dataAISt$gambLat[ii]) )
  dataAISt$dist2Ber[ii] = distm(c(dataAISt$Longitude[ii],dataAISt$Latitude[ii]) , c(dataAISt$berLon[ii],dataAISt$berbLat[ii]) )
}

yr = c(2015,2016)
mth = c(1,2,3,4,5,6,7,8,9,10,11,12)
outDist = NULL
for (yy in 1: length (yr)){
  datatmp= dataAISt[ dataAISt$year == yr[yy] , ]
  
  for (mm in 1:length (mth)){
    data1 = datatmp[ datatmp$month == mth[mm], ]
    if (nrow(data1) > 0 ) #put in catch for data1 = 0
    {
      for(ii in 1:nrow(data1) ){
        data1$dist2Gam[ii] = distm(c(data1$Longitude[ii],data1$Latitude[ii]) , c(data1$gamLon[ii],data1$gambLat[ii]) )
        data1$dist2Ber[ii] = distm(c(data1$Longitude[ii],data1$Latitude[ii]) , c(data1$berLon[ii],data1$berbLat[ii]) )
      }
      data1Gam = data1[data1$dist2Gam <= 50000,]
      numshipsGam = length( unique(data1Gam$MMSI))
      data1Ber = data1[data1$dist2Ber <= 50000,]
      numshipsBer = length( unique(data1Ber$MMSI))
      #output a table of the results
      outDist = rbind( outDist, c(yr[yy], mth[mm],numshipsGam, numshipsBer))
      rm(data1, data1Ber, data1Gam)
    }else {
      outDist = rbind( outDist, c(yr[yy], mth[mm],0, 0))
      rm(data1)
    }
    
  }#end of month
} #end of year

colnames(outDist) = c("year","month","gamships","beringShips")

#-----------------------------------------------------------
## EXPORT csv file of just data of interest
datatmp = dataAISt[ dataAISt$year == 2015 , ] # unique(datatmp$year)
datatmp = datatmp[ as.numeric(as.character(datatmp$month)) >= 7 , ]     # unique(datatmp$month)
datatmp2 = dataAISt[ dataAISt$year == 2016 , ] # unique(datatmp$year)
datatmp2 = datatmp2[ as.numeric(as.character(datatmp2$month)) <= 6 , ]     # unique(datatmp$month)
dataOut = rbind(datatmp,datatmp2)
save(dataOut,file = "D:\\RESEARCH\\COA_Projects\\2020_COA_WCS\\data\\AIS_Sat_Harris\\AISsMod.RData")

#-----------------------------------------------------------
# tracks to routes, not really working!!!
uships = unique(dataOI$MMSI)
ii = 1
for(ii in length(uships)){
  ship = dataOI[dataOI$MMSI == uships[ii],]
  route[ii] <- c(geom_path(aes(Longitude, Latitude, group = SOG), 
                           colour="#0F3B5F", size = 0.2, data= ship, alpha = 0.5, lineend = "round"))
}
ggplot(data = world) + route[5]

# need to get route in same coordinate system before the code below will work!
sites <- st_as_sf(data.frame( latitude = c(65.69976,63.8178), longitude = c(-168.38855,-171.6915) ), 
                  coords = c("longitude", "latitude"), crs = 4326, 
                  agr = "constant")

(ggulf2 <- ggplot(data = world) +
    geom_sf(aes(fill = region_wb)) +
    geom_sf(data = sites, size = 1, shape = 23, fill = "darkred") +
    route +
    coord_sf(crs = st_crs(3467), xlim = c(-1500000, 900000), ylim = c(100000, 1900000), 
             expand = FALSE, datum = NA) +
    scale_fill_viridis_d(option = "E") +
    theme(legend.position = "none", axis.title.x = element_blank(), 
          axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
          panel.border = element_rect(fill = NA)))
