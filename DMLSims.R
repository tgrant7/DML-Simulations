#Analysis for Des Moines Lobe Monarch Simulations

library(dplyr)
library(ggplot2)
library(mgcv)

############################### SCENARIO 1 ########################################

### read in RS output  ###########

scen1.1EZ = read.csv("CumEggsPerZone.2020.Dec.21.16_08_24.txt")
sum(scen1.1EZ$Eggs) #10,658,762
scen1.2EZ = read.csv("CumEggsPerZone.2020.Dec.21.19_58_57.txt")
sum(scen2EZ$Eggs) #
scen1.3EZ = read.csv("CumEggsPerZone.2020.Dec.22.09_48_27.txt")
sum(scen3EZ$Eggs) #
scen1.4EZ = read.csv("CumEggsPerZone.2020.Dec.22.16_06_23.txt")
sum(scen4EZ$Eggs) #

length(scen1.1EZ$run)/100 #868,618, the number of polgyons in the shapefile


#combine instances - run code below for each map
Denresults = data.frame(matrix(nrow=868618, ncol=5))
colnames(Denresults) = c("PolygonID", "HabType", "CumEggs","PolygonArea","ProbEggs")

dens = scen1.1EZ
dens = scen2EZ
dens = scen3EZ
dens = scen4EZ
nrow(dens) #should be 868,618*100=86,861,800

#loop through dens to create object with densities for each polygon - ~13 days for DML on my desktop laptop
system.time(
  for(i in 1:868618)
  {
    densi = filter(dens, ID == i) #combine all 100 instances for each polygon
    Denresults[i,1] = densi[1,3] #polygon ID number
    Denresults[i,2] = as.character(densi[1,4]) #habitat type
    Denresults[i,3] = sum(densi$Eggs) #sum across the 100 instances to get total eggs for per polygon
    Denresults[i,4] = densi[1,6] #polygon area - sometimes lat/long, depends on shapefile
    Denresults[i,5] = densi[1,5] #probEggs
    #print index
    print(i)
  }
)

#save results from 6.66 days of running code, but it didn't finish
DenresultsDML.12.29.Partial = Denresults

#load dataframes from combined data
load(file = "DMLDenresults_1.1B.RData")
#combine the top half and bottom half of this dataframe
Denresults.1.1 = DenresultsDML.12.29.Partial
Denresults.1.1[457006:868618,] = Denresults[457006:868618,]
Denresults.1.1[457005,]
Denresults.1.1[457006,]
sum(Denresults.1.1$CumEggs) #10,658,762 - matches the original RS output file

load(file = "DMLDenresults_1.2.RData")
Denresults.1.2 = Denresults
sum(Denresults.1.2$CumEggs) #10,658,972 - only sightly different from run 1

load(file = "DMLDenresults_1.3.RData")
Denresults.1.3 = Denresults
sum(Denresults.1.3$CumEggs) #10,650,676

load(file = "DMLDenresults_1.4.RData")
Denresults.1.4 = Denresults
sum(Denresults.1.4$CumEggs) #10,647,442


Denresults.1 = Denresults.1.1

for (i in 1:868618) {
  Denresults.1[i,3] = Denresults.1.1[i,3]+Denresults.1.2[i,3]+Denresults.1.3[i,3]+Denresults.1.4[i,3]
}

#add county to the dataframe
#can i use area to match on?
length(unique(Denresults.1$PolygonID))   #868618
length(unique(Denresults.1$PolygonArea)) #789330
#so can't use area to match, because not all are unique

#load attribute table from new shapefile with county for each polygon
Attr1 = read.csv("Spatial_Join_DML4_Counties3_attr_table.txt")

#just try and cbind Denresults.1 and Attr1 - hope rows are all the same
Denresults.1.B = cbind(Denresults.1,Attr1)
#subtract area from area to see if different in the two dataframes
Denresults.1.B$Diff = Denresults.1.B$PolygonArea-Denresults.1.B$Shape_Area
sum(Denresults.1.B$Diff) #very close to 0 - can't tell if a numerical thing or maybe a few polys not matched right?
NonZero = Denresults.1.B$Diff[c(Denresults.1.B$Diff!=0)]
#so, 201 out of 868,618 entries are not zero, but all of them are so small I think it must be numerical rounding error

#remove extra columns
Denresults.1.C = Denresults.1.B[,c(1:5,13)]

#check sums
sum(Denresults.1.1$CumEggs)+sum(Denresults.1.2$CumEggs)+sum(Denresults.1.3$CumEggs)+sum(Denresults.1.4$CumEggs) #42,615,852
sum(Denresults.1.C$CumEggs) #42,615,852

#calculate egg density PER HECTARE
Denresults.1.C$EggDensity = (Denresults.1.C$CumEggs/Denresults.1.C$PolygonArea)*10000



### Calculate Eggs per Landcover Type ###

#36 Landcover types because different techs did it differently
unique(Denresults.1.C$HabType)
#29 counties
unique(Denresults.1.C$COUNTY)

EggsLaid = data.frame(matrix(nrow=17, ncol=31))
colnames(EggsLaid) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.1.C$COUNTY)))
EggsLaid$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                     "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#developing code for MWROW0 and MWROW1-5
eggsi = filter(Denresults.1.C, HabType == "MWROW0" | HabType == "MWROW_0")
EggsLaid$DesMoinesLobe[1] = sum(eggsi$CumEggs)

eggsi = filter(Denresults.1.C, HabType == "MWROW1-5" | HabType == "MWROW1_5")
EggsLaid$DesMoinesLobe[2] = sum(eggsi$CumEggs)

#Kossuth
eggsi = filter(Denresults.1.C, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == "Kossuth")
EggsLaid[2,2] = sum(eggsi$CumEggs)

#loop through counties for MWROW1-5
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[2,i] = sum(eggsi$CumEggs)
}
#check sum
sum(EggsLaid[2,3:31]) #9340

#MWROW0
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[1,i] = sum(eggsi$CumEggs)
}

#MWROW5-20
eggsi = filter(Denresults.1.C, HabType == "MWROW5-20" | HabType == "MWROW5_20")
EggsLaid$DesMoinesLobe[3] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[3,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[3,3:31]) #365,818

#MWROW20-60
eggsi = filter(Denresults.1.C, HabType == "MWROW20-60" | HabType == "MWROW20_60")
EggsLaid$DesMoinesLobe[4] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[4,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[4,3:31]) #894,870

#MWROW60-100
eggsi = filter(Denresults.1.C, HabType == "MWROW60-100" | HabType == "MWROW60_100")
EggsLaid$DesMoinesLobe[5] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[5,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[5,3:31]) #3,073,934

#Grassland and Pasture
eggsi = filter(Denresults.1.C, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
EggsLaid$DesMoinesLobe[6] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[6,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[6,3:31]) #2,013,852

#Low Intensity Development
eggsi = filter(Denresults.1.C, HabType == "Developed/Open Space/Developed/Low Intensity")
EggsLaid$DesMoinesLobe[7] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[7,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[7,3:31]) #86,374

#RailroadROW
eggsi = filter(Denresults.1.C, HabType == "RailroadROW")
EggsLaid$DesMoinesLobe[8] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[8,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[8,3:31]) #363,302

#Corn
eggsi = filter(Denresults.1.C, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
EggsLaid$DesMoinesLobe[9] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[9,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[9,3:31]) #114,328

#NonGMOCorn
eggsi = filter(Denresults.1.C, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
EggsLaid$DesMoinesLobe[10] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[10,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[10,3:31]) #13,988,404

#Soybeans
eggsi = filter(Denresults.1.C, HabType == "Soybeans")
EggsLaid$DesMoinesLobe[11] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[11,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[11,3:31]) #81,470

#NonGMO Soybeans
eggsi = filter(Denresults.1.C, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
EggsLaid$DesMoinesLobe[12] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[12,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[12,3:31]) #3,231,152

#All other types of agriculture
eggsi = filter(Denresults.1.C, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                 HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
EggsLaid$DesMoinesLobe[13] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                   HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                 COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[13,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[13,3:31]) #22,658

#Forests of all kinds
eggsi = filter(Denresults.1.C, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                 HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                 HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
EggsLaid$DesMoinesLobe[14] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                   HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                   HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                 COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[14,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[14,3:31]) #31,014

#Wetlands of all kinds
eggsi = filter(Denresults.1.C, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                 HabType == "Woody Wetlands/Herbaceous Wetlands" |
                 HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
EggsLaid$DesMoinesLobe[15] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                   HabType == "Woody Wetlands/Herbaceous Wetlands" |
                   HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                 COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[15,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[15,3:31]) #222,336

#Med to High Intensity Development
eggsi = filter(Denresults.1.C, HabType == "Developed/Med Intensity/Developed/High Intensity")
EggsLaid$DesMoinesLobe[16] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                 COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[16,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[16,3:31]) #0

#Water
eggsi = filter(Denresults.1.C, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                 HabType == "Open Water/Barren" |
                 HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                 HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
EggsLaid$DesMoinesLobe[17] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1.C, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                   HabType == "Open Water/Barren" |
                   HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                   HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                 COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  EggsLaid[17,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid[17,3:31]) #0

#export csv
write.csv(EggsLaid, "EggsLaid.csv")




#calc area of landcover type by county

HabArea = data.frame(matrix(nrow=17, ncol=31))
colnames(HabArea) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.1.C$COUNTY)))
HabArea$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                     "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#MWROW0
habi = filter(Denresults.1.C, HabType == "MWROW0" | HabType == "MWROW_0")
HabArea$DesMoinesLobe[1] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[1,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[1,3:31]) #22,777.29

#MWROW1-5
habi = filter(Denresults.1.C, HabType == "MWROW1-5" | HabType == "MWROW1_5")
HabArea$DesMoinesLobe[2] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[2,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[2,3:31]) #11,808.77

#MWROW5-20
habi = filter(Denresults.1.C, HabType == "MWROW5-20" | HabType == "MWROW5_20")
HabArea$DesMoinesLobe[3] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[3,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[3,3:31]) #37,614.72

#MWROW20-60
habi = filter(Denresults.1.C, HabType == "MWROW20-60" | HabType == "MWROW20_60")
HabArea$DesMoinesLobe[4] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[4,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[4,3:31]) #36,582.02

#MWROW60-100+
habi = filter(Denresults.1.C, HabType == "MWROW60-100" | HabType == "MWROW60_100")
HabArea$DesMoinesLobe[5] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[5,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[5,3:31]) #48,240.68

#Grass/Pasture
habi = filter(Denresults.1.C, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
HabArea$DesMoinesLobe[6] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[6,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[6,3:31]) #234,511.6

#Low Int Dev
habi = filter(Denresults.1.C, HabType == "Developed/Open Space/Developed/Low Intensity")
HabArea$DesMoinesLobe[7] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[7,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[7,3:31]) #135,771.9

#RailroadROW
habi = filter(Denresults.1.C, HabType == "RailroadROW")
HabArea$DesMoinesLobe[8] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[8,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[8,3:31]) #7,982.949

#Corn
habi = filter(Denresults.1.C, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
HabArea$DesMoinesLobe[9] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[9,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[9,3:31]) #1,294,174

#NonGMO Corn
habi = filter(Denresults.1.C, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
HabArea$DesMoinesLobe[10] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[10,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[10,3:31]) #171,782.6

#Soybeans
habi = filter(Denresults.1.C, HabType == "Soybeans")
HabArea$DesMoinesLobe[11] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[11,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[11,3:31]) #930,222.9

#NonGMO Beans
habi = filter(Denresults.1.C, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
HabArea$DesMoinesLobe[12] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[12,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[12,3:31]) #34,267

#Other agriculture
habi = filter(Denresults.1.C, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
HabArea$DesMoinesLobe[13] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                  HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[13,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[13,3:31]) #26,294.1

#Forests
habi = filter(Denresults.1.C, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
HabArea$DesMoinesLobe[14] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                  HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                  HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[14,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[14,3:31]) #87,022.89

#Wetlands
habi = filter(Denresults.1.C, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                HabType == "Woody Wetlands/Herbaceous Wetlands" |
                HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
HabArea$DesMoinesLobe[15] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                  HabType == "Woody Wetlands/Herbaceous Wetlands" |
                  HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[15,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[15,3:31]) #29,858.65

#Med to High Int Dev
habi = filter(Denresults.1.C, HabType == "Developed/Med Intensity/Developed/High Intensity")
HabArea$DesMoinesLobe[16] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[16,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[16,3:31]) #19,402.57

#Water
habi = filter(Denresults.1.C, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                HabType == "Open Water/Barren" |
                HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
HabArea$DesMoinesLobe[17] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1.C, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                  HabType == "Open Water/Barren" |
                  HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                  HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                COUNTY == as.character(unique(Denresults.1.C$COUNTY))[i-2])
  HabArea[17,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea[17,3:31]) #30,647.19

#export csv
HabArea2 = cbind(HabArea$HabType,round(HabArea[,2:31], digits = 0))
colnames(HabArea2)[1] = "HabType"
#lost habitat types
write.csv(HabArea2, "HabArea2.csv") #HabArea.csv had errorrs in it, but still in the folder

#Checking GISPolyID, rm'd after examination
Attr2 = read.csv("Spatial_Join_DML4_Counties5_UTM.txt")

#check probEggs
unique(Denresults.1.C$ProbEggs)






################################### SCENARIO 2 ##################################################

#check how many instances ran
#import monarch.txt files
Instances = c()

Scen2.1M = read.csv("Monarchs.2021.Mar.09.19_40_48.txt")
Instances[1] = length(unique(Scen2.1M$run)) #97
hist = hist(Scen2.1M$run, breaks = seq(0.5,100.5,1)); hist #96
Scen2.2M = read.csv("Monarchs.2021.Mar.10.08_11_03.txt")
Instances[2] = length(unique(Scen2.2M$run)) #97
hist = hist(Scen2.2M$run, breaks = seq(0.5,100.5,1)); hist
Scen2.3M = read.csv("Monarchs.2021.Mar.10.11_58_08.txt")
Instances[3] = length(unique(Scen2.3M$run)) #26
hist = hist(Scen2.3M$run, breaks = seq(0.5,100.5,1)); hist
Scen2.4M = read.csv("Monarchs.2021.Mar.10.13_36_49.txt")
Instances[4] = length(unique(Scen2.4M$run)) #68
hist = hist(Scen2.4M$run, breaks = seq(0.5,100.5,1)); hist
Scen2.5M = read.csv("Monarchs.2021.Mar.11.10_43_18.txt")
Instances[5] = length(unique(Scen2.5M$run)) #32
hist = hist(Scen2.5M$run, breaks = seq(0.5,100.5,1)); hist
Scen2.6M = read.csv("Monarchs.2021.Mar.11.12_23_45.txt")
Instances[6] = length(unique(Scen2.6M$run)) #100
hist = hist(Scen2.6M$run, breaks = seq(0.5,100.5,1)); hist
Scen2.7M = read.csv("Monarchs.2021.Mar.11.16_34_28.txt")
Instances[7] = length(unique(Scen2.7M$run)) #40
hist = hist(Scen2.7M$run, breaks = seq(0.5,100.5,1)); hist

sum(Instances, na.rm = TRUE) #460

#so, leaving out run 3 and run 5, leaves 402, so leave out 2 instances
Instances2 = Instances[c(1:2,4,6:7)]
sum(Instances2) #402

unique(Scen2.3M$name) #always 500 for each text file, as it dang well should be



#mean lifetime eggs laid for monarch agents
#Scens 2.1, 2.2, 2.4, 2.6, and 2.7

#Scen2.1
#remove instance 72 that didn't fully run
Scen2.1M.B = filter(Scen2.1M, run != 72)
length(unique(Scen2.1M.B$run)) #96
hist = hist(Scen2.1M.B$run, breaks = seq(0.5,100.5,1)); hist #96

CEggsPerM2.1 = data.frame(matrix(nrow = 48000, ncol = 3))
colnames(CEggsPerM2.1) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.1M.B$run))
Mnames = sort(unique(Scen2.1M.B$name))
CEggsPerM2.1$Run = rep(Runs,500)
CEggsPerM2.1$Name = rep(Mnames,96)
for(i in 1:48000) {
  MonAg = filter(Scen2.1M.B, run == CEggsPerM2.1[i,1] & name == CEggsPerM2.1[i,2])
  CEggsPerM2.1[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM2.1$EggsLaid) #267.97 out of 410

CEggsPerM2.2 = data.frame(matrix(nrow = 48500, ncol = 3))
colnames(CEggsPerM2.2) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.2M$run))
Mnames = sort(unique(Scen2.2M$name))
CEggsPerM2.2$Run = rep(Runs,500)
CEggsPerM2.2$Name = rep(Mnames,97)
for(i in 1:48500) {
  MonAg = filter(Scen2.2M, run == CEggsPerM2.2[i,1] & name == CEggsPerM2.2[i,2])
  CEggsPerM2.2[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM2.2$EggsLaid) #268.48 out of 410

CEggsPerM2.4 = data.frame(matrix(nrow = 34000, ncol = 3))
colnames(CEggsPerM2.4) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.4M$run))
Mnames = sort(unique(Scen2.4M$name))
CEggsPerM2.4$Run = rep(Runs,500)
CEggsPerM2.4$Name = rep(Mnames,68)
for(i in 1:34000) {
  MonAg = filter(Scen2.4M, run == CEggsPerM2.4[i,1] & name == CEggsPerM2.4[i,2])
  CEggsPerM2.4[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM2.4$EggsLaid) #268.70 out of 410

CEggsPerM2.6 = data.frame(matrix(nrow = 50000, ncol = 3))
colnames(CEggsPerM2.6) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.6M$run))
Mnames = sort(unique(Scen2.6M$name))
CEggsPerM2.6$Run = rep(Runs,500)
CEggsPerM2.6$Name = rep(Mnames,100)
for(i in 1:50000) {
  MonAg = filter(Scen2.6M, run == CEggsPerM2.6[i,1] & name == CEggsPerM2.6[i,2])
  CEggsPerM2.6[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM2.6$EggsLaid) #269.27 out of 410

#old data with all 40 instances - overwrote CEggsperM2.7 in next code block
#CEggsPerM2.7 = data.frame(matrix(nrow = 20000, ncol = 3))
#colnames(CEggsPerM2.7) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.7M$run))
Mnames = sort(unique(Scen2.7M$name))
#CEggsPerM2.7$Run = rep(Runs,500)
#CEggsPerM2.7$Name = rep(Mnames,40)
for(i in 1:20000) {
  MonAg = filter(Scen2.7M, run == CEggsPerM2.7[i,1] & name == CEggsPerM2.7[i,2])
  CEggsPerM2.7[i,3] = sum(MonAg$EggsLaid)
}
#mean(CEggsPerM2.7$EggsLaid) #268.68 out of 410

#using only 39 instances, left out run 88 like eggs per zone analysis below
Scen2.7M.39 = filter(Scen2.7M, run != 88)
CEggsPerM2.7 = data.frame(matrix(nrow = 19500, ncol = 3))
colnames(CEggsPerM2.7) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen2.7M.39$run))
Mnames = sort(unique(Scen2.7M.39$name))
CEggsPerM2.7$Run = rep(Runs,500)
CEggsPerM2.7$Name = rep(Mnames,39)
for(i in 1:19500) {
  MonAg = filter(Scen2.7M.39, run == CEggsPerM2.7[i,1] & name == CEggsPerM2.7[i,2])
  CEggsPerM2.7[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM2.7$EggsLaid) #268.27 out of 410


#combined all monarchs from all runs - i didn't exclude 1 extra run, so its 401 runs
CEggsPerM2 = rbind(CEggsPerM2.1, CEggsPerM2.2, CEggsPerM2.4, CEggsPerM2.6, CEggsPerM2.7)
mean(CEggsPerM2$EggsLaid) #268.57 out of 410
mean(CEggsPerM2$EggsLaid)/sum(EggsToLaybyDay) #65.51
sd(CEggsPerM2$EggsLaid) #39.18
hist(CEggsPerM2$EggsLaid, xlim = c(0,400), xlab = "Lifetime Eggs Laid per Monarch Agent", main = "Eggs Laid by Monarch Agents in Scenario 2")
plot(density(CEggsPerM2$EggsLaid))
plot(density(CEggsPerM2$EggsLaid, bw=8))
sum(CEggsPerM2$EggsLaid) #53,714,320




#test code for HPC machines
library(dplyr)

scen2.3EZ = read.csv("CumEggsPerZone.2021.Mar.10.11_58_08.txt")
sum(scen2.3EZ$Eggs) 

length(scen2.3EZ$run)/26 #868,617, which is correct, lost one in the new shapefiles

#combine instances - run code below for each map
Denresults = data.frame(matrix(nrow=868617, ncol=6))
colnames(Denresults) = c("PolygonID","GISPolyID","HabType", "CumEggs","PolygonArea","ProbEggs")

dens = scen2.3EZ
nrow(dens) #should be 868,617*26=22,584,042

#loop through dens to create object with densities for each polygon - ~13 days for DML on my desktop laptop
system.time(
  for(i in 1:5)
  {
    densi = filter(dens, ID == i) #combine all instances for each polygon
    Denresults[i,1] = densi[1,3] #polygon ID number
    Denresults[i,2] = densi[1,8] #GISPolyiD from shapefile
    Denresults[i,3] = as.character(densi[1,4]) #habitat type
    Denresults[i,4] = sum(densi$Eggs) #sum across the 100 instances to get total eggs for per polygon
    Denresults[i,5] = densi[1,6] #polygon area - sometimes lat/long, depends on shapefile
    Denresults[i,6] = densi[1,5] #probEggs
    #print index
    print(i)
  }
)

save(Denresults, file="DMLDenresults_2.3.RData")

#Scen 2.1 - actually only has 96 full instances
83387232/868617
#Scen 2.2
84255849/868617

#leave out 1 instance
scen2.7EZ = read.csv("CumEggsPerZone.2021.Mar.11.16_34_28.txt")

unique(scen2.7EZ$run) 

34744680-868617 #33,876,063
scen2.7EZ.39 = scen2.7EZ[1:33876063]
#run 88 is the last one, that got left out
#eggs in run 88
Run88 = filter(scen2.7EZ, run == 88)
sum(Run88$Eggs) #134,368

#clean up workspace
rm(scen2.3EZ)
rm(DenresultsDML.12.29.Partial)
rm(dens)


######## Import Workspaces from HPC Machines #######################

load("DMLDenresults_2.1.RData")
DML2.1 = Denresults
sum(DML2.1$CumEggs) #12,873,346
load("DMLDenresults_2.2.RData")
DML2.2 = Denresults
sum(DML2.2$CumEggs) #13,021,142
load("DMLDenresults_2.4.RData")
DML2.4 = Denresults
sum(DML2.4$CumEggs) #9,125,308
load("DMLDenresults_2.6.RData")
DML2.6 = Denresults
sum(DML2.6$CumEggs) #13,402,438
load("DMLDenresults_2.7.39.RData")
DML2.7.39 = Denresults
sum(DML2.7.39$CumEggs) #5,231,338

Denresults.2 = DML2.1

for (i in 1:868617) {
  Denresults.2[i,4] = DML2.1[i,4]+DML2.2[i,4]+DML2.4[i,4]+DML2.6[i,4]+DML2.7.39[i,4]
}

sum(Denresults.2$CumEggs) #53,653,572
sum(DML2.1$CumEggs)+sum(DML2.2$CumEggs)+sum(DML2.4$CumEggs)+sum(DML2.6$CumEggs)+sum(DML2.7.39$CumEggs) #53,653,572

#load attribute table from new shapefile with county for each polygon
Attr2 = read.csv("Spatial_Join_DML4_Counties5_UTMS2_attr_table.txt")

#just try and cbind Denresults.1 and Attr1 - hope rows are all the same
Denresults.2.B = cbind(Denresults.2,Attr2)
#subtract area from area to see if different in the two dataframes
Denresults.2.B$Diff = Denresults.2.B$PolygonArea-Denresults.2.B$Shape_Area
sum(Denresults.2.B$Diff) #very close to 0 - can't tell if a numerical thing or maybe a few polys not matched right?
Denresults.2.B$Diff[c(Denresults.2.B$Diff!=0)]

#remove extra columns and go back to original name
Denresults.2.C = Denresults.2.B[,c(1:6,12)]
Denresults.2 = Denresults.2.C

#calculate egg density PER HECTARE
Denresults.2$EggDensity = (Denresults.2$CumEggs/Denresults.2$PolygonArea)*10000



### Calculate Eggs per Landcover Type ###

#36 Landcover types because different techs did it differently
unique(Denresults.2$HabType)
#29 counties
unique(Denresults.2$COUNTY)

EggsLaid2 = data.frame(matrix(nrow=17, ncol=31))
colnames(EggsLaid2) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.2$COUNTY)))
EggsLaid2$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                     "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#developing code for MWROW0 and MWROW1-5
eggsi = filter(Denresults.2, HabType == "MWROW0" | HabType == "MWROW_0")
EggsLaid2$DesMoinesLobe[1] = sum(eggsi$CumEggs)

eggsi = filter(Denresults.2, HabType == "MWROW1-5" | HabType == "MWROW1_5")
EggsLaid2$DesMoinesLobe[2] = sum(eggsi$CumEggs)

#loop through counties for MWROW1-5
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[2,i] = sum(eggsi$CumEggs)
}
#check sum
sum(EggsLaid2[2,3:31]) #9340

#MWROW0
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[1,i] = sum(eggsi$CumEggs)
}

#MWROW5-20
eggsi = filter(Denresults.2, HabType == "MWROW5-20" | HabType == "MWROW5_20")
EggsLaid2$DesMoinesLobe[3] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[3,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[3,3:31]) #365,818

#MWROW20-60
eggsi = filter(Denresults.2, HabType == "MWROW20-60" | HabType == "MWROW20_60")
EggsLaid2$DesMoinesLobe[4] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[4,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[4,3:31]) #894,870

#MWROW60-100
eggsi = filter(Denresults.2, HabType == "MWROW60-100" | HabType == "MWROW60_100")
EggsLaid2$DesMoinesLobe[5] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[5,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[5,3:31]) #3,073,934

#Grassland and Pasture
eggsi = filter(Denresults.2, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
EggsLaid2$DesMoinesLobe[6] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[6,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[6,3:31]) #2,013,852

#Low Intensity Development
eggsi = filter(Denresults.2, HabType == "Developed/Open Space/Developed/Low Intensity")
EggsLaid2$DesMoinesLobe[7] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[7,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[7,3:31]) #86,374

#RailroadROW
eggsi = filter(Denresults.2, HabType == "RailroadROW")
EggsLaid2$DesMoinesLobe[8] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[8,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[8,3:31]) #363,302

#Corn
eggsi = filter(Denresults.2, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
EggsLaid2$DesMoinesLobe[9] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[9,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[9,3:31]) #114,328

#NonGMOCorn
eggsi = filter(Denresults.2, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
EggsLaid2$DesMoinesLobe[10] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[10,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[10,3:31]) #13,988,404

#Soybeans
eggsi = filter(Denresults.2, HabType == "Soybeans")
EggsLaid2$DesMoinesLobe[11] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[11,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[11,3:31]) #81,470

#NonGMO Soybeans
eggsi = filter(Denresults.2, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
EggsLaid2$DesMoinesLobe[12] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[12,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[12,3:31]) #3,231,152

#All other types of agriculture
eggsi = filter(Denresults.2, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                 HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
EggsLaid2$DesMoinesLobe[13] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                   HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                 COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[13,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[13,3:31]) #22,658

#Forests of all kinds
eggsi = filter(Denresults.2, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                 HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                 HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
EggsLaid2$DesMoinesLobe[14] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                   HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                   HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                 COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[14,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[14,3:31]) #31,014

#Wetlands of all kinds
eggsi = filter(Denresults.2, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                 HabType == "Woody Wetlands/Herbaceous Wetlands" |
                 HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
EggsLaid2$DesMoinesLobe[15] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                   HabType == "Woody Wetlands/Herbaceous Wetlands" |
                   HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                 COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[15,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[15,3:31]) #222,336

#Med to High Intensity Development
eggsi = filter(Denresults.2, HabType == "Developed/Med Intensity/Developed/High Intensity")
EggsLaid2$DesMoinesLobe[16] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                 COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[16,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[16,3:31]) #0

#Water
eggsi = filter(Denresults.2, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                 HabType == "Open Water/Barren" |
                 HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                 HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
EggsLaid2$DesMoinesLobe[17] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.2, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                   HabType == "Open Water/Barren" |
                   HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                   HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                 COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  EggsLaid2[17,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid2[17,3:31]) #0

#export csv
write.csv(EggsLaid2, "EggsLaid2.csv")



######################## calc area of landcover type by county ######################

HabArea2 = data.frame(matrix(nrow=17, ncol=31))
colnames(HabArea2) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.2$COUNTY)))
HabArea2$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                    "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#MWROW0
habi = filter(Denresults.2, HabType == "MWROW0" | HabType == "MWROW_0")
HabArea2$DesMoinesLobe[1] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[1,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[1,3:31]) #22,777.29

#MWROW1-5
habi = filter(Denresults.2, HabType == "MWROW1-5" | HabType == "MWROW1_5")
HabArea2$DesMoinesLobe[2] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[2,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[2,3:31]) #11,808.77

#MWROW5-20
habi = filter(Denresults.2, HabType == "MWROW5-20" | HabType == "MWROW5_20")
HabArea2$DesMoinesLobe[3] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[3,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[3,3:31]) #37,614.72

#MWROW20-60
habi = filter(Denresults.2, HabType == "MWROW20-60" | HabType == "MWROW20_60")
HabArea2$DesMoinesLobe[4] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[4,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[4,3:31]) #36,582.02

#MWROW60-100+
habi = filter(Denresults.2, HabType == "MWROW60-100" | HabType == "MWROW60_100")
HabArea2$DesMoinesLobe[5] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[5,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[5,3:31]) #48,240.68

#Grass/Pasture
habi = filter(Denresults.2, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
HabArea2$DesMoinesLobe[6] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[6,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[6,3:31]) #234,511.6

#Low Int Dev
habi = filter(Denresults.2, HabType == "Developed/Open Space/Developed/Low Intensity")
HabArea2$DesMoinesLobe[7] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[7,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[7,3:31]) #135,771.9

#RailroadROW
habi = filter(Denresults.2, HabType == "RailroadROW")
HabArea2$DesMoinesLobe[8] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[8,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[8,3:31]) #7,982.949

#Corn
habi = filter(Denresults.2, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
HabArea2$DesMoinesLobe[9] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[9,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[9,3:31]) #1,294,174

#NonGMO Corn
habi = filter(Denresults.2, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
HabArea2$DesMoinesLobe[10] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[10,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[10,3:31]) #171,782.6

#Soybeans
habi = filter(Denresults.2, HabType == "Soybeans")
HabArea2$DesMoinesLobe[11] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[11,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[11,3:31]) #930,222.9

#NonGMO Beans
habi = filter(Denresults.2, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
HabArea2$DesMoinesLobe[12] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[12,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[12,3:31]) #34,267

#Other agriculture
habi = filter(Denresults.2, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
HabArea2$DesMoinesLobe[13] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                  HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[13,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[13,3:31]) #26,294.1

#Forests
habi = filter(Denresults.2, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
HabArea2$DesMoinesLobe[14] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                  HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                  HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[14,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[14,3:31]) #87,022.89

#Wetlands
habi = filter(Denresults.2, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                HabType == "Woody Wetlands/Herbaceous Wetlands" |
                HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
HabArea2$DesMoinesLobe[15] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                  HabType == "Woody Wetlands/Herbaceous Wetlands" |
                  HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[15,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[15,3:31]) #29,858.65

#Med to High Int Dev
habi = filter(Denresults.2, HabType == "Developed/Med Intensity/Developed/High Intensity")
HabArea2$DesMoinesLobe[16] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[16,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[16,3:31]) #19,402.57

#Water
habi = filter(Denresults.2, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                HabType == "Open Water/Barren" |
                HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
HabArea2$DesMoinesLobe[17] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.2, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                  HabType == "Open Water/Barren" |
                  HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                  HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                COUNTY == as.character(unique(Denresults.2$COUNTY))[i-2])
  HabArea2[17,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea2[17,3:31]) #30,647.19

#export csv
write.csv(HabArea2, "HabArea2.csv") 








################################### SCENARIO 3 ##################################################

#check to make sure all instances ran

Scen3.1M = read.csv("Monarchs.2021.Mar.17.05_14_13.txt")
length(unique(Scen3.1M$run)) #100
hist = hist(Scen3.1M$run, breaks = seq(0.5,100.5,1)); hist
Scen3.2M = read.csv("Monarchs.2021.Mar.19.10_02_28.txt")
length(unique(Scen3.2M$run)) #100
hist = hist(Scen3.2M$run, breaks = seq(0.5,100.5,1)); hist
Scen3.3M = read.csv("Monarchs.2021.Mar.19.12_10_19.txt")
length(unique(Scen3.3M$run)) #100
hist = hist(Scen3.3M$run, breaks = seq(0.5,100.5,1)); hist
Scen3.4M = read.csv("Monarchs.2021.Mar.20.10_57_04.txt")
length(unique(Scen3.4M$run)) #100
hist = hist(Scen3.4M$run, breaks = seq(0.5,100.5,1)); hist


#mean lifetime eggs laid for monarch agents

#Scen3.1
CEggsPerM3.1 = data.frame(matrix(nrow = 50000, ncol = 3))
colnames(CEggsPerM3.1) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen3.1M$run))
Mnames = sort(unique(Scen3.1M$name))
CEggsPerM3.1$Run = rep(Runs,500)
CEggsPerM3.1$Name = rep(Mnames,100)
for(i in 1:50000) {
  MonAg = filter(Scen3.1M, run == CEggsPerM3.1[i,1] & name == CEggsPerM3.1[i,2])
  CEggsPerM3.1[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM3.1$EggsLaid) #236.26 out of 410

#Scen3.2
CEggsPerM3.2 = data.frame(matrix(nrow = 50000, ncol = 3))
colnames(CEggsPerM3.2) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen3.2M$run))
Mnames = sort(unique(Scen3.2M$name))
CEggsPerM3.2$Run = rep(Runs,500)
CEggsPerM3.2$Name = rep(Mnames,100)
for(i in 1:50000) {
  MonAg = filter(Scen3.2M, run == CEggsPerM3.2[i,1] & name == CEggsPerM3.2[i,2])
  CEggsPerM3.2[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM3.2$EggsLaid) #237.97 out of 410

#Scen3.3
CEggsPerM3.3 = data.frame(matrix(nrow = 50000, ncol = 3))
colnames(CEggsPerM3.3) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen3.3M$run))
Mnames = sort(unique(Scen3.3M$name))
CEggsPerM3.3$Run = rep(Runs,500)
CEggsPerM3.3$Name = rep(Mnames,100)
for(i in 1:50000) {
  MonAg = filter(Scen3.3M, run == CEggsPerM3.3[i,1] & name == CEggsPerM3.3[i,2])
  CEggsPerM3.3[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM3.3$EggsLaid) #241.34 out of 410

#Scen3.4
CEggsPerM3.4 = data.frame(matrix(nrow = 50000, ncol = 3))
colnames(CEggsPerM3.4) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen3.4M$run))
Mnames = sort(unique(Scen3.4M$name))
CEggsPerM3.4$Run = rep(Runs,500)
CEggsPerM3.4$Name = rep(Mnames,100)
for(i in 1:50000) {
  MonAg = filter(Scen3.4M, run == CEggsPerM3.4[i,1] & name == CEggsPerM3.4[i,2])
  CEggsPerM3.4[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM3.4$EggsLaid) #240.02 out of 410

#combine all runs
CEggsPerM3 = rbind(CEggsPerM3.1, CEggsPerM3.2, CEggsPerM3.3, CEggsPerM3.4)
mean(CEggsPerM3$EggsLaid) #238.899 out of 410
mean(CEggsPerM3$EggsLaid)/sum(EggsToLaybyDay) #58.27
sd(CEggsPerM3$EggsLaid) #50.57
hist = hist(CEggsPerM3$EggsLaid, breaks = 40); hist
hist(CEggsPerM3$EggsLaid, xlim = c(0,400), xlab = "Lifetime Eggs Laid per Monarch Agent", main = "Eggs Laid by Monarch Agents in Scenario 3")
plot(density(CEggsPerM3$EggsLaid))
plot(density(CEggsPerM3$EggsLaid, bw=15))

#total eggs laid by individual monarchs
sum(CEggsPerM3$EggsLaid) #47,779,800


######## Import Workspaces from HPC Machines #######################

load("DMLDenresults_3.1.RData")
DML3.1 = Denresults
sum(DML3.1$CumEggs) #11,959,092
load("DMLDenresults_3.2.RData")
DML3.2 = Denresults
sum(DML3.2$CumEggs) #11,987,146
load("DMLDenresults_3.3.RData")
DML3.3 = Denresults
sum(DML3.3$CumEggs) #11,988,630
load("DMLDenresults_3.4.RData")
DML3.4 = Denresults
sum(DML3.4$CumEggs) #11,968,324

Denresults.3 = DML3.1

for (i in 1:868617) {
  Denresults.3[i,4] = DML3.1[i,4]+DML3.2[i,4]+DML3.3[i,4]+DML3.4[i,4]
}

sum(Denresults.3$CumEggs) #47,903,192
sum(DML3.1$CumEggs)+sum(DML3.2$CumEggs)+sum(DML3.3$CumEggs)+sum(DML3.4$CumEggs) #47,903,192


#load attribute table from new shapefile with county for each polygon
Attr3 = read.csv("Spatial_Join_DML4_Counties3_attr_table.txt")

#cbind Denresults.3 and Attr3
Denresults.3.B = cbind(Denresults.3,Attr3)
#subtract area from area to see if different in the two dataframes
Denresults.3.B$Diff = Denresults.3.B$PolygonArea-Denresults.3.B$Shape_Area
sum(Denresults.3.B$Diff) #very close to 0 - can't tell if a numerical thing or maybe a few polys not matched right?
Denresults.3.B$Diff[c(Denresults.3.B$Diff!=0)]

#remove extra columns and go back to original name
Denresults.3.C = Denresults.3.B[,c(1:6,12)]
Denresults.3 = Denresults.3.C

#calculate egg density PER HECTARE
Denresults.3$EggDensity = (Denresults.3$CumEggs/Denresults.3$PolygonArea)*10000

write.csv(Denresults.3, "Denresults.3.csv")



### Calculate Eggs per Landcover Type ###

#36 Landcover types because different techs did it differently
unique(Denresults.3$HabType)
#29 counties
unique(Denresults.3$COUNTY)

EggsLaid3 = data.frame(matrix(nrow=17, ncol=31))
colnames(EggsLaid3) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.3$COUNTY)))
EggsLaid3$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                      "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#developing code for MWROW0 and MWROW1-5
eggsi = filter(Denresults.3, HabType == "MWROW0" | HabType == "MWROW_0")
EggsLaid3$DesMoinesLobe[1] = sum(eggsi$CumEggs)

eggsi = filter(Denresults.3, HabType == "MWROW1-5" | HabType == "MWROW1_5")
EggsLaid3$DesMoinesLobe[2] = sum(eggsi$CumEggs)

#loop through counties for MWROW1-5
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[2,i] = sum(eggsi$CumEggs)
}
#check sum
sum(EggsLaid3[2,3:31]) #9340

#MWROW0
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[1,i] = sum(eggsi$CumEggs)
}

#MWROW5-20
eggsi = filter(Denresults.3, HabType == "MWROW5-20" | HabType == "MWROW5_20")
EggsLaid3$DesMoinesLobe[3] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[3,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[3,3:31]) #365,818

#MWROW20-60
eggsi = filter(Denresults.3, HabType == "MWROW20-60" | HabType == "MWROW20_60")
EggsLaid3$DesMoinesLobe[4] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[4,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[4,3:31]) #894,870

#MWROW60-100
eggsi = filter(Denresults.3, HabType == "MWROW60-100" | HabType == "MWROW60_100")
EggsLaid3$DesMoinesLobe[5] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[5,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[5,3:31]) #3,073,934

#Grassland and Pasture
eggsi = filter(Denresults.3, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
EggsLaid3$DesMoinesLobe[6] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[6,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[6,3:31]) #2,013,852

#Low Intensity Development
eggsi = filter(Denresults.3, HabType == "Developed/Open Space/Developed/Low Intensity")
EggsLaid3$DesMoinesLobe[7] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[7,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[7,3:31]) #86,374

#RailroadROW
eggsi = filter(Denresults.3, HabType == "RailroadROW")
EggsLaid3$DesMoinesLobe[8] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[8,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[8,3:31]) #363,302

#Corn
eggsi = filter(Denresults.3, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
EggsLaid3$DesMoinesLobe[9] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[9,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[9,3:31]) #114,328

#NonGMOCorn
eggsi = filter(Denresults.3, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
EggsLaid3$DesMoinesLobe[10] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[10,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[10,3:31]) #13,988,404

#Soybeans
eggsi = filter(Denresults.3, HabType == "Soybeans")
EggsLaid3$DesMoinesLobe[11] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[11,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[11,3:31]) #81,470

#NonGMO Soybeans
eggsi = filter(Denresults.3, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
EggsLaid3$DesMoinesLobe[12] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[12,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[12,3:31]) #3,231,152

#All other types of agriculture
eggsi = filter(Denresults.3, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                 HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
EggsLaid3$DesMoinesLobe[13] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                   HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                 COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[13,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[13,3:31]) #22,658

#Forests of all kinds
eggsi = filter(Denresults.3, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                 HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                 HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
EggsLaid3$DesMoinesLobe[14] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                   HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                   HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                 COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[14,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[14,3:31]) #31,014

#Wetlands of all kinds
eggsi = filter(Denresults.3, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                 HabType == "Woody Wetlands/Herbaceous Wetlands" |
                 HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
EggsLaid3$DesMoinesLobe[15] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                   HabType == "Woody Wetlands/Herbaceous Wetlands" |
                   HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                 COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[15,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[15,3:31]) #222,336

#Med to High Intensity Development
eggsi = filter(Denresults.3, HabType == "Developed/Med Intensity/Developed/High Intensity")
EggsLaid3$DesMoinesLobe[16] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                 COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[16,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[16,3:31]) #0

#Water
eggsi = filter(Denresults.3, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                 HabType == "Open Water/Barren" |
                 HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                 HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
EggsLaid3$DesMoinesLobe[17] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.3, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                   HabType == "Open Water/Barren" |
                   HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                   HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                 COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  EggsLaid3[17,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid3[17,3:31]) #0

#any doubles?
length(unique(as.numeric(EggsLaid3[15,])))
length(as.numeric(EggsLaid3[15,]))
length(unique(as.numeric(EggsLaid3[16,])))
length(as.numeric(EggsLaid3[16,]))

#export csv
write.csv(EggsLaid3, "EggsLaid3.csv")








######################## calc area of landcover type by county ######################

HabArea3 = data.frame(matrix(nrow=17, ncol=31))
colnames(HabArea3) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.2$COUNTY)))
HabArea3$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                     "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#MWROW0
habi = filter(Denresults.3, HabType == "MWROW0" | HabType == "MWROW_0")
HabArea3$DesMoinesLobe[1] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[1,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[1,3:31]) #22,777.29

#MWROW1-5
habi = filter(Denresults.3, HabType == "MWROW1-5" | HabType == "MWROW1_5")
HabArea3$DesMoinesLobe[2] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[2,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[2,3:31]) #11,808.77

#MWROW5-20
habi = filter(Denresults.3, HabType == "MWROW5-20" | HabType == "MWROW5_20")
HabArea3$DesMoinesLobe[3] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[3,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[3,3:31]) #37,614.72

#MWROW20-60
habi = filter(Denresults.3, HabType == "MWROW20-60" | HabType == "MWROW20_60")
HabArea3$DesMoinesLobe[4] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[4,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[4,3:31]) #36,582.02

#MWROW60-100+
habi = filter(Denresults.3, HabType == "MWROW60-100" | HabType == "MWROW60_100")
HabArea3$DesMoinesLobe[5] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[5,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[5,3:31]) #48,240.68

#Grass/Pasture
habi = filter(Denresults.3, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
HabArea3$DesMoinesLobe[6] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[6,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[6,3:31]) #234,511.6

#Low Int Dev
habi = filter(Denresults.3, HabType == "Developed/Open Space/Developed/Low Intensity")
HabArea3$DesMoinesLobe[7] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[7,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[7,3:31]) #135,771.9

#RailroadROW
habi = filter(Denresults.3, HabType == "RailroadROW")
HabArea3$DesMoinesLobe[8] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[8,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[8,3:31]) #7,982.949

#Corn
habi = filter(Denresults.3, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
HabArea3$DesMoinesLobe[9] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[9,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[9,3:31]) #1,294,174

#NonGMO Corn
habi = filter(Denresults.3, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
HabArea3$DesMoinesLobe[10] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[10,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[10,3:31]) #171,782.6

#Soybeans
habi = filter(Denresults.3, HabType == "Soybeans")
HabArea3$DesMoinesLobe[11] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[11,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[11,3:31]) #930,222.9

#NonGMO Beans
habi = filter(Denresults.3, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
HabArea3$DesMoinesLobe[12] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[12,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[12,3:31]) #34,267

#Other agriculture
habi = filter(Denresults.3, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
HabArea3$DesMoinesLobe[13] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                  HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[13,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[13,3:31]) #26,294.1

#Forests
habi = filter(Denresults.3, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
HabArea3$DesMoinesLobe[14] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                  HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                  HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[14,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[14,3:31]) #87,022.89

#Wetlands
habi = filter(Denresults.3, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                HabType == "Woody Wetlands/Herbaceous Wetlands" |
                HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
HabArea3$DesMoinesLobe[15] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                  HabType == "Woody Wetlands/Herbaceous Wetlands" |
                  HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[15,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[15,3:31]) #29,858.65

#Med to High Int Dev
habi = filter(Denresults.3, HabType == "Developed/Med Intensity/Developed/High Intensity")
HabArea3$DesMoinesLobe[16] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[16,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[16,3:31]) #19,402.57

#Water
habi = filter(Denresults.3, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                HabType == "Open Water/Barren" |
                HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
HabArea3$DesMoinesLobe[17] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.3, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                  HabType == "Open Water/Barren" |
                  HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                  HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                COUNTY == as.character(unique(Denresults.3$COUNTY))[i-2])
  HabArea3[17,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea3[17,3:31]) #30,647.19

#export csv
write.csv(HabArea3, "HabArea3.csv") 





#################################  Scenario 1 again ######################################


Scen1.1M = read.csv("Monarchs.2021.Apr.07.09_05_14.txt")
length(unique(Scen1.1M$run)) #30 of 50 ran, but 3 others failed, so 27 total are good
sort(unique(Scen1.1M$run))
hist = hist(Scen1.1M$run, breaks = seq(0.5,50.5,1)); hist
#runs 15, 25, and 45
Run15 = filter(Scen1.1M, run == 15)
Run25 = filter(Scen1.1M, run == 25)
Run45 = filter(Scen1.1M, run == 45)
#compared to full run:
Run1 = filter(Scen1.1M, run == 1) #10 ticks, 5000 rows
#look at monarch M435 in run 1
M435.1 = filter(Run1, name == "M435")
#remove instances that didn't fully run - should be 500*27*10=135,000
Scen1.1M.B = filter(Scen1.1M, run==1 | run==2 | run==3 | run==4 | run==7 | run==8 | run==11 | run==12 | run==13 | run==14 | run==17 | run==18 | 
                      run==23 | run==24 | run==27 | run==28 | run==29 | run==30 | run==33 | run==34 | run==35 | run==36 | run==41 | run==42 | 
                      run==43 | run==47 | run==48)

Scen1.2M = read.csv("Monarchs.2021.Apr.08.00_00_11.txt")
length(unique(Scen1.2M$run)) #50
hist = hist(Scen1.2M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.3M = read.csv("Monarchs.2021.Apr.08.02_27_34.txt")
length(unique(Scen1.3M$run)) #50
hist = hist(Scen1.3M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.4M = read.csv("Monarchs.2021.Apr.08.05_49_41.txt")
length(unique(Scen1.4M$run)) #50
hist = hist(Scen1.4M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.5M = read.csv("Monarchs.2021.Apr.08.08_36_30.txt")
length(unique(Scen1.5M$run)) #50
hist = hist(Scen1.5M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.6M = read.csv("Monarchs.2021.Apr.08.11_20_15.txt")
length(unique(Scen1.6M$run)) #50
hist = hist(Scen1.6M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.7M = read.csv("Monarchs.2021.Apr.09.00_22_24.txt")
length(unique(Scen1.7M$run)) #50
hist = hist(Scen1.7M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.8M = read.csv("Monarchs.2021.Apr.09.05_27_55.txt")
length(unique(Scen1.8M$run)) #50
hist = hist(Scen1.8M$run, breaks = seq(0.5,50.5,1)); hist
Scen1.9M = read.csv("Monarchs.2021.Apr.09.09_50_54.txt")
length(unique(Scen1.9M$run)) #50
hist = hist(Scen1.9M$run, breaks = seq(0.5,50.5,1)); hist



#eggs avaiable to lay each day: 50, 48, 46, 44, 42, 
EggsToLaybyDay = c(50,48,46,44,42,40,38,36,34,32)
length(EggsToLaybyDay) #10
sum(EggsToLaybyDay) #410

#look at mean eggs laid per day per tick
MonEggs1.1 = data.frame(matrix(nrow=10, ncol=5))
colnames(MonEggs1.1) = c("Tick", "MeanEggsLaid","SDEggsLaid","MeanEggsToLay","SDEggsToLay")
Mon = Scen1.1M.B

for(i in 1:10){
  tick = filter(Mon, tick == i) #look at each tick 
  MonEggs1.1[i,1] = i #tick number
  MonEggs1.1[i,2] = mean(tick$EggsLaid, na.rm = TRUE) #mean eggs laid across all monarch agents in tick i
  MonEggs1.1[i,3] = sd(tick$EggsLaid)
  MonEggs1.1[i,4] = mean(tick$EggsToLay, na.rm = TRUE) #mean eggs left to lay across all monarch agents in tick i
  MonEggs1.1[i,5] = sd(tick$EggsToLay, na.rm = TRUE)
}

MonEggs1.1Z = MonEggs1.1 #with all instances, including bad ones

#plot histograms of eggs laid for each tick
library(sm)
hist(tick$EggsLaid)
hist(tick$EggsToLay)
plot(density(tick$EggsLaid))
sm.density.compare(Scen1.2M$EggsLaid, Scen1.2M$tick)



############# mean eggs laid per monarch ##################

#Scen1.2
CEggsPerM1.2 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.2) = c("Run","Name","EggsLaid")
#test
filter(Scen1.2M, run == 1 & name == "M425")
Mnames = sort(unique(Scen1.2M$name))
Runs = sort(unique(Scen1.2M$run))

CEggsPerM1.2$Run = rep(Runs,500)
CEggsPerM1.2$Name = rep(Mnames,50)

for(i in 1:25000) {
  MonAg = filter(Scen1.2M, run == CEggsPerM1.2[i,1] & name == CEggsPerM1.2[i,2])
  CEggsPerM1.2[i,3] = sum(MonAg$EggsLaid)
}
#mean eggs laid by monarchs in Scen 1.2
mean(CEggsPerM1.2$EggsLaid) #220.136 out of 410
mean(CEggsPerM1.2$EggsLaid)/sum(EggsToLaybyDay) #53.69
hist(CEggsPerM1.2$EggsLaid)
plot(density(CEggsPerM1.2$EggsLaid))

#Scen1.1 - code block for 1.1 now obsolete - need the change and rerun
CEggsPerM1.1 = data.frame(matrix(nrow = 13500, ncol = 3))
colnames(CEggsPerM1.1) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.1M.B$run))
Mnames = sort(unique(Scen1.1M.B$name))
CEggsPerM1.1$Run = rep(Runs,500)
CEggsPerM1.1$Name = rep(Mnames,27)
for(i in 1:13500) {
  MonAg = filter(Scen1.1M.B, run == Scen1.1M.B[i,1] & name == Scen1.1M.B[i,2]) # should be CEggsPerM1.1 not Scen1.1M.B
  CEggsPerM1.1[i,3] = sum(MonAg$EggsLaid)
}

mean(CEggsPerM1.1$EggsLaid) #214.6135 out of 410
mean(CEggsPerM1.1$EggsLaid)/sum(EggsToLaybyDay) #52.34475
hist(CEggsPerM1.1$EggsLaid)
plot(density(CEggsPerM1.1$EggsLaid))
#should be close or the same as this, just as a check. Very close but not exactly the same for some reason. 
sum(MonEggs1.1$MeanEggsLaid) #215.0024


#Scen1.3
CEggsPerM1.3 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.3) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.3M$run))
Mnames = sort(unique(Scen1.3M$name))
CEggsPerM1.3$Run = rep(Runs,500)
CEggsPerM1.3$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.3M, run == CEggsPerM1.3[i,1] & name == CEggsPerM1.3[i,2])
  CEggsPerM1.3[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.3$EggsLaid) #218.316 out of 410

#Scen1.4
CEggsPerM1.4 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.4) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.4M$run))
Mnames = sort(unique(Scen1.4M$name))
CEggsPerM1.4$Run = rep(Runs,500)
CEggsPerM1.4$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.4M, run == CEggsPerM1.4[i,1] & name == CEggsPerM1.4[i,2])
  CEggsPerM1.4[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.4$EggsLaid) #216.412 out of 410

#Scen1.5
CEggsPerM1.5 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.5) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.5M$run))
Mnames = sort(unique(Scen1.5M$name))
CEggsPerM1.5$Run = rep(Runs,500)
CEggsPerM1.5$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.5M, run == CEggsPerM1.5[i,1] & name == CEggsPerM1.5[i,2])
  CEggsPerM1.5[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.5$EggsLaid) #216.272 out of 410

#Scen1.6
CEggsPerM1.6 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.6) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.6M$run))
Mnames = sort(unique(Scen1.6M$name))
CEggsPerM1.6$Run = rep(Runs,500)
CEggsPerM1.6$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.6M, run == CEggsPerM1.6[i,1] & name == CEggsPerM1.6[i,2])
  CEggsPerM1.6[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.6$EggsLaid) #217.096 out of 410

#Scen1.7
CEggsPerM1.7 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.7) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.7M$run))
Mnames = sort(unique(Scen1.7M$name))
CEggsPerM1.7$Run = rep(Runs,500)
CEggsPerM1.7$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.7M, run == CEggsPerM1.7[i,1] & name == CEggsPerM1.7[i,2])
  CEggsPerM1.7[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.7$EggsLaid) #215.34 out of 410

#Scen1.8
CEggsPerM1.8 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.8) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.8M$run))
Mnames = sort(unique(Scen1.8M$name))
CEggsPerM1.8$Run = rep(Runs,500)
CEggsPerM1.8$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.8M, run == CEggsPerM1.8[i,1] & name == CEggsPerM1.8[i,2])
  CEggsPerM1.8[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.8$EggsLaid) #212.92 out of 410

#Scen1.9
CEggsPerM1.9 = data.frame(matrix(nrow = 25000, ncol = 3))
colnames(CEggsPerM1.9) = c("Run","Name","EggsLaid")
Runs = sort(unique(Scen1.9M$run))
Mnames = sort(unique(Scen1.9M$name))
CEggsPerM1.9$Run = rep(Runs,500)
CEggsPerM1.9$Name = rep(Mnames,50)
for(i in 1:25000) {
  MonAg = filter(Scen1.9M, run == CEggsPerM1.9[i,1] & name == CEggsPerM1.9[i,2])
  CEggsPerM1.9[i,3] = sum(MonAg$EggsLaid)
}
mean(CEggsPerM1.9$EggsLaid) #219.62 out of 410


#combined all monarchs from all runs
CEggsPerM1 = rbind(CEggsPerM1.2, CEggsPerM1.3, CEggsPerM1.4, CEggsPerM1.5, CEggsPerM1.6, CEggsPerM1.7, CEggsPerM1.8, CEggsPerM1.9)
mean(CEggsPerM1$EggsLaid) #217.01 out of 410
mean(CEggsPerM1$EggsLaid)/sum(EggsToLaybyDay) #52.93
sd(CEggsPerM1$EggsLaid) #53.93
hist(CEggsPerM1$EggsLaid)
hist(CEggsPerM1$EggsLaid, xlim = c(0,400), xlab = "Lifetime Eggs Laid per Monarch Agent", main = "Eggs Laid by Monarch Agents in Scenario 1")
plot(density(CEggsPerM1$EggsLaid))
plot(density(CEggsPerM1$EggsLaid, bw=10))


####### compare all 3 scenario density plots ##############

#plot histograms of eggs laid for each tick
library(sm)
hist(tick$EggsLaid)
hist(tick$EggsToLay)
plot(density(tick$EggsLaid))
sm.density.compare(Scen1.2M$EggsLaid, Scen1.2M$tick)

CEggsPerM1Z=CEggsPerM1
CEggsPerM1Z$Scen = 1
CEggsPerM2Z=CEggsPerM2
CEggsPerM2Z$Scen = 2
CEggsPerM3Z=CEggsPerM3
CEggsPerM3Z$Scen = 3
CEggsPerMAllZ = rbind(CEggsPerM1Z,CEggsPerM2Z,CEggsPerM3Z)
sm.density.compare(CEggsPerMAllZ$EggsLaid, group = CEggsPerMAllZ$Scen, h = 12, xlab = "Number of Eggs Laid", xlim = c(0,400), 
                   lty = c(1,2,3), col = c(1,1,1), yaxt = "n")

sum(CEggsPerM1$EggsLaid) #43,402,900
sum(CEggsPerM2$EggsLaid) #53,714,320
sum(CEggsPerM3$EggsLaid) #47,779,800



######## Import Workspaces from HPC Machines #######################

load("DMLDenresults_1B.2.RData")
DML1B.2 = Denresults
sum(DML1B.2$CumEggs) #5,407,136
load("DMLDenresults_1B.3.RData") 
DML1B.3 = Denresults
sum(DML1B.3$CumEggs) #5,394,048
load("DMLDenresults_1B.4.RData")
DML1B.4 = Denresults
sum(DML1B.4$CumEggs) #5,397,322
load("DMLDenresults_1B.5.RData")
DML1B.5 = Denresults
sum(DML1B.5$CumEggs) #5,396,626
load("DMLDenresults_1B.6.RData")
DML1B.6 = Denresults
sum(DML1B.6$CumEggs) #5,405,090
load("DMLDenresults_1B.7.RData")
DML1B.7 = Denresults
sum(DML1B.7$CumEggs) #5,397,660
load("DMLDenresults_1B.8.RData")
DML1B.8 = Denresults
sum(DML1B.8$CumEggs) #5,395,736
load("DMLDenresults_1B.9.RData")
DML1B.9 = Denresults
sum(DML1B.9$CumEggs) #5,406,912

Denresults.1 = DML1B.2

for (i in 1:868617) {
  Denresults.1[i,4] = DML1B.2[i,4]+DML1B.3[i,4]+DML1B.4[i,4]+DML1B.5[i,4]+DML1B.6[i,4]+DML1B.7[i,4]+DML1B.8[i,4]+DML1B.9[i,4]
}

sum(Denresults.1$CumEggs) #43,200,530

#load attribute table from new shapefile with county for each polygon
Attr3 = read.csv("Spatial_Join_DML4_Counties3_attr_table.txt")

#cbind Denresults.3 and Attr3
Denresults.1.B = cbind(Denresults.1,Attr3)
#subtract area from area to see if different in the two dataframes
Denresults.1.B$Diff = Denresults.1.B$PolygonArea-Denresults.1.B$Shape_Area
sum(Denresults.1.B$Diff) #very close to 0 - can't tell if a numerical thing or maybe a few polys not matched right?
Denresults.1.B$Diff[c(Denresults.1.B$Diff!=0)]

#remove extra columns and go back to original name
Denresults.1.C = Denresults.1.B[,c(1:6,12)]
Denresults.1 = Denresults.1.C

#calculate egg density PER HECTARE
Denresults.1$EggDensity = (Denresults.1$CumEggs/Denresults.1$PolygonArea)*10000

write.csv(Denresults.1, "Denresults.1.csv")
write.csv(Denresults.2, "Denresults.2.csv")



### Calculate Eggs per Landcover Type ###

#36 Landcover types because different techs did it differently
unique(Denresults.1$HabType)
#29 counties
unique(Denresults.1$COUNTY)

EggsLaid1 = data.frame(matrix(nrow=17, ncol=31))
colnames(EggsLaid1) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.3$COUNTY)))
EggsLaid1$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                      "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#developing code for MWROW0 and MWROW1-5
eggsi = filter(Denresults.1, HabType == "MWROW0" | HabType == "MWROW_0")
EggsLaid1$DesMoinesLobe[1] = sum(eggsi$CumEggs)

eggsi = filter(Denresults.1, HabType == "MWROW1-5" | HabType == "MWROW1_5")
EggsLaid1$DesMoinesLobe[2] = sum(eggsi$CumEggs)

#loop through counties for MWROW1-5
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[2,i] = sum(eggsi$CumEggs)
}
#check sum
sum(EggsLaid1[2,3:31]) #9362

#MWROW0
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[1,i] = sum(eggsi$CumEggs)
}

#MWROW5-20
eggsi = filter(Denresults.1, HabType == "MWROW5-20" | HabType == "MWROW5_20")
EggsLaid1$DesMoinesLobe[3] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[3,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[3,3:31]) #361,240

#MWROW20-60
eggsi = filter(Denresults.1, HabType == "MWROW20-60" | HabType == "MWROW20_60")
EggsLaid1$DesMoinesLobe[4] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[4,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[4,3:31]) #784,458

#MWROW60-100
eggsi = filter(Denresults.1, HabType == "MWROW60-100" | HabType == "MWROW60_100")
EggsLaid1$DesMoinesLobe[5] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[5,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[5,3:31]) #3,028,276

#Grassland and Pasture
eggsi = filter(Denresults.1, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
EggsLaid1$DesMoinesLobe[6] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[6,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[6,3:31]) #19,447,258

#Low Intensity Development
eggsi = filter(Denresults.1, HabType == "Developed/Open Space/Developed/Low Intensity")
EggsLaid1$DesMoinesLobe[7] = sum(eggsi$CumEggs)

for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[7,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[7,3:31]) #73,604

#RailroadROW
eggsi = filter(Denresults.1, HabType == "RailroadROW")
EggsLaid1$DesMoinesLobe[8] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[8,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[8,3:31]) #346,432

#Corn
eggsi = filter(Denresults.1, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
EggsLaid1$DesMoinesLobe[9] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[9,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[9,3:31]) #112,076

#NonGMOCorn
eggsi = filter(Denresults.1, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
EggsLaid1$DesMoinesLobe[10] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[10,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[10,3:31]) #15,137,378

#Soybeans
eggsi = filter(Denresults.1, HabType == "Soybeans")
EggsLaid1$DesMoinesLobe[11] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[11,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[11,3:31]) #81,108

#NonGMO Soybeans
eggsi = filter(Denresults.1, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
EggsLaid1$DesMoinesLobe[12] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[12,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[12,3:31]) #3,458,106

#All other types of agriculture
eggsi = filter(Denresults.1, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                 HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                 HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
EggsLaid1$DesMoinesLobe[13] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                   HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                   HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                 COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[13,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[13,3:31]) #21,808

#Forests of all kinds
eggsi = filter(Denresults.1, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                 HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                 HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
EggsLaid1$DesMoinesLobe[14] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                   HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                   HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                 COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[14,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[14,3:31]) #29,266

#Wetlands of all kinds
eggsi = filter(Denresults.1, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                 HabType == "Woody Wetlands/Herbaceous Wetlands" |
                 HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
EggsLaid1$DesMoinesLobe[15] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                   HabType == "Woody Wetlands/Herbaceous Wetlands" |
                   HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                 COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[15,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[15,3:31]) #216,158

#Med to High Intensity Development
eggsi = filter(Denresults.1, HabType == "Developed/Med Intensity/Developed/High Intensity")
EggsLaid1$DesMoinesLobe[16] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                 COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[16,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[16,3:31]) #0

#Water
eggsi = filter(Denresults.1, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                 HabType == "Open Water/Barren" |
                 HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                 HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
EggsLaid1$DesMoinesLobe[17] = sum(eggsi$CumEggs)
for (i in 3:31) {
  eggsi = filter(Denresults.1, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                   HabType == "Open Water/Barren" |
                   HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                   HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                 COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  EggsLaid1[17,i] = sum(eggsi$CumEggs)
}
sum(EggsLaid1[17,3:31]) #0

#any doubles?
length(unique(as.numeric(EggsLaid1[15,])))
length(as.numeric(EggsLaid1[15,]))
length(unique(as.numeric(EggsLaid1[16,])))
length(as.numeric(EggsLaid1[16,]))

#export csv
write.csv(EggsLaid1, "EggsLaid1.csv")





######################## calc area of landcover type by county ######################

HabArea1 = data.frame(matrix(nrow=17, ncol=31))
colnames(HabArea1) = c("HabType","DesMoinesLobe",as.character(unique(Denresults.2$COUNTY)))
HabArea1$HabType = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100+","Grass/Pasture","LowIntDev","RailroadROW",
                     "Corn","NonGMOCorn","Soybeans","NonGMOSoybeans","OtherAg","Forest","Wetlands","MedHighIntDev","Water")

#MWROW0
habi = filter(Denresults.1, HabType == "MWROW0" | HabType == "MWROW_0")
HabArea1$DesMoinesLobe[1] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "MWROW0" | HabType == "MWROW_0", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[1,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[1,3:31]) 

#MWROW1-5
habi = filter(Denresults.1, HabType == "MWROW1-5" | HabType == "MWROW1_5")
HabArea1$DesMoinesLobe[2] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "MWROW1-5" | HabType == "MWROW1_5", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[2,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[2,3:31]) #11,808.77

#MWROW5-20
habi = filter(Denresults.1, HabType == "MWROW5-20" | HabType == "MWROW5_20")
HabArea1$DesMoinesLobe[3] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "MWROW5-20" | HabType == "MWROW5_20", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[3,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[3,3:31]) #37,614.72

#MWROW20-60
habi = filter(Denresults.1, HabType == "MWROW20-60" | HabType == "MWROW20_60")
HabArea1$DesMoinesLobe[4] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "MWROW20-60" | HabType == "MWROW20_60", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[4,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[4,3:31]) #36,582.02

#MWROW60-100+
habi = filter(Denresults.1, HabType == "MWROW60-100" | HabType == "MWROW60_100")
HabArea1$DesMoinesLobe[5] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "MWROW60-100" | HabType == "MWROW60_100", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[5,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[5,3:31]) #48,240.68

#Grass/Pasture
habi = filter(Denresults.1, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture")
HabArea1$DesMoinesLobe[6] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Grass/Pasture" | HabType == "Sod/Grass Seed/Switchgrass/Grass/Pasture", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[6,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[6,3:31]) #234,511.6

#Low Int Dev
habi = filter(Denresults.1, HabType == "Developed/Open Space/Developed/Low Intensity")
HabArea1$DesMoinesLobe[7] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Developed/Open Space/Developed/Low Intensity", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[7,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[7,3:31]) #135,771.9

#RailroadROW
habi = filter(Denresults.1, HabType == "RailroadROW")
HabArea1$DesMoinesLobe[8] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "RailroadROW", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[8,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[8,3:31]) #7,982.949

#Corn
habi = filter(Denresults.1, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn")
HabArea1$DesMoinesLobe[9] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Corn" | HabType == "Corn/Sweet Corn/Pop or Orn Corn", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[9,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[9,3:31]) #1,294,174

#NonGMO Corn
habi = filter(Denresults.1, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO")
HabArea1$DesMoinesLobe[10] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "NonGMOCorn" | HabType == "Corn_nonGMO", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[10,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[10,3:31]) #171,782.6

#Soybeans
habi = filter(Denresults.1, HabType == "Soybeans")
HabArea1$DesMoinesLobe[11] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Soybeans", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[11,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[11,3:31]) #930,222.9

#NonGMO Beans
habi = filter(Denresults.1, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO")
HabArea1$DesMoinesLobe[12] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "NonGMOSoybeans" | HabType == "Soybeans_nonGMO", COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[12,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[12,3:31]) #34,267

#Other agriculture
habi = filter(Denresults.1, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other")
HabArea1$DesMoinesLobe[13] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Cotton/Rice/Sorghum/Sunflower/Blank_/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Whea" | 
                  HabType == "Sorghum/Sunflower/Barley/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other Hay/Non Alfalfa/Sugarb" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa" |
                  HabType == "Sorghum/Sweet Corn/Pop or Orn Corn/Durum Wheat/Spring Wheat/Winter Wheat/Dbl Crop WinWht/Soybeans/Rye/Oats/Millet/Alfalfa/Other", 
                COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[13,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[13,3:31]) #26,294.1

#Forests
habi = filter(Denresults.1, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest")
HabArea1$DesMoinesLobe[14] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands" | 
                  HabType == "Deciduous Forest/Evergreen Forest/Mixed Forest/Shrubland" |
                  HabType == "Forest/Shrubland/Deciduous Forest/Evergreen Forest/Mixed Forest", 
                COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[14,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[14,3:31]) #87,022.89

#Wetlands
habi = filter(Denresults.1, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                HabType == "Woody Wetlands/Herbaceous Wetlands" |
                HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands")
HabArea1$DesMoinesLobe[15] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Blank_/Wetlands/Herbaceous Wetlands" | 
                  HabType == "Woody Wetlands/Herbaceous Wetlands" |
                  HabType == "Blank_/Wetlands/Woody Wetlands/Herbaceous Wetlands", 
                COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[15,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[15,3:31]) #29,858.65

#Med to High Int Dev
habi = filter(Denresults.1, HabType == "Developed/Med Intensity/Developed/High Intensity")
HabArea1$DesMoinesLobe[16] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Developed/Med Intensity/Developed/High Intensity", 
                COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[16,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[16,3:31]) #19,402.57

#Water
habi = filter(Denresults.1, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                HabType == "Open Water/Barren" |
                HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
HabArea1$DesMoinesLobe[17] = sum(habi$PolygonArea)/10000
for (i in 3:31) {
  habi = filter(Denresults.1, HabType == "Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow" | 
                  HabType == "Open Water/Barren" |
                  HabType == "Fallow/Idle Cropland/Open Water/Barren" |
                  HabType == "Background/Blank_/Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow", 
                COUNTY == as.character(unique(Denresults.1$COUNTY))[i-2])
  HabArea1[17,i] = sum(habi$PolygonArea)/10000
}
sum(HabArea1[17,3:31]) #30,647.19

#export csv
write.csv(HabArea1, "HabArea1.csv") 





##################### Spatial Analysis #################################

#import g/p scen1 dbf with area and perimeter

GP1 = read.csv("Grass-P.Scen1.txt")
#perimeter-area ratio
GP1$PAratio = GP1$Perimeter/GP1$Area3
hist(GP1$PAratio)
#shape index
GP1$ShapeInd = (0.25*GP1$Perimeter)/(sqrt(GP1$Area3))
hist(GP1$ShapeInd)
#Fractal Dimension Index
GP1$FractalInd = (2*log(0.25*GP1$Perimeter))/(log(GP1$Area3))
hist(GP1$FractalInd)

#egg density graphs
plot(GP1$Area3, GP1$EggDensity)
smoothScatter(GP1$Area3, GP1$EggDensity)
smoothScatter(GP1$AreaHA, GP1$EggDensity)
smoothScatter(GP1$Perimeter, GP1$EggDensity)
plot(GP1$Perimeter, GP1$EggDensity)
plot(GP1$ShapeInd, GP1$EggDensity)
smoothScatter(GP1$ShapeInd, GP1$EggDensity)
smoothScatter(GP1$FractalInd, GP1$EggDensity)

#cumulative eggs graphs
plot(GP1$Area3, GP1$CumEggs)
smoothScatter(GP1$AreaHA, GP1$CumEggs)

#exclude larger plots
hist(GP1$Area3, breaks = 100)
#first convert to ha
GP1$AreaHA = GP1$Area3/10000
hist(GP1$AreaHA, breaks = 100)
#exclude everything over 50 ha then 20 ha
GP1.50 = filter(GP1, AreaHA <= 50)
hist(GP1.50$AreaHA, breaks = 100)
#percentage of total
1-nrow(GP1.50)/nrow(GP1) #0.9%

GP1.20 = filter(GP1, AreaHA <= 20)
hist(GP1.20$AreaHA)
smoothScatter(GP1.20$AreaHA, GP1.20$EggDensity)
smoothScatter(GP1.20$Perimeter, GP1.20$EggDensity)
smoothScatter(GP1.20$ShapeInd, GP1.20$EggDensity)
smoothScatter(GP1.20$FractalInd, GP1.20$EggDensity)
1-nrow(GP1.20)/nrow(GP1) #3.1%

GP1.5 = filter(GP1, AreaHA <= 5)
hist(GP1.5$AreaHA, breaks = 50)
smoothScatter(GP1.5$AreaHA, GP1.5$EggDensity)
smoothScatter(GP1.5$Perimeter, GP1.5$EggDensity)
smoothScatter(GP1.5$ShapeInd, GP1.5$EggDensity)
smoothScatter(GP1.5$FractalInd, GP1.5$EggDensity)
1-nrow(GP1.5)/nrow(GP1) #13.9%

GP1.1.5 = filter(GP1, AreaHA <= 1.5)
hist(GP1.1.5$AreaHA, breaks = 200)
#at this scale, can see effect of 30m^2 raster squares
#highest density is about 0.449 ha = 4496 m^2 ~4500 m2, which is about 50 90m2 pixels
smoothScatter(GP1.1.5$AreaHA, GP1.1.5$EggDensity)
plot(GP1.1.5$AreaHA, GP1.1.5$EggDensity, pch = 19, cex = 0.25)
#what's happening under 0.4 ha??

GP1.0.4 = filter(GP1, AreaHA <= 0.4)
hist(GP1.0.4$AreaHA, breaks = 200)
#maybe cut off data at 0.20 to see if causes early hook
plot(GP1.0.4$AreaHA, GP1.0.4$EggDensity, pch = 19, cex = 0.25)
smoothScatter(GP1.0.4$AreaHA, GP1.0.4$EggDensity)
#the striated rows maybe have to do with the clumped areas of patches?

#cut data off from 0.2 ha to 20 ha
GP1.0.2_20 = filter(GP1, AreaHA > 0.2 & AreaHA <= 20)


#linear regression
#all data
fit1 = lm(GP1$EggDensity ~ GP1$AreaHA, data = GP1)
summary(fit1)
smoothScatter(GP1$AreaHA, GP1$EggDensity)
abline(fit1)

fit2 = lm(GP1$EggDensity ~ GP1$Perimeter, data = GP1)
summary(fit2)
smoothScatter(GP1$Perimeter, GP1$EggDensity)
abline(fit2)

fit3 = lm(GP1$EggDensity ~ GP1$ShapeInd, data = GP1)
summary(fit3)
smoothScatter(GP1$ShapeInd, GP1$EggDensity)
abline(fit3)

fit4 = lm(GP1$EggDensity ~ GP1$FractalInd, data = GP1)
summary(fit4)
smoothScatter(GP1$FractalInd, GP1$EggDensity)
abline(fit4)

#polygons <= 5 ha
fit5 = lm(GP1.5$EggDensity ~ GP1.5$AreaHA, data = GP1.5)
summary(fit5)
smoothScatter(GP1.5$AreaHA, GP1.5$EggDensity)
abline(fit5)

fit6 = lm(GP1.5$EggDensity ~ GP1.5$FractalInd, data = GP1.5)
summary(fit6)
smoothScatter(GP1.5$FractalInd, GP1.5$EggDensity)
abline(fit6)

fit7 = lm(GP1.5$EggDensity ~ GP1.5$FractalInd + GP1.5$AreaHA, data = GP1.5)
summary(fit7)

cor(GP1.5$AreaHA,GP1.5$FractalInd) #0.08
cor(GP1.5$ShapeInd,GP1.5$FractalInd) #0.97

fit8 = lm(GP1.5$EggDensity ~ GP1.5$ShapeInd, data = GP1.5)
summary(fit8)
smoothScatter(GP1.5$ShapeInd, GP1.5$EggDensity)
abline(fit8)

fit24 = lm(GP1.1.5$EggDensity ~ GP1.1.5$AreaHA, data = GP1.1.5)
summary(fit24)
smoothScatter(GP1.1.5$AreaHA, GP1.1.5$EggDensity)
abline(fit24)


#nonlinear regression

#polynomial regression
#AreaHA
fit9 = lm(EggDensity ~ poly(AreaHA, 9, raw = TRUE), data = GP1)
summary(fit9)
#tested up to 15 poly terms, which were all sig, but some gave NA

ggplot(GP1, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE))
#nonsense - tested less terms 1 at a time and NONE make sense

#try with <5 ha
fit10 = lm(EggDensity ~ poly(AreaHA, 6, raw = TRUE), data = GP1.5)
summary(fit10)

ggplot(GP1.5, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 6, raw = TRUE))
#pretty good

#try with < 20 ha
fit11 = lm(EggDensity ~ poly(AreaHA, 6, raw = TRUE), data = GP1.20)
summary(fit11)

ggplot(GP1.20, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 6, raw = TRUE))

#Fractal Ind
fit12 = lm(EggDensity ~ poly(FractalInd, 6, raw = TRUE), data = GP1.20)
summary(fit12)

ggplot(GP1.20, aes(FractalInd, EggDensity) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))



#GAM regression - from http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/
library(mgcv)

fit20 = gam(EggDensity ~ s(AreaHA), data = GP1)
summary(fit20)

ggplot(GP1, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
#bad

ggplot(GP1.5, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
#good

ggplot(GP1.50, aes(AreaHA, EggDensity) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
#a little weird

#GP1.20 looks very very good

#FINAL FIGURE 9 - Area by Egg Density
fit21 = gam(EggDensity ~ s(AreaHA), data = GP1.20)
summary(fit21)
max(fit21$fitted.values) #117.64
match(max(fit21$fitted.values), fit21$fitted) #42773
GP1.20$AreaHA[match(max(fit21$fitted.values), fit21$fitted)] #2.87 ha
predict.gam(fit21, data.frame(AreaHA = 1:20)) #84.09 at 20 ha
predict.gam(fit21, data.frame(AreaHA = 0.2)) #84.09 at 20 ha

p = ggplot(GP1.20, aes(AreaHA, EggDensity) ) 

#p + stat_density_2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
#  stat_smooth(method = gam, formula = y ~ s(x))

#p + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
#  stat_smooth(method = gam, formula = y ~ s(x))

p + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab("Polygon Area (Ha)") +
  ylab("Eggs Per Hectare") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(r=10)),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size=14),
        plot.margin = margin(20, 10, 10, 10, unit = "pt")
  ) +
  stat_smooth(method = gam, formula = y ~ s(x))


#decided not to use this data cut
fit25 = gam(EggDensity ~ s(AreaHA), data = GP1.0.2_20)
summary(fit25)
p = ggplot(GP1.0.2_20, aes(AreaHA, EggDensity) ) 
p + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab("Area (Ha)") +
  ylab("Eggs Per Hectare") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, margin = margin(r=10)),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size=16)
        ) +
  stat_smooth(method = gam, formula = y ~ s(x))
#basically exactly the same as including all data under 20ha, so making no changes


#fractal index
fit22 = gam(EggDensity ~ s(FractalInd), data = GP1.20)
summary(fit22)

p2 = ggplot(GP1.20, aes(FractalInd, EggDensity) ) 
p2 + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab("Fractal Index") +
  ylab("Eggs Per Hectare") +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20, margin = margin(r=10)),
          axis.text.x = element_text(size = 16),
          axis.text.y  = element_text(size=16),
          plot.margin = margin(20, 10, 10, 10, unit = "pt")) +
  stat_smooth(method = gam, formula = y ~ s(x))


#perimeter
fit22 = gam(EggDensity ~ s(Perimeter), data = GP1.20)
summary(fit22)

p2 = ggplot(GP1.20, aes(Perimeter, EggDensity) ) 
p2 + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab("Perimeter (m)") +
  ylab("Eggs Per Hectare") +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18, margin = margin(r=10)),
          axis.text.x = element_text(size = 16),
          axis.text.y  = element_text(size=14),
          plot.margin = margin(20, 10, 10, 10, unit = "pt")) +
  stat_smooth(method = gam, formula = y ~ s(x))

cor(GP1.20$AreaHA,GP1.20$Perimeter) #92%

#eggs per patch/cumeggs by area
fit23 = gam(CumEggs ~ s(AreaHA), data = GP1.20)
summary(fit23)

p3 = ggplot(GP1.20, aes(AreaHA, CumEggs) ) 
p3 + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  theme(legend.position = "none") +
  stat_smooth(method = gam, formula = y ~ s(x))

#Shape Index
fit24 = gam(EggDensity ~ s(ShapeInd), data = GP1.20)
summary(fit24)
cor(GP1.20$ShapeInd,GP1.20$FractalInd) #96%

p3 = ggplot(GP1.20, aes(ShapeInd, EggDensity))
p3 + stat_density_2d(geom="tile", aes(fill = ..density..^0.25), contour = FALSE) +
  #geom_point(size=0.5) +
  stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.6,0,1)), contour=FALSE) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab("Shape Index") +
  ylab("Eggs Per Hectare") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(r=10)),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size=14),
        plot.margin = margin(20, 10, 10, 10, unit = "pt")) +
  stat_smooth(method = gam, formula = y ~ s(x))





#loess regression
#apparently default span is 0.75

#AreaHA
fit13 = loess(EggDensity ~ AreaHA, data = GP1)
summary(fit13)
smoothScatter(GP1$AreaHA, GP1$EggDensity)
j = order(GP1$AreaHA)
lines(GP1$AreaHA[j], fit13$fitted[j])

#function to calculate SSE - from http://r-statistics.co/Loess-Regression-With-R.html
calcSSE <- function(x){
  print(x)
  loessMod <- try(loess(EggDensity ~ AreaHA, data=GP1, span=x), silent=F)
  res <- try(loessMod$residuals, silent=F)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  print(sse)
  return(sse)
}

calcSSE(0.5)
calcSSE(0.05) #smallest number i've tested that works

optim(par=c(0.5), calcSSE, method="SANN")

optim(par=c(0.5), calcSSE, method="L-BFGS-B", lower = c(0.1), upper = c(0.9))

optim(par=c(0.5), calcSSE, method="L-BFGS-B", lower = c(0.05), upper = c(1))

fit14 = loess(EggDensity ~ AreaHA, data = GP1, span = 0.05)
summary(fit14)
smoothScatter(GP1$AreaHA, GP1$EggDensity)
j = order(GP1$AreaHA)
lines(GP1$AreaHA[j], fit14$fitted[j])

fit15 = loess(EggDensity ~ AreaHA, data = GP1.50, span = 0.05)
summary(fit15)
smoothScatter(GP1.50$AreaHA, GP1.50$EggDensity)
j = order(GP1.50$AreaHA)
lines(GP1.50$AreaHA[j], fit14$fitted[j])

##### best one so far
fit16 = loess(EggDensity ~ AreaHA, data = GP1.50, span = 0.15)
summary(fit16)
smoothScatter(GP1.50$AreaHA, GP1.50$EggDensity)
j = order(GP1.50$AreaHA)
lines(GP1.50$AreaHA[j], fit16$fitted[j])
max(fit16$fitted) #116.0514
fit16$fitted
match(max(fit16$fitted), fit16$fitted) #38336
GP1.50$AreaHA[match(max(fit16$fitted), fit16$fitted)] #3.43 ha

fit17 = loess(EggDensity ~ AreaHA, data = GP1.50, span = 0.10)
summary(fit17)
smoothScatter(GP1.50$AreaHA, GP1.50$EggDensity)
j = order(GP1.50$AreaHA)
lines(GP1.50$AreaHA[j], fit17$fitted[j])

fit18 = loess(EggDensity ~ AreaHA, data = GP1.50, span = 0.075)
summary(fit18)
smoothScatter(GP1.50$AreaHA, GP1.50$EggDensity)
j = order(GP1.50$AreaHA)
lines(GP1.50$AreaHA[j], fit18$fitted[j])

#fractal Ind
fit19 = loess(EggDensity ~ FractalInd, data = GP1, span = 0.6)
summary(fit19)
smoothScatter(GP1$FractalInd, GP1$EggDensity)
j = order(GP1$FractalInd)
lines(GP1$FractalInd[j], fit19$fitted[j])
#looks pretty good

fit19 = loess(EggDensity ~ FractalInd, data = GP1, span = 0.2)
summary(fit19)
smoothScatter(GP1$FractalInd, GP1$EggDensity)
j = order(GP1$FractalInd)
lines(GP1$FractalInd[j], fit19$fitted[j])













