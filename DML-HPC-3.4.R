# Rscript for batch run on ISU HPC machines

library(dplyr)

scen3.4EZ = read.csv("CumEggsPerZone.2021.Mar.20.10_57_04.txt")
sum(scen3.4EZ$Eggs) 

length(scen3.4EZ$run)/100 #868,617, the number of polgyons in the shapefile

#combine instances - run code below for each map
Denresults = data.frame(matrix(nrow=868617, ncol=6))
colnames(Denresults) = c("PolygonID","GISPolyID","HabType","CumEggs","PolygonArea","ProbEggs")

dens = scen3.4EZ
nrow(dens) #should be 868,617*100=

#loop through dens to create object with densities for each polygon - ~13 days for DML on my desktop laptop
system.time(
  for(i in 1:868617)
  {
    densi = filter(dens, ID == i) #combine all instances for each polygon
    Denresults[i,1] = densi[1,3] #polygon ID number
    Denresults[i,2] = densi[1,8] #GISPolyID from shapefile
    Denresults[i,3] = as.character(densi[1,4]) #habitat type
    Denresults[i,4] = sum(densi$Eggs) #sum across the 100 instances to get total eggs for per polygon
    Denresults[i,5] = densi[1,6] #polygon area - sometimes lat/long, depends on shapefile
    Denresults[i,6] = densi[1,5] #probEggs
    #print index
    print(i)
  }
)

save(Denresults, file="DMLDenresults_3.4.RData")








