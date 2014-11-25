path <- "/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/"
setwd(path)
require(raster)
require(rgdal)
require(foreign)
require(mapdata)
#####################################
##Download and unzip IUCN species shapefiles 
#mammals
download.file("http://goo.gl/aCwMuk", paste(path, "Mammals.zip", sep=""))
unzip(paste(path, "Mammals.zip", sep=""), exdir = "Mammals")
file.remove(paste(path, "Mammals.zip", sep=""))
#amphibians
download.file("http://goo.gl/wGQo4g", paste(path, "Amphibians.zip", sep=""))
unzip(paste(path, "Amphibians.zip", sep=""), exdir = "Amphibians")
file.remove(paste(path, "Amphibians.zip", sep=""))
#reptiles
download.file("http://goo.gl/y4hyiX", paste(path, "Reptiles.zip", sep=""))
unzip(paste(path, "Reptiles.zip", sep=""), exdir = "Reptiles")
file.remove(paste(path, "Reptiles.zip", sep=""))
# Marine Fish
download.file("http://goo.gl/cWX57I", paste(path, "MarineFish.zip", sep=""))
unzip(paste(path, "MarineFish.zip", sep=""), exdir = "MarineFish")
file.remove(paste(path, "MarineFish.zip", sep=""))

#load species attribute tables, extract IDs and names and write csv file
mammals <- read.dbf(paste(path, "Mammals/MAMMALS.dbf", sep=""))
amphibians <- read.dbf(paste(path, "Amphibians/AMPHIBIANS.dbf", sep=""))
reptiles <- read.dbf(paste(path, "Reptiles/REPTILES.dbf", sep=""))
MarineFish1 <- read.dbf(paste(path, "MarineFish/MARINEFISH_PART1.dbf", sep=""))
MarineFish2 <- read.dbf(paste(path, "MarineFish/MARINEFISH_PART2.dbf", sep=""))

#extract species IDs for further use and write to file
Species_ID <- unique(rbind(mammals[1:2], amphibians[1:2], reptiles[1:2], MarineFish1[2:3], MarineFish2[2:3]))

write.csv(Species_ID, file = paste(path, "IUCN_Species_ID.csv", sep=""))