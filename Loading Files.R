#------------------------------------------
#LOAD PACKAGES AND PREPARE WORKING SPACE
library(raster)

path <- "/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/"
setwd(path)

cea <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m"

#------------------------------------------
#LOAD PARAMETERS
parameters <- getWorldClim(c("tmin", "tmax", "alt") , 10, path)

#------------------------------------------
#OPEN IUCN SPECIES WEBSITE PROMPTING DOWNLOAD LINK
IUCNdata("Cervus elaphus")

