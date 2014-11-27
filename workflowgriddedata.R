
#What does this script do?

# - it aims to create a final data sheet, in which various environmetnal parameters and the distribution of one (or more) species in an area of interest are assigned to spatial equal-area-cells of a given size. The data are the foundation for macro-ecological research, species distribution modelling and so forth.

# 1) First, the following parameters are specified by the user:
#     - a working directory in which the project directories are saved
#     - the IUCN-listed species targeted for analyses
#     - the worldClim parameters the user wishes to look at (tmin, tmax, tmean, prec, alt)
#     - the target resoultion of the grid
#     - an equal area coordinate reference system covering the area of interest (recommended: UTM-systems)

# 2) The previously specified parameter raw data are then loaded into our folder structure

# 3) A link to the target species IUCN online data sheet with download link (direct download was not possible unfortunately). At this stage it may make sense to just download the entire species file we need becaus that is a direct download -> it takes longer but the workflow is more smooth, with less user interaction and we can also look at several species at once

# 4) Rasterization: interactive clip a polygon from a map that also shows the species distribution. We use the box surrounding the polygon as templete for our raster.
# Necessary steps:
#      - select AOI
#     - use AOI-surrounding box, projected in the specified UTM, as raster template
#     - build empty raster of specified target resolution
#     - rasterize species distribution into the given raster
#     - rasterize WorldClim data into the empty raster
# 5) Final data sheet:
# this has columns with UTM coordinates of our cells, the values of their climate parameters
# still missing but easy to complete: glc-data

#######################
######PREPARATION######
#######################

#LOAD NECESSARY PACKAGES
library(rgdal)
library(sp)
library(splancs)
library(rgeos)
library(spatial.tools)
library(raster)
library(foreign)
library(RCurl)
#---------------------------------
#SET CUSTOM PARAMETERS

setwd("/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/")
species <- c("Crocodylus siamensis") 
parameters <- c("alt", "tmin", "tmax")
target_resolution <- 50000
crs <- "+init=epsg:32648" #
deletefiles <- T #(delete proccessed and downloaded files after running workflow, T or F)
#----------------------------------
#LOAD WORKFLOW FUNCTIONS
#Download WorldClim
getWorldClim <- function(par, res){
  parlist <-list()
  for (i in 1:length(par)) {
    getData('worldclim', path=path.raw, download = T, var=par[i], res=res)
  }
}

#Prompt species distribution shp download website at IUCN
IUCNdata <- function(name){
  browseURL(paste("http://maps.iucnredlist.org/map.html?id=", ID, sep=""))
  paste("CAUTION: Make sure to move the species folder into your working directory")
}

#round numbers to specific multiple (by Alberto Santini: https://gist.github.com/albertosantini/3638434)
mround <- function(number, multiple) {
  # if number and multiple have different sign, returning an error.
  if (sign(number) != sign(multiple)) {
    stop("number and multiple have different sign")
  }
  n = number / multiple
  if (abs(n - trunc(n)) == 0.5) {
    if (sign(n) > 0) {
      n = n + 0.1
    } else {
      n = n - 0.1
    }
  }
  round(n) * multiple
}
#----------------------------------
###########################
####DOWNLOADING RAW DATA###
###########################

# common parameters included are derived from the WorldClim database, the global landcover data provided by the UNFAO GeoNetwork and a species distribution dataset from the IUCN redlist.

# #----------------------------------
# #SET PROJECT FILES DIRECTORY
path.temp_data <- paste(getwd(), "temp_data", sep="/")
unlink(path.temp_data, recursive=T)
path.raw <- paste(path.temp_data, "raw", sep="/")
dir.create(path.temp_data)

#----------------------------------
#IUCN-DATA LINK
#get species ID from online resource
x <- getURL("https://raw.githubusercontent.com/achumani/Verbreitungsdaten-in-R/master/IUCN_Species_ID.csv")
SpeciesID <- read.csv(text = x, header=T)
ID <- as.character(SpeciesID$id_no[which(SpeciesID$binomial == species)])

#promt download link
if (file.exists(paste(getwd(), paste("species_", ID, sep=""), sep="/"))){
  message("File does not have to be downloaded, as it already exists in your working directory")
}else{
  IUCNdata(species) #download species file as is into rawdata
}

#----------------------------------
#DOWNLOAD WorldClim DATA INTO RAWDATA FILE

if (file.exists(paste(path.raw, paste("wc", res, sep=""), sep="/"))){
  message("Files already exist, download not necessary")
}else{
  par <- parameters
  if(target_resolution < 20000){
    res <- 5
  }else{
    res <- 10
  }
  getWorldClim(par, res)
}

#----------------------------------
# DOWNLOAD WORLD BORDERS
if (file.exists(paste(path.raw, "WorldBorders", sep="/"))){
  message("Files already exist, download not necessary")
}else{
  download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip", paste(path.raw, "WorldBorders.zip", sep="/"))
  unzip(paste(path.raw, "WorldBorders.zip", sep="/"), exdir = paste(path.raw, "WorldBorders", sep="/"), overwrite=T)
  file.remove(paste(path.raw, "WorldBorders.zip", sep="/"))
}

worldborders <- readOGR(dsn=paste(path.raw, "WorldBorders", sep="/"), layer="TM_WORLD_BORDERS-0.3")

#######################
#####RASTERIZATION#####
#######################
species.shp <- readOGR(dsn=paste(paste(getwd(), "species_", sep="/"), ID, sep=""),layer=paste("species_", ID, sep=""))

plot(species.shp, col="brown") # from iucnredlist.org transformed to suitable equal area projection
plot(worldborders, add=T)
aoi <- getpoly(quiet = F) # draw the extent of your area of interest
aoi_sp <- Polygon(aoi)
aoi_sps <- Polygons(list(aoi_sp),ID=ID)
aoi_spls <- SpatialPolygons(list(aoi_sps), proj4string=CRS(species.shp@proj4string@projargs))
#aoi_spls_utm <-spTransform(aoi_spls, CRS(crs))

# get the extent of the aoi_spls
aoi_ext <- gEnvelope(aoi_spls)
aoi_ext_utm <- spTransform(aoi_ext, crs(crs))
ext <- extent(aoi_ext_utm)

#force grid to be a multiple of our resoultion in order to get integer values for our dimensions
dimx <- (mround(length(ext@xmin:ext@xmax), target_resolution))/target_resolution
dimy <- (mround(length(ext@ymin:ext@ymax), target_resolution))/target_resolution

# build a template raster for the spatial_sync_raster function
aoi_rr_utm <- raster(ext, ncols=dimx,nrows=dimy,crs=crs)
#vals <- 1:ncell(aoi_rr_utm)
#aoi_rr_utm <- setValues(aoi_rr_utm, vals)

# use aoi_rr to clip species shapefile and rasterize it
# to cover full extent of species, the polygons borders need to be rasterized seperatly. Otherwise only what is 100% inside the polygon will be rasterized. But it is not 100% perfect. possible improvements!!
species_aoi_shp <- crop(species.shp, aoi_ext)
species_aoi_shp_utm <- spTransform(species_aoi_shp, crs(crs))
species_aoi_raster <- rasterize(species_aoi_shp_utm, aoi_rr_utm, getCover=T)
species_aoi_raster@data@values <- species_aoi_raster@data@values/100
species_aoi_raster@data@names <- species
#fitting downloaded worldclim data into raster#

rasterlist <- list.raster.files(paste(paste(path.raw,"wc", sep="/"),res, sep=""), pattern= "bil$") # this function will check the folder and subfolders at the paths location for ALL rasters
names <- list.files(paste(paste(path.raw, "wc",sep="/"), res, sep=""), pattern= "bil$") #stores rasternames in same order as list.raster.files()

wc_processed <- list()
# empty list to store processed rasters in RAM
for (i in 1:length(rasterlist$raster_files)){
  r <- raster(rasterlist$raster_files[[i]], sep="") #loading longlat rasters
  rc <- crop(r, aoi_ext)
  rc_utm <- projectRaster(rc, crs=crs)
  wc_processed[[i]] <- resample(rc_utm, aoi_rr_utm, method="bilinear")
  #use aoi_rr_utm as input for spatial sync
  name <- paste("processed_", names[[i]], sep="")
}

plot(wc_processed[[1]])
plot(species_aoi_raster)
#make data frame with climate attributes, coordinates and species presence/absence
datastack <- stack(wc_processed)
datastack <- addLayer(datastack, species_aoi_raster)
data_sheet <- as.data.frame(rasterToPoints(datastack))
colnames(data_sheet)[1:2] <- c("EOFORIGIN", "NOFORIGIN")
data_sheet <- cbind(ID= ID, CELLCODE = paste0(target_resolution/1000,"KM","E", round(data_sheet$EOFORIGIN/1000), "N", round(data_sheet$NOFORIGIN/1000)),data_sheet)
write.csv(data_sheet, paste0("species_", ID,"_", target_resolution/1000, "KM_data.csv"))

if (deletefiles == T){
  unlink(paste(getwd(), "grid_data", sep="/"))
  message("folder structure deleted from working directory")
}else{
  message("folder structure kept in working directory")
}

