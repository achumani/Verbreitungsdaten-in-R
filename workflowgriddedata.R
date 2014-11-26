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

#---------------------------------
#SET CUSTUM PARAMETERS

setwd("/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/")
species <- "Martes foina"
parameters <- c("alt")
target_resolution <- 50000
crs <- "+init=epsg:32631"

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
  library(RCurl)
  x <- getURL("https://raw.githubusercontent.com/achumani/Verbreitungsdaten-in-R/master/IUCN_Species_ID.csv")
  SpeciesID <- read.csv(text = x, header=T)
  ID <- as.character(SpeciesID$id_no[which(SpeciesID$binomial == name)])
  browseURL(paste("http://maps.iucnredlist.org/map.html?id=", ID, sep=""))
  paste("CAUTION: Make sure to move the species folder into this folder: getwd()/grid_data/raw")
  print(ID)
}

#----------------------------------
#######################
####DOWNLOADING RAW DATA###
#######################

# common parameters included are derived from the WorldClim database, the global landcover data provided by the UNFAO GeoNetwork and a species distribution dataset from the IUCN redlist.

# #----------------------------------
# #SET PROJECT FILES DIRECTORY
# unlink(path.grid_data, recursive=T)
path.grid_data <- paste(getwd(), "grid_data", sep="/")
path.raw <- paste(path.grid_data, "raw", sep="/")
path.processed <- paste(path.grid_data, "processed", sep="/")
# dir.create(path.grid_data)
# dir.create(path.raw)
# dir.create(path.processed)

#----------------------------------
#IUCN-DATA LINK
ID <- IUCNdata(species) #download species file as is into rawdata

#----------------------------------
#DOWNLOAD WorldClim DATA INTO RAWDATA FILE

par <- parameters
if(target_resolution < 20000){
  res <- 5
}else{
  res <- 10
}

getWorldClim(par, res)

#----------------------------------
#WORLD BORDERS
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip", paste(path.raw, "WorldBorders.zip", sep="/"))
# unzip(paste(path.raw, "WorldBorders.zip", sep="/"), exdir = paste(path.raw, "WorldBorders", sep="/"), overwrite=T)
# file.remove(paste(path.raw, "WorldBorders.zip", sep="/"))
worldborders <- readOGR(dsn=paste(path.raw, "WorldBorders", sep="/"), layer="TM_WORLD_BORDERS-0.3")
#######################
####shapefile laden###
#######################
species.shp <- readOGR(dsn=paste(paste(path.raw, "species_", sep="/"), ID, sep=""),layer=paste("species_", ID, sep=""))
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
dimx <- (round(length(ext@xmin:ext@xmax)/target_resolution))
dimy <- (round(length(ext@ymin:ext@ymax)/target_resolution))

# build a template raster for the spatial_sync_raster function
aoi_rr_utm <- raster(ext, ncols=dimx,nrows=dimy,crs=crs)
vals <- 1:ncell(aoi_rr_utm)
aoi_rr_utm <- setValues(aoi_rr_utm, vals)
plot(species_aoi_edge_raster)
plot(species_aoi_filling_raster, add=T)
plot(species_aoi_rr_utm)
# use aoi_rr to clip species shapefile and rasterize it
# to cover full extent of species, the polygons borders need to be rasterized seperatly. Otherwise only what is 100% inside the polygon will be rasterized. But it is not 100% perfect. possible improvements!!
species_aoi_shp <- crop(species.shp, aoi_ext)
species_aoi_shp_utm <- spTransform(species_aoi_shp, crs(crs))
#test <- gdal_rasterize(species_aoi_shp_utm, aoi_rr_utm, at=T)
species_aoi_edge <- as(species_aoi_shp_utm, "SpatialLinesDataFrame")
species_aoi_edge_raster <- rasterize(species_aoi_edge, aoi_rr_utm)
species_aoi_filling_raster <- rasterize(species_aoi_shp_utm, aoi_rr_utm)
species_aoi_rr_utm <- merge(species_aoi_edge_raster, species_aoi_filling_raster)
plot(species_aoi_rr_utm, add=T)
plot(species_aoi_shp_utm, add=T)
plot(aoi_rr_utm, add=T)
rasterlist <- list.raster.files(paste(paste(path.raw,"wc", sep="/"),res, sep=""), pattern= "bil$") # this function will check the folder and subfolders at the paths location for ALL rasters
names <- list.files(paste(paste(path.raw, "wc",sep="/"), res, sep=""), pattern= "bil$") #stores rasternames in same order as list.raster.files()
r <- list() # empty list just to get the loop going
for (i in 1:length(rasterlist$raster_files)){
  r[[i]] <- raster(rasterlist$raster_files[[i]], sep="") #loading longlat rasters
  rr <- spatial_sync_raster(r[[i]], aoi_rr_utm, method="bilinear") #use aoi_rr_utm as input for spatial sync
  name <- paste("processed_", names[[i]], sep="")
  writeRaster(rr, filename=paste(path.processed, name, sep="/"), format="EHdr", overwrite=T) # format can be 'GTif', or others, check documentation at http://www.inside-r.org/packages/cran/raster/docs/writeRaster
}

test <- raster(paste(path.processed, "processed_tmin1.bil", sep="/"))

plot(test, add=T)
