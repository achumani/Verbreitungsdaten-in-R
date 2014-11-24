# Rasterized

# new gridding

library(raster)
library(splancs)
library(rgdal)
library(sp)
library(rgeos)

path <- "C:/Studium/WS 2014_2015/Best practise R/test"
setwd(path)

damadama <- readOGR(dsn="C:/Studium/WS 2014_2015/Best practise R/species_42188", layer="species_42188")

damadama_utm <- spTransform(damadama, CRS("+init=epsg:32632"))

getWorldClim <- function(par, res, path){ #par: parameter vector, res: resolution (2.5, 5, 10), path: path character string)
  parlist <-list()
  for (i in 1:length(par)) {
    parlist[[par[i]]] <- getData('worldclim', path=path, download = T, var=par[i], res=res)
  }
  print(parlist)
}

parameters <- getWorldClim(c("tmean") , 10, path)
tmean10_aug <- raster(paste0(path,"/wc10/tmean7.bil"), sep="")
tmean10_aug_utm <- projectRaster(tmin10_aug, crs="+init=epsg:32632")
plot(tmean10_aug_utm)

aoi <- getpoly(quiet=FALSE) # draw polygon of aoi, plot needs to be open
ID <- "aoi"
aoi_sp <- Polygon(aoi)
aoi_sps <- Polygons(list(aoi_sp), ID=ID)
aoi_spls <- SpatialPolygons(list(aoi_sps), proj4string=CRS("+init=epsg:32632"))

# clip raster

aoi_r <- crop(tmean10_aug_utm, aoi_spls)

# clip shapefile
aoi_shp <- crop(damadama_utm, aoi_r)

# rasterize shapefile
aoi_rshp <- rasterize(aoi_shp, aoi_r)
