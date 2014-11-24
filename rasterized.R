
# Rasterized

# new gridding

library(raster)
library(splancs)
library(rgdal)
library(sp)
library(rgeos)

#path <- "C:/Studium/WS 2014_2015/Best practise R/test"
path <- "C:/Users/Kukuxumusu/Documents/Studium/WS 2014_2015/Best practise R/test"
setwd(path)

damadama <- readOGR(dsn="C:/Users/Kukuxumusu/Documents/Studium/WS 2014_2015/Best practise R/species_42188", layer="species_42188")

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
tmean10_aug_utm <- projectRaster(tmean10_aug, crs="+init=epsg:32632")
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

plot(aoi_r)
plot(aoi_shp, add=T)

nrow(aoi_rshp)



##############

# in this run, we make the AoI Poly from the damadama shp. And then clip everything with that. Should work as well

plot(damadama_utm)
aoi <- getpoly(quiet=F)
ID <- "aoi"
aoi_sp <- Polygon(aoi)
aoi_sps <- Polygons(list(aoi_sp), ID=ID)
aoi_spls <- SpatialPolygons(list(aoi_sps), proj4string=CRS("+init=epsg:32632"))

aoi_shp <- crop(damadama_utm, aoi_spls)
aoi_r <- crop(tmean10_aug_utm, aoi_spls)


aoi_rshp <- rasterize(aoi_shp, aoi_rr)

res(aoi_rshp)
res(aoi_rr)
extent(aoi_rshp)
extent(aoi_rr)

# klappt :)
#
#
# to get all the borders, we need to rasterize the PolyLines (the borders of the Polygons)

aoi_shp_edge <-as(aoi_shp, "SpatialLinesDataFrame")
aoi_rshp_edge <- rasterize(aoi_shp_edge, aoi_rr)
aoi_rshp_all <- merge(aoi_rshp_edge, aoi_rshp)
plot(aoi_rshp_all)


########

# change all the values in the species raster to eiter yes or now (1 or 0 depending on weather the species is present or not)

aoi_rshp_all[!NA] <- 1
plot(aoi_rshp_all)

aoi_rshp_all[!is.na(aoi_rshp_all)] <- 1


# the species raster is now complete
