# get polygon of Area of Interest

install.packages("rgeos")
install.packages("rgdal")
install.packages("splancs")
install.packages("raster")
library(raster)
library(splancs)
library(rgdal)
library(sp)

# get worldclim data
# path <- "C:/Users/Kukuxumusu/Documents/Studium/WS 2014_2015/Best practise R/test"
path <- "C:/Studium/WS 2014_2015/Best practise R/test"
setwd(path)

getWorldClim <- function(par, res, path){ #par: parameter vector, res: resolution (2.5, 5, 10), path: path character string)
  parlist <-list()
  for (i in 1:length(par)) {
    parlist[[par[i]]] <- getData('worldclim', path=path, download = T, var=par[i], res=res)
  }
  print(parlist)
}

parameters <- getWorldClim(c("tmin") , 10, path) # get 10degreeminutes minimum temperature data from worldclim.org
tmin10_aug <- raster(paste0(path,"/wc10/tmin8.bil"), sep="") # load raster into r
# plot(tmin10_aug) # plot the raster


# change raster projection predefined espg
# Raster with right epsg
tmin10_aug_utm <- projectRaster(tmin10_aug, crs="+init=epsg:32632")
plot(tmin10_aug_utm) 


# subset data with area of interest polygon

# AoI zu SpatialPolygons in UTM32
aoi <- getpoly(quiet=FALSE) # draw polygon of aoi, plot needs to be open
ID <- "aoi"
aoi_sp <- Polygon(aoi)
aoi_sps <- Polygons(list(aoi_sp), ID=ID)
aoi_spls <- SpatialPolygons(list(aoi_sps), proj4string=CRS("+init=epsg:32632"))

# cliping mit mask(), not recomended
# clipped <- mask(tmin10_aug_utm, aoi_spls)
# plot(clipped) # same extent, many NAs

# cliping mit crop(), recomended
clipped <- crop(tmin10_aug_utm, aoi_spls)
plot(clipped) # besser als mit mask()


writeRaster(clipped, filename="clipped.tif", format="GTiff", overwrite=T)


