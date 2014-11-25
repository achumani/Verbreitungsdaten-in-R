# requiered packages

install.packages("rgdal")
install.packages("splancs")
install.packages("raster")
install.packages("spatial.tools")

library(rgdal)
library(sp)
library(splancs)
library(rgeos)
library(raster)
library(spatial.tools)

#########################################
# codeblock to get iucnredlist shapefile#
#########################################

#########################################
# codeblock to get all the raster files #
#########################################

# create template raster
plot(species_utm) # from iucnredlist.org transformed to suitable equal area projection
aoi <- getpoly(quite = F) # draw the extent of your area of interest

# get the extent of the aoi
ext <- extent(aoi[2,1], aoi[4,1], aoi[2,2], aoi[4,2])
dimx <- (length(ext[1]:ext[2])/50000)
dimy <- (length(ext[3]:ext[4])/50000)

# build a template raster for the spatial_sync_raster function
aoi_rr <- raster(ext,ncol=dimx,nrow=dimy,crs="+init=epsg:32632")
vals <- 1:ncell(aoi_rr)
aoi_rr <- setValues(aoi_rr, vals)

# use aoi_rr to clip species shapefile and rasterize it
# to cover full extent of species, the polygons borders need to be rasterized seperatly. Otherwise only what is 100% inside the polygon will be rasterized.
species_aoi_shp <- crop(species_utm, aoi_rr)
species_aoi_edge <- as(species_aoi_shp, "SpatialLinesDataFrame")
species_aoi_edge_raster <- rasterize(species_aoi_edge, aoi_rr )
species_aoi_filling_raster <- rasterize(species_aoi_shp, aoi_rr)
apecies_aoi_rr <- merge(species_aoi_edge_raster, species_aoi_filling_raster)

# process the worldclim etc. rasterdata

rasterlist <- list.raster.files(paste0(path,"/wc5")) # this function will check the folder and subfolders at the paths location for ALL rasters

r <- list() # empty list just to get the loop going
for (i in 1:length(rasterlist$raster_files)){
  r[i] <- raster(rasterlist$raster_files[[i]], sep="")
  rr <- spatial_sync_raster(r[[i]], aoi_rr)
  name <- paste0("output/resampled", i,".bil")  # hopefuly 'output/...' will create a subfolder called 'output'. Not tested yet though ...
  writeRaster(rr, filename=name, format="EHdr") # format can be 'GTif', or others, check documentation at http://www.inside-r.org/packages/cran/raster/docs/writeRaster
}
