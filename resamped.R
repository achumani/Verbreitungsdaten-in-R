# Resample Raster (Monday, November 24th)

# take  different input rasters with different resolutions and resample them to the resolution best suitable to the distribution of our species

# unfortunately, there is no function that allows us to specify the new cell size. There are two routes to reach our goal. Either create a template raster with the desired cell size. Or use the aggregate function to reduce the number of cells (make them bigger), bzw. use the disagregate function to reduce the cell size.
# the latter seems more straight forward

# if we want 50x50km, we need to define the resampling factor

res(aoi_rr) #x: 18100 m /cell; y: 18600 m/cell
resX <- 50000/res(aoi_rr)[1]
resY <- 50000/res(aoi_rr)[2]
resampleFactor <- c(resX,resY) # note that inside the function, our resampling factor will be rounded to the closest integer(here it is 3))

aoi_rshp_agg <- aggregate(aoi_rshp_all, fact=resampleFactor, expand=T, na.rm=T)
aoi_rr_agg <- aggregate(aoi_rr, fact=resampleFactor, expand=T, na.rm=T)

plot(aoi_rr_agg)
plot(aoi_rshp_agg, add=T)

# works

# Lets say we have a raster that incorporates all the right attributes regarding extent, resolution and projection. All other raster files can be 'synchronised' with this reference raster using the function spatial.tools::spatial_sync_raster (spatial_sync_raster() in the package "spatial.tools").

# Lets try this by downloading 5 minutes data and 'syncing' them with a 10 minutes data reference raster

getWorldClim(c("tmin", "tmax") , 5, path)
tmin5_aug <- raster(paste0(path,"/wc5/tmin8.bil"), sep="")
tmax5_aug <- raster(paste0(path,"/wc5/tmax8.bil"), sep="")

install.packages("spatial.tools")
library(spatial.tools)

tmin10_aug_aoi_ngb <- spatial_sync_raster(tmin5_aug, aoi_rr, method="ngb")
tmin10_aug_aoi_bil <- spatial_sync_raster(tmin5_aug, aoi_rr, method="bilinear")

plot(tmin10_aug_aoi)
plot(tmin10_aug_aoi_bil,add=T)

# It works. Bilinear interpolation yields slightly less ocean cells/bigger land area.


# another method, one that consumes lots of computing power and takes aaages to compute, is projectRaster_rigorous. It uses an area-based resampling, not the point based (centroid of the raster) resampling used by spatial_sync_raster() or resample(). It works the same as spatial_sync_raster()

tmin10_aug_aoi_narmF <- projectRaster_rigorous(tmin5_aug, aoi_rr, method="mode", na.rm=F)
tmin10_aug_aoi_narmT <- projectRaster_rigorous(tmin5_aug, aoi_rr, method="mode", na.rm=T)

# takes ages. Didnt let it finish, so I dont know if it works well


# finaly, the traditional function resample() requires the target and reference raster to be in the same projection, cover the same extent and so on. SO it doesnt realy pose an advantage, its just one small step that is included in spatial_sync_raster for example.

# preparing (takes long due to high resolution, fills my RAM, sucky...)
tmin5_aug_utm <- projectRaster(tmin5_aug, crs="+init=epsg:32632")
tmin5_aug_utm_aoi <- crop(tmin5_aug_utm, aoi_spls)

# resampling
aoi_5min_to_10min_tmin_aug <- resample(tmin5_aug_utm_aoi, aoi_rr)

plot(aoi_rr)
plot(aoi_5min_to_10min_tmin_aug, add=T)

#################
## conclusion ###

spatial_sync_raster seems the fastest and easiest function to use for resampling a raster

