#download glc
download.file("http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_Dominant.zip&access=private", paste0(path, "GLCdom.zip", sep=""), mode="wb")
unzip(paste0(path, "GLCdom.zip", sep=""), exdir = "GLCdom")
file.remove(paste(path, "GLCdom.zip", sep=""))
#load raster
glc <- raster(paste(path, "GLCdom/glc_shv10_DOM.Tif", sep=""))

glc_aoi <- spatial_sync_raster(glc, aoi_rr)
plot(glc_aoi)