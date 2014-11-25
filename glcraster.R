
###Landcover data
#download glc
download.file("http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_Dominant.zip&access=private", paste(path, "GLCdom.zip", sep=""))
unzip(paste(path, "GLCdom.zip", sep=""), exdir = "GLCdom")
file.remove(paste(path, "GLCdom.zip", sep=""))
#load raster
crs(glc) <- raster(paste(path, "GLCdom/glc_shv10_DOM.Tif", sep=""))

plot(glc)


extent(Germany)
glc.Germany <- crop(glc, Germany)
crs(glc.Germany) <- cea
ceaex <- projectExtent(glc.Germany, cea)

ceaex.glc.Germany <- projectRaster(glc.Germany, ceaex)


plot(ceaex.glc.Germany, add=T)

plot(glc.Germany)
crs(glc.Germany)

projectRaster(parameters[[1]][[1]], glc.German cea)
# 5. CREATING AN EMPTY GRID (Thanks, Robert Hijmans!)
r <- raster(ceaex.glc.Germany)
res(r) <- 1
r[] <- rnorm(ncell(r))
plot(r)

plot(ceaex.glc.Germany, add=T)
r[] <- 0

glc.raster <- rasterize(ceaex.glc.Germany, r)
#test subset
worldborder <- readOGR(paste(path, "World_Boarders", sep=""), layer="TM_WORLD_BORDERS-0.3")
Germany <- worldborder[worldborder$NAME == "Germany",]
SpainFrance <- worldborder[worldborder$NAME == "Spain"  worldborder$NAME == "France"]
GermanyFrance <- worldborder[worldborder$NAME == "Germany" & worldborder$NAME == "France"]

glc.SpainFrance <- crop(glc, SpainFrance)
glc.GermanyFrance<- crop(glc, GermanyFrance)

plot(SpainFrance)

legendglc <- read.csv(paste(path, "GLCdom/glc_shv10_DOM_ReadMe_Legend.txt", sep=""), sep=",", header = F)

#glc raster bekommt gleiche crs daten
crs(glc) <- cea
res(glc)

Germany <
res(glc)
