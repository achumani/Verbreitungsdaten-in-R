path <- "/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/"
setwd(path)

library(raster)


getWorldClim <- function(par, res, path){ #par: parameter vector, res: resolution (2.5, 5, 10), path: path character string)
  parlist <-list()
  for (i in 1:length(par)) {
    parlist[[par[i]]] <- getData('worldclim', path=path, download = T, var=par[i], res=res)
  }
  print(parlist)
}

parameters <- getWorldClim(c("tmin") , 2.5, "/Users/Simon/Studium/MSC/Best Practice in R/DistributionProject/")


#download glc
download.file("http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_Dominant.zip&access=private", paste(path, "GLCdom.zip", sep=""))
#unzip glc
unzip(paste(path, "GLCdom.zip", sep=""), exdir = "GLCdom")
file.remove(paste(path, "GLCdom.zip", sep=""))
#load raster
glc <- raster(paste(path, "GLCdom/glc_shv10_DOM.Tif", sep=""))

#load raster
crs(parameters[[1]])

crs(glc) <- crs(parameters[[1]])
plot(glc)
plot(parameters$tmin@layers[[1]], add=T)

ceaex <- projectExtent(glc, CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
cea <- projectRaster(glc, ceaex, c(50000,50000), method="bilinear")


cea

legendglc <- read.csv(paste(path, "GLCdom/glc_shv10_DOM_ReadMe_Legend.txt", sep=""), sep=",", header = F)
