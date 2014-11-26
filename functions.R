##functions
#-----------------------------------------------------
#download climate parameters global and reproject them
getWorldClim <- function(par, res, path)
  parlist <-list()
  for (i in 1:length(par)) {
    parlist[[par[i]]] <- getData('worldclim', path=path, download = T, var=par[i], res=res)
  }
  print(parlist)
}
#-----------------------------------------------------
#open species data website at IUCN
IUCNdata <- function(name){
  library(RCurl)
  x <- getURL("https://raw.githubusercontent.com/achumani/Verbreitungsdaten-in-R/master/IUCN_Species_ID.csv")
  SpeciesID <- read.csv(text = x, header=T)
  ID <- as.character(SpeciesID$id_no[which(SpeciesID$binomial == name)])
  browseURL(paste("http://maps.iucnredlist.org/map.html?id=", ID, sep=""))
}
IUCNdata(species)

# #----------------------------------
# #DOWNLOAD GLC DATA AND STORE INTO RAWDATA DIRECTORY
# if (file.exists(paste(path.raw, "GLCdom", sep="/"))){
#   stop("File already exists")
# }else{
#   download.file("http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_Dominant.zip&access=private", paste(path.raw, "GLCdom.zip", sep="/"))
#   unzip(paste(path.raw, "GLCdom.zip", sep="/"), exdir = paste(path.raw, "GLCdom", sep="/"))
#   file.remove(paste(path.raw, "GLCdom.zip", sep="/"))
# }

