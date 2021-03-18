# This function uses the SF package to rasterize lines faster than the raster package
# What it also takes care of is that raster cells that are touched are also considered
# When plotting very high res rasters, make sure that you choose maxpixels in plot::raster deliberately. 

library(move)
library(fields)
library(sf)
library(stars)
library(data.table)

Rastrack <- function(x, grid){
  ls <- st_sf(a = 1, st_sfc(st_linestring(coordinates(x))), crs=CRS("+proj=longlat +datum=WGS84"))
  tmp <- st_rasterize(ls, grd, options="ALL_TOUCHED=TRUE")
  return(tmp)
  message(paste("Done with ", x@idData$local_identifier, ".", sep=""))
}

#Example with a large stork data from the movebank repository
download.file("https://www.datarepository.movebank.org/bitstream/handle/10255/move.418/MPIO%20white%20stork%20lifetime%20tracking%20data%20%282013-2014%29-gps.csv.zip?sequence=3", paste(tempdir(), "storks.zip", sep="/"))
StorkData <- unzip(paste(tempdir(), "storks.zip", sep="/"), exdir=tempdir())
Storks <- fread(StorkData[1])
names(Storks) <- gsub("-", ".", names(Storks))
#create a list of move objects
StorksMove <- lapply(unique(Storks$individual.local.identifier), function(ID) { 
  tmp <- Storks[Storks$individual.local.identifier==ID,]
  mtmp <- move(x=tmp$location.long, y=tmp$location.lat, time=as.POSIXct(tmp$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
       animal=ID, data=tmp, proj=CRS("+proj=longlat +ellps=WGS84"))
  return(mtmp)
  })
# briefly stack to obtain the extent of the data to define the raster for rasterizing
StorkStack <- moveStack(StorksMove) 
studyArea <- st_bbox(extent(StorkStack), crs=CRS("+proj=longlat +datum=WGS84"))
# make a grid which will be the raster with which the tracks will be rasterized
grd <- st_as_stars(studyArea, dx = 0.08, dy = 0.08, values = 0)
#sf_extSoftVersion()["GDAL"]
# dump the large files not needed anymore
rm(Storks)
rm(StorkStack)

# rasterize the tracks
# the function takes a move object. MoveStack objects would need to be unstacked first
# alternatively the function could be extended to recognise the type of move object and 
# accordingly accomodate move or moveStacks
StorkRasters <- lapply(StorksMove, function(x) {Rastrack(x, grid = grd)})

# Sum the rasters
sumRas <- StorkRasters[[1]]
for(i in 2:length(StorkRasters)){
  sumRas <- sumRas+StorkRasters[[i]]
  print(i)
}
# replace 0 with NA for plotting
sumRas[sumRas==0] <- NA
# convert to a raster object for plotting
sumRas <- as(sumRas, "Raster")

plot(sumRas, col=tim.colors(24), axes=F, border=NA, legend.cex=3)
