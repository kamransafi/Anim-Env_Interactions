## SRTM download options for movement data
#srtm15p extracts terrestrial topography and marine bathymetry data from: Tozer, B, Sandwell, D. T., Smith, W. H. F., Olson, C., Beale, J. R., & Wessel, P. (2019). Global bathymetry and topography at 15 arc sec: SRTM15+. Earth and Space Science, 6, 1847– 1864. https://doi.org/10.1029/2019EA000658
#srtm90m extracts terrestrial data from: Jarvis A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled seamless SRTM data V4, International Centre for Tropical Agriculture (CIAT), available from http://srtm.csi.cgiar.org.
#citation: Reuter H.I, A. Nelson, A. Jarvis, 2007, An evaluation of void filling interpolation methods for SRTM data, International Journal of Geographic Information Science, 21:9, 983-1008.

library(RCurl)
library(raster)
srtm15p <- function(x){
  bb <- as.vector(extent(x)*1.5)
  txt <- postForm("https://topex.ucsd.edu/cgi-bin/get_srtm15.cgi",
           submitButton = "get data",
           north=bb[4],
           west=bb[1],
           east=bb[2],
           south=bb[3],
           style = "POST")
  xyz <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(txt, "\n")), "\t"))), ncol=3, byrow=T))
  names(xyz) <- c("long", "lat", "alt")
  coordinates(xyz) <- ~long+lat
  gridded(xyz) <- TRUE
  return(raster(xyz))
}

srtm90m <- function(x, method="wget", quiet=F){
  bb <- as.vector(extent(x)*1.5)
  xtilellc <- ceiling((bb[1]+180)/5)
  ytilellc <- 24 - floor((bb[3]+60)/5)
  xtileurc <- ceiling((bb[2]+180)/5)
  ytileurc <- 24 - floor((bb[4]+60)/5)
  combi <- expand.grid(xtilellc:xtileurc, ytilellc:ytileurc)
  tile <- apply(combi, 1, function(x) paste("srtm_", sprintf("%02d",x[1]), "_" , sprintf("%02d", x[2]), ".zip", sep=""))
  tileURL <- lapply(tile, function(tn) paste("http://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/", tn, sep=""))
  demsDL <- mapply(function(URL, tileName) download.file(URL, paste(tempdir(), tileName, sep="/"), method=method, quiet=quiet), tileURL, tile)
  unzip(paste(tempdir(), tile, sep="/"), exdir=tempdir())
  dems <- lapply(list.files(tempdir(), pattern=".tif", full.names = T), raster)
  # Merge the tiles into a single raster
  if(length(dems)>1){
    DEM <- do.call("merge", dems)}else{DEM <- dems[[1]]}
  # Reduce to the extent of the study area
  DEM <- crop(DEM, bb)
  return(DEM)
}

# Example code with plots
library(move)
library(scales)
Dem15Leroy <- srtm15p(leroy)
Dem90mLeroy <- srtm90m(leroy, quiet=T)
proj4string(Dem15Leroy) <- projection(leroy)
proj4string(Dem90mLeroy) <- projection(leroy)

slope15 <- terrain(Dem15Leroy, opt='slope')
aspect15 <- terrain(Dem15Leroy, opt='aspect')
hill15 <- hillShade(slope15, aspect15, 40, 270)

slope90m <- terrain(Dem90mLeroy, opt='slope')
aspect90m <- terrain(Dem90mLeroy, opt='aspect')
hill90m <- hillShade(slope90m, aspect90m, 40, 270)

ColRamp <- terrain.colors(512)

plot(leroy,  type="n", xaxt="n", yaxt="n", ylab=NA, xlab=NA, bty="n")
plot(crop(hill15, extent(leroy)*1.1), col=alpha(grey(0:512/519), 1), add=T, legend=FALSE, main='')
plot(crop(Dem15Leroy, extent(leroy)*1.1), col=alpha(ColRamp, 0.4), add=T, legend=FALSE)
lines(leroy, col=alpha(c("black"), 1), lwd=1.5)
lines(leroy, col=alpha(c("white"), 1), lwd=1)
lines(leroy, col=alpha(c("purple"), 0.75), lwd=1.25)

plot(leroy,  type="n", xaxt="n", yaxt="n", ylab=NA, xlab=NA, bty="n")
plot(crop(hill90m, extent(leroy)*1.1), col=alpha(grey(0:512/519), 1), add=T, legend=FALSE, main='')
plot(crop(Dem90mLeroy, extent(leroy)*1.1), col=alpha(ColRamp, 0.4), add=T, legend=FALSE)
lines(leroy, col=alpha(c("black"), 1), lwd=1.5)
lines(leroy, col=alpha(c("white"), 1), lwd=1)
lines(leroy, col=alpha(c("purple"), 0.66), lwd=1.25)

