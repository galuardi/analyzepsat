# Download bathymetry
# res is either 30 second, one or two minute resolution (.5, 1, 2)

get.bath.data <- function(lonlow, lonhigh, latlow, lathigh, folder = tempdir(), seaonly = T, res = c(.5,1)){
   require(ncdf)
  
  rot90 <- function(A) {
	n <- dim(A)[2]
	A <- t(A)
	A[n:1, ]
}

fliplr <- function(A){
	A = (A)[(ncol(A)):1,]
	A
}

fname = paste(folder, "request.nc", sep = "/")
    if (res == 1) {
        cat("ERDDAP downloading: Topography, Smith & Sandwell v9.1, 1/60-degree \n UCSD   (Dataset ID: usgsCeSS91)")
        opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSS91.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]&.draw=surface&.vars=longitude|latitude|topo&.colorBar=|||||&.land=over"
        bathid = "topo"
    }
    if (res == 0.5) {
        cat("ERDDAP downloading: Topography, SRTM30+ Version 1.0, 30 arc second, Global \n \tScripps   (Dataset ID: usgsCeSrtm30v6)")
        opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSrtm30v6.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]&.draw=surface&.vars=longitude|latitude|topo&.colorBar=|||||"
        bathid = "topo"
    }
opt <- sub("LATLOW", latlow, opt)
opt <- sub("LATHIGH", lathigh, opt)
opt <- sub("LONLOW", lonlow, opt)
opt <- sub("LONHIGH", lonhigh, opt)
  
download.file(opt, fname, mode="wb")

nc <- open.ncdf(fname)
lon <- as.numeric(get.var.ncdf(nc, varid="longitude"))
lat <- as.numeric(get.var.ncdf(nc, varid="latitude"))
bdata = get.var.ncdf(nc, varid=bathid)
# if(res==.5)bdata=t(fliplr(bdata))
if(res==1)bdata = rot90(bdata)
if(res==.5)bdata = rot90(bdata)	
lat = lat[order(lat)]
if(seaonly==T) bdata[bdata>=0] = 1
bathy = list(lon = lon, lat = lat, data = bdata)
bathy
}

# par(mfrow=c(1,3))
# bath2 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 2)
# image(bath2[[1]],(bath2[[2]]),t(bath2[[3]]),col=bath.colors, zlim=c(-1000,0))
# bath3 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 1)
# image(bath3[[1]],(bath3[[2]]),t(bath3[[3]]),col=bath.colors, zlim=c(-1000,0))
# bath4 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = .5)
# image(bath4[[1]],(bath4[[2]]),t(bath4[[3]]),col=bath.colors, zlim=c(-1000,0))

