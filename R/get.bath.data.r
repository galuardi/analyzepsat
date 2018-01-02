#' get.bath.data
#' Download bathymetry
#'
#' @param lonlow western most longitude -180 to 180
#' @param lonhigh eastern most longitude -180 to 180
#' @param latlow southern most latitude 
#' @param lathigh northern most latitude 
#' @param folder defaults to \code{tempdir()}
#' @param seaonly defaults to \code{T}
#' @param res resolution of desired product. Currently, 1 degree and .5 degree are supported (see details)
#'
#' @return a list of lon lat and bathymetry data
#' @export
#' @details Products supported are: Smith & Sandwell v9.1, 1/60-degree \n UCSD   (Dataset ID: usgsCeSS91) and SRTM30+ SRTM30_PLUS Estimated Topography, 30 seconds, Global, v1 \n \tScripps   (Dataset ID: srtm30plus_LonPM180)
#' @references \url{http://www.ngdc.noaa.gov/mgg/global/global.html}
#' \url{http://coastwatch.pfeg.noaa.gov/erddap/info/usgsCeSrtm30v6/index.html}
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
#' Becker, J. J., D. T. Sandwell, W. H. F. Smith, J. Braud, B. Binder, J. Depner, D. Fabre, J. Factor, S. Ingalls, S-H. Kim, R. Ladner, K. Marks, S. Nelson, A. Pharaoh, G. Sharman, R. Trimmer, J. vonRosenburg, G. Wallace, P. Weatherall., Global Bathymetry and Elevation Data at 30 Arc Seconds Resolution: SRTM30_PLUS, revised for Marine Geodesy, January 20, 2009
#' @author Benjamin Galuardi


#' @examples
#' 
#' # nice blue cascade for bathymetry
#' bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

#' #***OR****#
#' # bath.colors <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteel#' blue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))
#' library(fields)
#' bath1 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 1)
#' bath.5 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = .5)

#' # plot 
#' par(mfrow=c(1,2))
#' par(mar=c(8,4,4,4))
#' image(bath1[[1]],(bath1[[2]]),t(bath1[[3]]),col=bath.colors(100), zlim=c(-10000,0), xlab = '', ylab='')
#' image(bath.5[[1]],(bath.5[[2]]),t(bath.5[[3]]),col=bath.colors(100), zlim=c(-10000,0), xlab = '', ylab='')

#' # add a scale bar
#' ticks <- c(-10000,-8000,-6000,-4000,-2000,-1000,-500,0)
#' par(cex=0.9)
#' image.plot(matrix(1), zlim=c(-10000,0), horizontal=T, col = bath.colors(100),
#'					 axis.args=list(at=ticks,labels=ticks*-1), legend.shrink = 0.8, 
#'					 legend.args=list(text="  Bottom \n depth (m)", cex=0.8, side=4, line=1, las=1),
#'					 legend.only=T)
#' par(cex=1)

get.bath.data <- function(lonlow, lonhigh, latlow, lathigh, folder = tempdir(), seaonly = T, res = c(.5,1)){
	require(ncdf4)
	
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
		opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSS111.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]"
		bathid = "topo"
	}
	if (res == 0.5) {
		cat("ERDDAP downloading: Topography, SRTM30+ SRTM30_PLUS Estimated Topography, 30 seconds, Global, v1 \n \tScripps   (Dataset ID: srtm30plus_LonPM180)")
		opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/srtm30plus_LonPM180.nc?z[(LATLOW):(LATHIGH)][(LONLOW):(LONHIGH)]"
		bathid = "z"
	}
	opt <- sub("LATLOW", latlow, opt)
	opt <- sub("LATHIGH", lathigh, opt)
	opt <- sub("LONLOW", lonlow, opt)
	opt <- sub("LONHIGH", lonhigh, opt)
	
	download.file(opt, fname, mode = "wb")
	
	nc <- nc_open(fname)
	lon <- as.numeric(ncvar_get(nc, varid="longitude"))
	lat <- as.numeric(ncvar_get(nc, varid="latitude"))
	bdata = ncvar_get(nc, varid=bathid)
	if(res==.5)bdata=t(fliplr(bdata))
	bdata = rot90(bdata)
	lat = lat[order(lat)]
	if(seaonly==T) bdata[bdata>=0] = 1
	bathy = list(lon = lon, lat = lat, data = bdata)
	bathy
}