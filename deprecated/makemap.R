makemap <-
function(xmin=-100,xmax=0,ymin=20,ymax=65,mapdata="C:/Ben/DATA/coast/gshhs/gshhs_i.b"){
	# require(maptools)
	gshhs.i.b <-mapdata
	pxlim <- c(xmin,xmax)+360
	pylim <- c(ymin,ymax)
	map2 <- Rgshhs(gshhs.i.b, xlim=pxlim, ylim=pylim)
	#projstr="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
	proj4string(map2$SP) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "  #"+proj=longlat +ellps=clrk66"
	map2
}

