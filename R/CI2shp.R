#' CI2shp
#'
#' @param track
#' @param fname
#' @param level
#' @param npoints
#' @param proj4string
#'
#' @return
#' @export
#'
#' @examples
CI2shp <- function(track, fname = 'testshp', level = .95, npoints = 100, proj4string= CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")){
	my.ellipse <- function(track, irow = 1, level = .95, npoints = 100){
		print(paste('making polygon #', irow))
		point = as.numeric(track[irow,])
		x = matrix(point[4:7],2,2)
		centre = c(point[8:9])
		e1 = ellipse(x, scale = c(1, 1), centre = centre, level = level, t = sqrt(qchisq(level, 2)), which = c(1, 2), npoints = npoints)
		e1 = rbind(e1, e1[1,])
		e1
	}

	ndays = dim(track)[1]
	# Polygons
	all.ps = lapply(1:ndays, function(i) Polygons(list(Polygon(my.ellipse(track,i, level = level, npoints = npoints))),i))
	row.names(track) = names(all.ps) = 1:ndays 		# make sure names match for shapefile creation..
	# Spatial polygons
	all.sp = SpatialPolygons(all.ps, proj4string= proj4string)
	# Spatial points data frame
	all.spdf = SpatialPolygonsDataFrame(all.sp, data = track, match.ID = T)
	# Shapefile
	writePolyShape(all.spdf, fname, factor2char = TRUE, max_nchar=254)
}

#' track2shp
#'
#' @param track
#' @param fname
#' @param proj4string
#'
#' @return
#' @export
#'
#' @examples
track2shp <- function(track, fname = 'testshp', proj4string =CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")){
coords = coordinates(track[,8:9])
out = SpatialPointsDataFrame(coords, track, coords.nrs = numeric(0), proj4string = proj4string, match.ID = TRUE, bbox = NULL)
writePointsShape(out, fname, factor2char = TRUE, max_nchar=254)

# if(both == T){ both = F,
 # coords = coordinates(track[,8:9])
 # out.L = SpatialLinesDataFrame(coords, track, proj4string = proj4string, match.ID = TRUE, bbox = NULL)
 # writeLinesShape(out.L, paste(fname, "_Line", sep = ""), factor2char = TRUE, max_nchar = 254)
# }
}
