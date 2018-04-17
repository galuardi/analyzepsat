#' Title
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
}