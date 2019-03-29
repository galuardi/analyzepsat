
#' get.SST
#'
#' @param lon
#' @param lat
#' @param SST
#' @param date
#'
#' @return
#' @export
#'
#' @examples
get.SST <-
function(lon,lat,SST,date){
    X=as.vector(SST$lon)
    Y=as.vector(SST$lat)
    xidx=which.min((lon-X)^2)
    yidx=which.min((lat-Y)^2)
    zidx=which.min((as.numeric(date)-as.numeric(SST$sstdates))^2)
    SST$DATA[xidx,yidx,zidx]
  }
