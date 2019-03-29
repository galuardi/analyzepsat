
# functions to match point to kernel areas. Points and kernels must correspond

normalize <- function(x) x/max(x,na.rm=T)

normalise <- function(ingrid){
		normConst = sum(ingrid,na.rm=T)
		ingrid/normConst
	}

  fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
	
  fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }	

#============================================================#
# function to get the home range density over a fine grid
#============================================================#
#' Title
#'
#' @param track 
#' @param xsize 
#' @param ysize 
#' @param range.x 
#' @param range.y 
#'
#' @return
#' @export
#'
#' @examples
track2KD=function(track,xsize=.1,ysize=.1,range.x=c(-100,-5),range.y=c(20,50)){
  require(GenKern)
  dd=dim(track)
  xgsize = (range.x[2]-range.x[1])/xsize
  ygsize=(range.y[2]-range.y[1])/ysize
  if(dd[2]>6) {
    xidx=8;yidx=9;coridx=4:7;varidx=c(4,7)
    } else{xidx=5;yidx=6;coridx=1:4;varidx=c(1,4)
      }
  len=length(track[,xidx])
  x=track[,xidx]
  y=track[,yidx]
  vars=cbind(track[,varidx])
  cormat=NULL
    for(i in 1:len){cormat[i]=as.vector(cov2cor(matrix(as.numeric(track[i,coridx]),2,2))[2])}
  cormat[is.nan(cormat)]=0
  cormat[is.infinite(cormat)]=0
  vars[vars==0]=1e-10
  op=KernSur(x,y,xgridsize=xgsize,xbandwidth=vars[,1],ybandwidth=vars[,2],
    ygridsize=ygsize,correlation=cormat,range.x=range.x,range.y=range.y)
  op[[3]][is.na(op[[3]])]=0
  op[[3]][is.nan(op[[3]])]=0
  op
   }         
	
#============================================================#
# Function to convert a KF kernel object to UDvolume in asc form
#============================================================#

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
kern2UD <- function(x){
		asc = as.asc(normalise(x[[3]]), xll = min(x[[1]]), yll = min(x[[2]]), cellsize = diff(x[[1]])[2], type = c("numeric"))
		cs <- attr(asc, "cellsize")
        v <- .C("calcvolume", as.double(t(asc)), as.integer(ncol(asc)), 
            as.integer(nrow(asc)), as.double(cs), PACKAGE = "adehabitatHR")[[1]]
        index <- 1:length(v)
        vord <- v[order(v, decreasing = TRUE)]
        indord <- index[order(v, decreasing = TRUE)]
        vsu <- cumsum(vord)
        vreord <- vsu[order(indord)] * 100
        u <- matrix(vreord, ncol = ncol(asc), byrow = TRUE)
        UD <- getascattr(asc, u)
        attr(UD, "UD") <- "volume"
        x <- UD
    class(x) <- c("asc")
	x
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
kern2UDarch <- function(x){
		asc = as.asc(normalize(x[[3]]), xll = min(x[[1]]), yll = min(x[[2]]), cellsize = diff(x[[1]])[2], type = c("numeric"))
		cs <- attr(asc, "cellsize")
        v <- .C("calcvolume", as.double(t(asc)), as.integer(ncol(asc)), 
            as.integer(nrow(asc)), as.double(cs), PACKAGE = "adehabitatHR")[[1]]
        index <- 1:length(v)
        vord <- v[order(v, decreasing = TRUE)]
        indord <- index[order(v, decreasing = TRUE)]
        vsu <- cumsum(vord)
        vreord <- vsu[order(indord)] * 100
        u <- matrix(vreord, ncol = ncol(asc), byrow = TRUE)
        UD <- getascattr(asc, u)
        attr(UD, "UD") <- "volume"
        x <- UD
    class(x) <- c("asc")
	x
}


#' Title
#'
#' @param x 
#' @param levels 
#'
#' @return
#' @export
#'
#' @examples
UD.area = function(x, levels = seq(.20, .95, by = .05)){
    area <- list()
    contours <- list()
    for (j in names(x)) {
        tmpsurf <- rep(0, length(levels))
        for (i in 1:length(levels)) {
            asc <- x[[j]]
            tmp <- asc < levels[i]
            cs <- attr(asc, "cellsize")
            tmpsurf[i] <- sum(as.numeric(tmp)) * cs * cs
        }
        area[[j]] <- tmpsurf
    }
    area <- data.frame(area)
	 row.names(area) <- levels
    names(area) <- names(x)
	area*100  # for km
	}

# image.plot(kern2UD(monthUD[[3]]))
# contour(kern2UD(monthUD[[3]]), add=T)


#' Title
#'
#' @param x 
#' @param lev 
#'
#' @return
#' @export
#'
#' @examples
kern2vertices <- function(x, lev=95){
ud <- kern2UD(x)
        xyl <- getXYcoords(ud)
        re <- contourLines(x = xyl$x, y = xyl$y, ud, nlevels = 1, 
            levels = lev)
        so <- do.call("rbind", lapply(1:length(re), function(i) {
            so <- data.frame(fac = rep(i, length(re[[i]]$x)), 
                x = re[[i]]$x, y = re[[i]]$y)
            return(so)
        }))
  so[, 1] <- factor(so[, 1])
  contour <- as.area(so)
  #names(contour) <- names(x)
  class(contour) <- c("kver", "area")
  contour
}

	# x = kern2UD(monthUD[[3]])
	# y=kern2vertices(monthUD[[3]], .95)
	# image.plot(x)
	# plot(y, add=T, colpol=-1)

#===================================================================#
# Function to determine which points are within various UD levels
# Outputs a data frame with binary columns for each level
# Requires input of KF kernel object and tracks where long/lat are columns 8:9
#==================================================================#
 
#' Title
#'
#' @param btrack 
#' @param kern 
#' @param level 
#'
#' @return
#' @export
#'
#' @examples
matchkern <- function(btrack, kern, level = c(.5, .95)){
require(adehabitat)
require(sp)

# use a posix date 
posdates = ISOdate(btrack$Year, btrack$Month, btrack$Day)

# make ltraj object
x = as.ltraj(xy = btrack[,8:9], date = posdates, id = btrack$tagID)

# using the aggregated kernel from KF CI's, detrermine which points are in high use areas
kxy = expand.grid(kern[[1]], kern[[2]])
kxyz = cbind(kxy, as.vector(normalise(kern[[3]])))

# create a kernel density object (kud) for use in the next steps
ktemp=(kernelUD(btrack[,8:9], grid = 200))

# make a replacement density asc object based on user arguement. Assumes square cells!
tasc = as.asc(kern[[3]], xll = min(kern[[1]]), yll = min(kern[[2]]), cellsize = diff(kern[[1]])[2], type = c("numeric"))
ktemp[[1]]$UD = normalise(tasc)

# get overlays of points in high use areas
sptr = btrack
coordinates(sptr)=sptr[,8:9]

conts = cpolys = srpolys = opts = NULL

# loop through each UD level
for(i in 1:length(level)){
	x = getverticeshr(ktemp, lev = level[i])[[1]]  # gets the boundaries of the UD for each level
	li <- split(x[, 2:3], x[, 1])
	which = levels(x[,1])
	cpolys[[i]] = lapply(1:length(li), function(j) Polygons(list(Polygon(cbind(li[[which[j]]]$x,li[[which[j]]]$y))),j)) # this goes through each polygon (for each level)
	srpolys = SpatialPolygons(cpolys[[i]])
	opts[[i]] = overlay(sptr, srpolys)
	idx = is.na(opts[[i]])
	opts[[i]][idx] = 0
	opts[[i]][idx==F] = 1
	}
names(opts) = paste('c',(1-level)*100, sep="")
opts = as.data.frame(opts)
opts
}
