# OLD VERSION 
#plot.btrack <- function (btrack, map, cex = 1.5, ci = F, bathy = NULL, add = F, 
    # bathlevels = c(-100, -200), alpha = 0.15, bymonth = T, pch = 21, 
    # bg = 4, offset = 360, legend=F) 
# {
# bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))
# if(legend==T){ oldpar = unlist(par()['usr'])
# small = c(oldpar[2]-(oldpar[2]-oldpar[1])/10, oldpar[2]-(oldpar[2]-oldpar[1])/15, oldpar[3]+(oldpar[4]-oldpar[3])/5, oldpar[4]-(oldpar[4]-oldpar[3])/8)
# data(month.colors)
# par(mar = c(5.1,4.1,4.1,4.1))
# }
    # len = length(btrack[, 1])
	# btrack = btrack[!is.na(btrack[,8]),]
	# btrack = btrack[!is.na(btrack[,9]),]
    # xlims = c(min(btrack[, 8], na.rm=T) - 2, max(btrack[, 8], na.rm = T) + 2) + offset
    # ylims = c(min(btrack[, 9], na.rm = T) - 2, max(btrack[, 9], na.rm = T) + 2)
	# if(!is.null(map$SP)) map = map$SP
    # if (add == F) 
        # plot(map, col = "khaki", pbg = "azure2", xlim = xlims, 
            # ylim = ylims, xaxs = "i", yaxs = "i", axes = TRUE)
    # if (!is.null(bathy)) {
        # bathy$lon = bathy$lon + offset
        # contour(bathy$lon, bathy$lat, t(bathy$data), levels = bathlevels, 
            # drawlabels = F, add = T, col = bath.colors(length(bathlevels)))
    # }
    # S = SpatialPointsDataFrame(btrack[, 8:9], btrack)
    # if (btrack[1, 8] < 0) 
        # S@coords[, 1] = S@data[, 8] = btrack[, 8] = S@coords[, 
            # 1] + offset
    # proj4string(S) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
    # if (ci) {
        # sapply(1:len, function(i) .makeCI(as.numeric(btrack[i, 
            # 4:9]), col = rgb(0.7, 0.7, 0.7, alpha = alpha), border = 0))
        # plot(map, col = "khaki", pbg = "azure2", xaxs = "i", 
            # yaxs = "i", axes = F, add = T)
    # }
    # points(S, pch = pch, bg = bg, typ = "o")
    # if (bymonth) 
        # .plot.by.month(S@data, cex = cex, pch = pch)
    # lines(btrack[1, 8], btrack[1, 9], typ = "p", pch = 21, col = 1, 
        # bg = 3, cex = 1.8)
    # lines(btrack[len, 8], btrack[len, 9], typ = "p", pch = 24, 
        # col = 1, bg = 2, cex = 1.8)
    # box(lwd = 2)

# if(legend==T){ .add.month.scale(small)}

# }

#x11(type='Xlib')

#plot.btrack(fakeb,legend=T)

.add.month.scale <- function(...){
    ticks <- c(1:12)+0.5
    par(cex=0.9)
    image.plot(-matrix(1:13), horizontal=F, col = rev(month.colors[,2]),
             axis.args=list(at=-ticks,labels=month.abb[as.numeric(month.colors[,1])]),
             legend.args=list(text="", cex=0.75, side=3, line=1),
              legend.mar=3.6, legend.only=T,...)
    par(cex=1)
}

.add.bathy.scale <- function(colscale = c('blue','grey'),ticks = c(-10000,-8000,-6000,-4000,-2000,-1000,-500,0),...){
	
	if(colscale=='blue') bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))
	if(colscale=='grey') bath.colors <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))

    ticks <- ticks
    par(cex=0.9)
    image.plot(matrix(1), zlim=c(-10000,0), horizontal=T, col = bath.colors(100),
             axis.args=list(at=ticks,labels=ticks*-1), legend.shrink = 0.8, 
             legend.args=list(text="  Bottom \n depth (m)", cex=0.8, side=4, line=0.2, las=1),
             legend.only=T, ...)
    par(cex=1)
}

.plot.by.month <-
function(ttrack, saveplot=F, filename=NULL, cex=2, pch = 21){
    attach(ttrack)
data(month.colors)
# month.colors=cbind(c(8:12,1:7),
  # c(rgb(115/255,0,76/255),
    # rgb(0,76/255,115/255),
    # rgb(0,92/255,230/255),
    # rgb(115/255,223/255,1),
    # rgb(190/255,232/255,1),
    # rgb(1,1,190/255),
    # rgb(230/255,230/255,0),
    # rgb(1,170/255,0),
    # rgb(1,120/255,0),
    # rgb(1,0,197/255),
    # rgb(1,0,0),
    # rgb(168/255,0,0)
  # )
# )

    mons=unique(Month)
    for(i in 1:length(mons)){
      lines(ttrack[Month==mons[i],8:9],typ='o',cex=cex,pch=pch,bg=month.colors[month.colors[,1]==mons[i],2])
      
     if(saveplot){
        savePlot(filename,type='eps')
        savePlot(filename,type='png',res=100)
        }
    }
   detach(ttrack)
  }

#bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

# NEW VERSION
#' plot.btrack
#'
#' @param btrack 
#' @param map 
#' @param cex 
#' @param ci 
#' @param bathy 
#' @param add 
#' @param bathlevels 
#' @param alpha 
#' @param bymonth 
#' @param pch 
#' @param bg 
#' @param offset 
#' @param legend 
#' @param axes 
#' @param xlims 
#' @param ylims 
#'
#' @return
#' @export
#'
#' @examples
plot.btrack<- function (btrack, map=NULL, cex = 1.5, ci = F, bathy = NULL, add = F, bathlevels = c(-100, -200), alpha = 0.15, bymonth = T, pch = 21, bg = 4, offset = 0, legend = F, axes=T, xlims=c(-80, -50), ylims = c(20, 50)) 
{
    bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", 
        "cyan"))
	data(myramps)
	
	if(class(btrack)!='data.frame') class(btrack) = 'data.frame'
	
    if (legend == T) {
        oldpar = unlist(par()["usr"])
        small = c(oldpar[2] - (oldpar[2] - oldpar[1])/10, oldpar[2] - 
            (oldpar[2] - oldpar[1])/15, oldpar[3] + (oldpar[4] - 
            oldpar[3])/5, oldpar[4] - (oldpar[4] - oldpar[3])/8)
        data(month.colors)
        par(mar = c(5.1, 4.1, 4.1, 4.1))
    }
    len = nrow(btrack)
	
	btrack[,8] = btrack[,8] + offset   
	
    btrack = btrack[!is.na(btrack[, 8]), ]
    btrack = btrack[!is.na(btrack[, 9]), ]
    # xlims = c(min(btrack[, 8], na.rm = T) - 2, max(btrack[, 8], 
        # na.rm = T) + 2)
    # ylims = c(min(btrack[, 9], na.rm = T) - 2, max(btrack[, 9], 
        # na.rm = T) + 2)
    # if (is.null(map)) data(ATL); map = map2$SP
    if (add == F) 
		if (is.null(map)) {
			world(xlim = xlims, ylim = ylims, xlab = 'Latitude', ylab = 'Longitude', fill=T, col = 'grey90')
			# mymap = F
			}else{
				plot(map, col = "khaki", pbg = "azure2", xlim = xlims, ylim = ylims, axes=F, xlab = 'Latitude', ylab = 'Longitude') # 
			}
    if (!is.null(bathy)) {
        bathy$lon = bathy$lon + offset
        contour(bathy$lon, bathy$lat, t(bathy$data), levels = bathlevels, drawlabels = F, add = T, col = bath.colors(length(bathlevels)))
    }
    # S = SpatialPointsDataFrame(btrack[, 8:9], btrack)     
	
	 S = SpatialPointsDataFrame(btrack[, 8:9], btrack)
     if(is.null(map)) S@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
	 else S@proj4string = map@proj4string  #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
	
    if (ci==T) {
        sapply(1:len, function(i) .makeCI(as.numeric(btrack[i, 
            4:9]), col = rgb(0.7, 0.7, 0.7, alpha = alpha), border = 0))
			# if(mymap==F) world(xlim = xlims, ylim = ylims, xlab = 'Latitude', ylab = 'Longitude', fill=T, col = 'grey90')
			# else plot(map, col = "khaki", axes=F, xlab = '', ylab = '', add=T)
    }
    points(S, pch = pch, bg = bg, typ = "o")
    if (bymonth) 
        .plot.by.month(as(S, 'data.frame'), cex = cex, pch = pch)
    lines(btrack[1, 8], btrack[1, 9], typ = "p", pch = 21, col = 1, 
        bg = 3, cex = 1.8)
    lines(btrack[len, 8], btrack[len, 9], typ = "p", pch = 24, 
        col = 1, bg = 2, cex = 1.8)
    box(lwd = 2)
	if(axes){ degAxis(1); degAxis(2);}
    if (legend == T) {
        .add.month.scale(small)
    }
}
