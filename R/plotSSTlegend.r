#' plotSSTlegend
#'
#' @param cbrks 
#' @param cex 
#' @param font 
#'
#' @return
#' @export
#'
#' @examples
plotSSTlegend <- function (cbrks = 33, cex=1.2, font= 1) 
{
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", 
        "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
	breaks = pretty(1:cbrks,201)
	mid = breaks[1:(length(breaks)-1)]
    col = jet.colors(length(breaks)-1) 
	#tzcol = jet.colors(cbrks)[as.vector((dataT))]

   image(mid,1,as.matrix(mid), breaks=breaks, col=col, axes=FALSE, ylab="", xlab=expression(C^o), font.lab = font, cex=cex) 
   axis(1);box();

	}

#plotTZprofile(psat[[3]],legend=T)