#' plotTZprofile
#'
#' @param tzdata 
#' @param zlim 
#' @param pch 
#' @param cex 
#' @param font 
#' @param cex.lab 
#' @param cbrks 
#' @param ylab 
#' @param cex.axis 
#' @param MWT 
#' @param axes 
#' @param legend 
#'
#' @return
#' @export
#'
#' @examples
plotTZprofile =  function (tzdata, zlim = NULL, pch = 21, cex = 1.2, font = 1, 
    cex.lab = 1.2, cbrks = 33, ylab = "Depth (m)", cex.axis = 1, 
    MWT = T, axes = T, legend = T) 
{
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", 
        "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    require(date)
    if (MWT) {
        dataZ = t(tzdata$Z)
        dataT = t(tzdata$T)
        if (!is.null(zlim)) {
            dataZ[dataZ < (zlim)] = NaN
        }
    }
    else {
        dataT = tzdata$depth
    }
    day0 = tzdata$day0
    dayT = tzdata$dayT
    breaks = pretty(1:cbrks, 201)
    mid = breaks[1:(length(breaks) - 1)]
    col = jet.colors(length(breaks) - 1)
    tzcol = jet.colors(cbrks)[as.vector((dataT))]
    plotdata = as.vector(dataZ)
    if (legend == T) {
        layout(matrix(c(2, 2, 2, 2, 1), ncol = 1))
        image(mid, 1, as.matrix(mid), breaks = breaks, col = col, 
            axes = FALSE, ylab = "", xlab = expression(C^o), 
            font.lab = font, cex = cex)
        axis(1, font = font, cex.lab = cex.lab)
        box()
    }
    plot(plotdata, axes = F, type = "p", xlab = "", ylab = ylab, 
        pch = pch, col=0, bg = tzcol, par(c(cex = cex)), font = font, 
        font.lab = font, cex.lab = cex.lab)
    xidx = (dim(dataT)[1] * dim(dataT)[2])
    if (day0 > 7e+05) {
        day0 = day0 - 715876 + 1
        dayT = dayT - 715876 + 1
    }
    tzdates = seq(day0, dayT + 1)
    if (axes) {
        xp <- par("xaxp")
        xcut <- round(seq(xp[1], xidx, length = 12))
        dcut = seq(day0, dayT, length = 12)
        xcut[1] = 1
        axis(1, at = xcut, label = paste(as.date(dcut)), las = 2, 
            font = font, font.lab = font, cex.axis = cex.axis)
        axis(2, font = font, font.lab = font, cex.axis = cex.axis, 
            las = 2)
    }
    box()
}




