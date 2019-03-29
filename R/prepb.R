
#' prepb
#' 	Make a dataframe for bathymetric and/or longitudinal sst correction
#' @param kfit A fitted object returned from kftrack, ukfsst, kfsst
#' @param prepf Original data frame used for kftrack/ukfsst
#' @param fill.sst 	Logical. Should days with missing SST be interpolated? If TRUE, loess smoothing is applied.
#' @param span span, or bandwidth, for loess smoothing if fill.sst = TRUE
#'
#' @return
#' @export
#' @description
#'	Makes a dataframe for bathymetric and/or longitudinal sst correction. This format is also useful for export to Excel, ArcMap, or other software. This does not currently work with trackit but future versions will have this capability.
#' @examples
#'
#'
#' \name{prepb}

#' @details LOESS smoothing is more accurate the less data is missing. Bandwidth is always an important consideration and should be reduced if there is a long time series with relatively little missing data. It is also worth noting that interpolating SST, which is most cases is considered the maximum temperature per day, will be biased towards colder regions if the fish is consistently deep over the measured time period.
#' @value A dataframe with columns for Year, Month, Day, V11, V12, V21, V22, Longitude, Latitude, Maxdepth and Max Temp
#' @author Benjamin Galuardi
#' @ seealso \code{\link{prepf},\code{\link{make.btrack}}}

#' @example
prepb = function (kfit, prepf, fill.sst = F, span = NULL)
{
	if(any(prepf[,7]>0)) print('You have positive Depth values! Please correct and re-run this function')
	if(any(prepf[,4]>180)) print('You have Longitude values greater than 180. Please manipulate your longitude values to be -180 to 180.')
    if (fill.sst) {
		if(is.null(span)) span=.75
        prepf[,6] = .fill.vals(prepf[,6], span = span)
    }
    fmat = as.data.frame(cbind(prepf[, rev(1:3)], kfit$var.most.prob.track,
        kfit$most.prob.track, prepf[, c(7, 6)]))
    names(fmat) = c("Year", "Month", "Day", "V11", "V12", "V21",
        "V22", "Lon_E", "Lat_N", "max_depth", "SST")
	#fmat$Lon_E = fmat$Lon_E-360
	#attr(fmat,'Header') = "#Output data frame from kftrack/kfsst/ukfsst #Includes Max daily depth for Bathymetric correction"
    fmat
}
