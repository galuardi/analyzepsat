
#' Generate an object for kftrack/ukfsst estimation
#' This function takes an object created by \code{\link{MWTextract}} and returns a data frame with 7 columns suitable for kf estimation. This also sets some limits on where in the world the fish may have gone. These limits are meant to be extremely broad and exist to filter only the most egregiously erroneous light measurements. The result is a standard format for kftrack/ukfsst analysis
#' @param tag object returned from MWTextract
#' @param xmin Western longitude cutoff
#' @param xmax Eastern longitude cutoff
#' @param ymin Southern latitude cutoff
#' @param ymax Northern latitude cutoff
#' @param keepall logical. If you do not wish to use these cutoffs, keepall = T. Default is keepall = F
#' @param use.minmax Should min/max temperature and depth tabs be used. If False, the archival data will be used to detemine a min/max for each day. If the minmax data are present, this should be True.
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' tagdata = MWTextract(tagID = '12345',  xlsfile = '12345.xlsx', use.minmax = T)
#' xdata = prepf(tagdata, xmin = 100, xmax = 0, ymin = 10, ymax = 55, keepall = F)
#'
prepf <- function (tag, xmin = -100, xmax = 0, ymin = 10, ymax = 55, keepall = F,
									 sst.depth = NULL, use.minmax = F)
{
	dim1 = dim(tag$T)[1]
	dim2 = dim(tag$T)[2]
	Temp = tag$T[1:(dim1), ]
	Z = tag$Z[1:(dim1), ]
	loc.init = tag$x0
	loc.last = tag$xT
	# loc.init = c(date.mdy(tag$day0)$year, date.mdy(tag$day0)$month,
	# 						 date.mdy(tag$day0)$day, (tag$x0))
	# loc.last = c(date.mdy(tag$dayT)$year, date.mdy(tag$dayT)$month,
	# 						 date.mdy(tag$dayT)$day, (tag$xT))
	locs = tag$MWTxy
	len = length(locs[, 4])
	# locs[1, 1:5] = loc.init
	# locs[len, 1:5] = loc.last
	locs[, 4:5] = rev(locs[, 4:5])
	dates = rev(locs[, 1:3])
	# if (is.null(sst.depth) == F)
	# 	Temp[Z <= (sst.depth)] = NaN
	maxz = apply(Z, 1, min, na.rm = T)
	maxz[!is.finite(maxz)] = 0
	maxt = apply(Temp, 1, max, na.rm = T)
	if (use.minmax) {
		# mdates = as.POSIXct(trunc(ISOdate(tag$MWTxy[, 1], tag$MWTxy[,
		# 2], tag$MWTxy[, 3]), "day"))
		# mdates = tag$fulldates
		mdates = seq(tag$day0, tag$dayT, 'day')
		attributes(mdates)$tzone = 'GMT'
		mdates = as.POSIXct(trunc(mdates,'day'))
		tdates = tag$mmt[, 1]
		pdates = tag$mmz[, 1]
		tidx = na.omit(match(tdates, mdates))
		pidx = na.omit(match(pdates, mdates))
		maxz = maxt = numeric(nrow(tag$MWTxy))
		maxz[pidx] = tag$mmz[, 3]
		maxt[tidx] = tag$mmt[, 3]
		maxz2 = apply(Z, 1, min, na.rm = T)
		maxz2[!is.finite(maxz2)] = 0
		maxt2 = apply(Temp, 1, max, na.rm = T)
		maxz[is.na(maxz)] = maxz2[is.na(maxz)]
		maxt[is.na(maxt)] = maxt2[is.na(maxt)]
	}
	dat = as.data.frame(cbind(dates, (locs[, 4:5]), maxt[1:len],
														maxz[1:len]))
	names(dat)[4:7] = c("Lon", "Lat", "SST", "maxD")
	if (keepall) {
		dat = dat[!is.na(dat[, 5]), ]
		dat = dat[!is.na(dat[, 4]), ]
	}
	else {
		dat[is.nan(dat[, 5]), 5] = NA
		dat[is.nan(dat[, 4]), 4] = NA
		dat[!is.na(dat[, 5]) & dat[, 5] < ymin, 5] = NA
		dat[!is.na(dat[, 5]) & dat[, 5] > ymax, 5] = NA
		dat[!is.na(dat[, 4]) & dat[, 4] > xmax, 4] = NA
		dat[!is.na(dat[, 4]) & dat[, 4] < (xmin), 4] = NA
		dat = dat[!is.na(dat[, 5]), ]
		dat = dat[!is.na(dat[, 4]), ]
		dat[is.na(dat[, 6]), 6] = -1
	}
	rm(Temp, maxz, dates, locs, Z, dim1, dim2, tag)
	attr(dat, "Header") = "#Input data frame for kftrack/kfsst/ukfsst"
	dat
}


prepf.arch <- function (tag, xmin = -100, xmax = 0, ymin = 10, ymax = 55, keepall = F)
{
	loc.init = c(date.mdy(tag$day0)$year, date.mdy(tag$day0)$month,
							 date.mdy(tag$day0)$day, (tag$x0))
	loc.last = c(date.mdy(tag$dayT)$year, date.mdy(tag$dayT)$month,
							 date.mdy(tag$dayT)$day, (tag$xT))
	locs = tag$MWTxy
	len = length(locs[, 4])
	locs[1, 1:5] = loc.init
	locs[len, 1:5] = loc.last
	locs[, 4:5] = rev(locs[, 4:5])
	dates = rev(locs[, 1:3])
	maxz = tag$mmz[,3]
	maxz[!is.finite(maxz)] = 0
	maxt = tag$mmt[,3]
	dat = as.data.frame(cbind(dates, (locs[, 4:5]), maxt[1:len],
														maxz[1:len]))
	names(dat)[4:7] = c("Lon", "Lat", "SST", "maxD")
	if (keepall) {
		dat = dat[!is.na(dat[, 5]), ]
		dat = dat[!is.na(dat[, 4]), ]
	}
	else {
		dat[is.nan(dat[, 5]), 5] = NA
		dat[is.nan(dat[, 4]), 4] = NA
		dat[!is.na(dat[, 5]) & dat[, 5] < ymin, 5] = NA
		dat[!is.na(dat[, 5]) & dat[, 5] > ymax, 5] = NA
		dat[!is.na(dat[, 4]) & dat[, 4] > xmax, 4] = NA
		dat[!is.na(dat[, 4]) & dat[, 4] < (xmin), 4] = NA
		dat = dat[!is.na(dat[, 5]), ]
		dat = dat[!is.na(dat[, 4]), ]
		dat[is.na(dat[, 6]), 6] = -1
	}
	attr(dat, "Header") = "#Input data frame for kftrack/kfsst/ukfsst"
	dat
}
