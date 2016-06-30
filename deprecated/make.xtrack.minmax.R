make.xtrack.minmax  =
function (tag, xmin = -100, xmax = 0, ymin = 10, ymax = 55, keepall = F) 
{
    pminmax = tag$pminmax
    pdates = as.date(pminmax[, 1])
    tminmax = tag$tminmax
    tdates = as.date(tminmax[, 1])
    dim1 = dim(tag$T)[1]
    dim2 = dim(tag$T)[2]
    Temp = tag$T[1:(dim1), ]
    Z = tag$Z[1:(dim1), ]
    loc.init = c(date.mdy(tag$day0)$year, date.mdy(tag$day0)$month, 
        date.mdy(tag$day0)$day, (tag$x0))
    loc.last = c(date.mdy(tag$dayT)$year, date.mdy(tag$dayT)$month, 
        date.mdy(tag$dayT)$day, (tag$xT))
    locs = tag$MWTxy
	locs[,4:5] = rev(locs[,4:5])
	
    if (mdy.date(tag$MWTxy[1, 2], tag$MWTxy[1, 3], tag$MWTxy[1, 
        1]) == tag$day0) {
        locs[1, 4:5] = tag$x0
    }else {    
        locs = rbind(loc.init, locs)
    }
    len = length(locs[, 4])
    locs[len, 1:5] = loc.last
    dates = rev(locs[, 1:3])
    Temp[Z <= (-11)] = NaN
    maxz = apply(Z, 1, min, na.rm = T)
    maxz[!is.finite(maxz)] = 0
    maxt = apply(Temp, 1, max, na.rm = T)
    dat = as.data.frame(cbind(dates, (locs[, 4:5]), maxt[1:len], 
        maxz[1:len]))
    if (!is.null(tag$pminmax)) {
        pminmax = tag$pminmax
        pdates = as.date(pminmax[, 1])
        tminmax = tag$tminmax
        tdates = as.date(tminmax[, 1])
        ldates = mdy.date(locs[, 2], locs[, 3], locs[, 1])
        didx = !is.na(match(tdates, ldates))
        ttemp = cbind(maxt, tminmax)
        ttemp = apply(cbind(ttemp[, 1], ttemp[, 4]), 1, max, 
            na.rm = T)
        ptemp = cbind(maxz, pminmax)
        ptemp = apply(cbind(ptemp[, 1], ptemp[, 4]), 1, min, 
            na.rm = T)
        dat = as.data.frame(cbind(dates, (locs[, 4:5]), ttemp[didx], 
            ptemp[didx]))
    }
	
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
        dat[!is.na(dat[, 4]) & dat[, 4] < xmin, 4] = NA
        dat = dat[!is.na(dat[, 5]), ]
        dat = dat[!is.na(dat[, 4]), ]
        dat[is.na(dat[, 6]), 6] = -1
    }
    rm(Temp, maxz, dates, locs, Z, dim1, dim2, tag)
    class(dat) = c('xtrack', 'data frame')
	dat
}