.fill.vals <- function (vec, span = 0.25) 
{
	len = length(vec)
	vlen = 1:len
	# fidx = (vec < 5) 
	fidx1 = is.infinite(vec) 
	fidx2 = is.na(vec)
	fidx3 = vec<0
	fidx4 = is.nan(vec)
	fidx = as.logical(fidx1+fidx2+fidx3+fidx4)
	fidx[is.na(fidx)] = TRUE
	vec[fidx] = NA
	if (any(fidx == T)) {
		ltmp = loess(vec ~ vlen, span = span)
		#ltmp = locfit(vec ~ vlen, alpha = span)
		vec[fidx] = predict(ltmp, newdata = vlen[fidx])
	}
	vec
}

.kfsys<-function(cmd) if(.Platform$OS.type=='windows'){shell(cmd)}else{system(cmd)}

.gmtok = function () 
{
	.testgmt <- tempfile("testgmt")
	on.exit(unlink(.testgmt))
	return(.kfsys(paste("gmtdefaults -L 1>", .testgmt)) == 0)
}

MWTextract = function (tagID, xlsfile, taglocfile, delta=F, minmax=F)
{
	#psatcon = odbcConnectExcel(xlsfile, readOnly = T)
	print("Reading tagging data")
	
	day0 = (read_excel(xlsfile, skip = 3, sheet = 'Lat&Long', n_max = 1)[,8])[[1]]
	x0 = as.numeric((read_excel(xlsfile, skip = 5, sheet = 'Lat&Long', n_max = 1))[,8:9])
	
	dayT = (read_excel(xlsfile, skip = 9, sheet = 'Lat&Long', n_max = 1)[,8])[[1]]
	xT = as.numeric((read_excel(xlsfile, skip = 11, sheet = 'Lat&Long', n_max = 1))[,8:9])
	
	# x0 = .getTaggingLoc(taglocfile, tagID, tyear)
	# x0 = rev(x0)
	
	# day0 = .getTaggingDay(taglocfile, tagID, tyear)
	# day0b = ISOdatetime(date.mdy(day0)[[3]], date.mdy(day0)[[1]], date.mdy(day0)[[2]], 0, 0, 0)
	# 
	print("Reading ARGOS data")
	adata = .GetArgosPSAT(xlsfile) 
	# dayT = .GetLastDay(adata)
	# dayTb = .GetDayTb(dayT)
	# xT = .GetLastLoc(adata)
	print("Reading MWT produced locations")
	MWTxy = .getMWTxy(xlsfile, x0, xT, day0, dayT)
	# fulldates = seq(day0, dayT)
	fulldates = seq(day0, dayT, by = 'day')
	
	print("Reading sunset/sunrise times")
	SRSS = .getSRSS(xlsfile, day0, dayT)
	
	print("Reading temperature data")
	if(delta==T){ 
		print("Removing delta limited Values")
		dataT = .getPSATtemp(xlsfile, day0, dayT, delta=T)
	}else{
		dataT = .getPSATtemp(xlsfile, day0, dayT, delta=F)
	}
	print("Reading pressure data")
	if(delta==T){ 
		print("Removing delta limited Values")
		dataP = .getPSATdepth(xlsfile, day0, dayT, delta=T)
	}else{
		dataP = .getPSATdepth(xlsfile, day0, dayT, delta=F)
	}
	if(minmax==T){
		print("Reading daily minimum and maximum depth and temperature")
		mmp = .getminmaxp(xlsfile, day0, dayT, MWTxy)
		mmt = .getminmaxt(xlsfile, day0, dayT, MWTxy)
		dataout = list(tagID = tagID, x0 = x0, day0 = day0,  xT = xT, 
									 dayT = dayT, fulldates = fulldates,SRSS = SRSS,  
									 MWTxy = MWTxy, T = dataT, Z = dataP, mmt = mmt, mmz = mmp, Argos = adata)
	}else{
		dataout = list(tagID = tagID, x0 = x0, day0 = day0, xT = xT, 
									 dayT = dayT, fulldates = fulldates,SRSS = SRSS,  
									 MWTxy = MWTxy, T = dataT, Z = dataP, Argos = adata)
	}
	#     odbcClose(psatcon) #
	class(dataout) = c('MWTpsat', 'list')
	dataout
}


.GetArgosPSAT = function (xlsfile) {
		res = read_excel(xlsfile, sheet = 'Argos Data', skip = 1)
		adata = res[, 1:4]
		names(adata) = c("Date-Time", "LC", "Lat", "Lon")
		res = res[!is.na(res[, 1]), ]
		res = res[!is.na(res[, 2]), ]
		d1 = dim(res)[1]
		d2 = dim(res)[2]
	as.data.frame(adata)
}

.getMWTxy = function (xlsfile, x0, xT, day0, dayT) 
{
	MWTxy = read_excel(xlsfile, sheet = "Lat&Long", skip=1)
	MWTxy = MWTxy[!is.na(MWTxy[, 1]), 1:3]
	
	datevec = seq(day0, dayT, by = 'day')
	didx = match(MWTxy$Date, datevec)
	
	len = length(datevec)
	MWTdata = as.data.frame(array(NA, c(len, 5)))
	didx = didx[!is.na(didx)]
	MWTdata[, 1:3] = cbind(year(datevec), month(datevec), day(datevec))
	MWTdata[didx, 4:5] = MWTxy[, 2:3]
	MWTdata[1, 4:5] = x0
	MWTdata[len, 4:5] = xT
	MWTdata[, 5] = -1 * (MWTdata[, 5])
	names(MWTdata) = c("Year", "Month", "Day", "Lat", "Lon")
	MWTdata
}


.getSRSS = function (xlsfile, day0, dayT) 
{
		res = as.data.frame(read_excel(xlsfile, sheet = "Sunrise and Sunset Times", skip=1))
		res = res[!is.na(res[, 1]), c(1, 2, 4)]
		len = length(res[, 1])
		# srssdates = as.POSIXct(strptime(res[,1], "%b %d, %Y"))
		srssdates = res[,1]
		
		SR = hour(res[,2])*60+minute(res[,2])
		SS = hour(res[,3])*60+minute(res[,3])
	
		
		tidx = srssdates>=day0&srssdates<=dayT
		
		SRSS = data.frame(Date = srssdates[tidx], SR = SR[tidx], 
											SS = SS[tidx])
		SRSS[which(SRSS[, 3] < 500), 3] = SRSS[which(SRSS[, 3] < 
																								 	500), 3] + 1400
		SRSS[which(SRSS[, 2] > 1400), 3] = SRSS[which(SRSS[, 2] > 
																										1400), 2] - 500
	SRSS
}

.getPSATtemp = function (xlsfile, day0, dayT, delta=F) 
{

		tres = as.data.frame(read_excel(xlsfile, "Temp Data", skip =1))
		tres = tres[2:length(tres[, 1]), ]
		tdates = tres[,1]
		
	tidx = tdates <= (dayT) & tdates >= day0
	tres = tres[tidx, ]
	
	ttimes = seq(day0, dayT, 86400/96)
	
	ttidx = match(tdates, ttimes)
	
	ttidx = ttidx[!is.na(ttidx)]
	
	dataT = matrix(NaN, 24 * 4, length(seq(day0, dayT, 86400)))
	
	if(delta){
		delt = as.numeric(as.character(tres[,4]))
		delt[is.na(delt)] = 0
		delt=abs(delt)>=31
		tres[delt,3] = NA
	}
	dataT[ttidx] = tres[, 3]
	dataT = t(dataT)
	dataT
}


.getPSATdepth <- function (xlsfile, day0, dayT, delta = F) 
{

		pres = as.data.frame(read_excel(xlsfile, "Press Data", skip = 1))
		pres = pres[2:length(pres[, 1]), ]
		pdates = pres[,1]

	pidx = pdates <= (dayT) & pdates >= day0
	pres = pres[pidx, ]
	ptimes = seq(day0, dayT, 86400/96)
	ptidx = match(pdates, ptimes)
	ptidx = ptidx[!is.na(ptidx)]
	dataP = matrix(NaN, 24 * 4, length(seq(day0, dayT, 86400)))
	if (delta) {
		pres[, 4] = as.numeric(as.character(pres[, 4]))
		deltap = as.numeric(as.character(pres[, 5]))
		deltap[is.na(deltap)] = 0
		deltap = abs(deltap) >= 31
		pres[deltap, 4] = NA
	}
	if ((min(pres[, 3], na.rm = T) < 0)==F) {
		dataP[ptidx] = pres[, 4]
	}else {
		dataP[ptidx] = pres[, 3]
	}
	dataP = t(dataP)
	dataP
}

.getminmaxp <- function(xlsfile, day0, dayT, MWTxy){
		pres = as.data.frame(read_excel(xlsfile, "Press Data (MinMax)", skip =1))
		pres= pres[!is.na(pres[,1]),]
		mmpdates = pres[,1]
	
	alldates = seq(day0, dayT, by = 'day')
	
	pidx = mmpdates <= (dayT) #& mmpdates >= day0b
	pres = pres[pidx, ]
	didx = match(mmpdates,alldates)
	didx=didx[!is.na(didx)]
	mmp = as.data.frame(matrix(NA, nrow=length(alldates), ncol=3))
	mmp[,1] = alldates
	mmp[didx,2] = pres[,4]
	mmp[didx,3] = pres[,5]
	names(mmp) = c('Date','minz','maxz')
	mmp
}


.getminmaxt <- function(xlsfile,  day0, dayT, MWTxy){
		tres = as.data.frame(read_excel(xlsfile, "Temp Data (MinMax)", skip =1))
		mmtdates = tres[,1]
		alldates = seq(day0, dayT, by = 'day')
	pidx = mmtdates <= (dayT) #& mmtdates >= day0b
	tres = tres[pidx, ]
	didx = match(mmtdates,alldates)
	didx=didx[!is.na(didx)]
	mmt = as.data.frame(matrix(NA, nrow=length(alldates), ncol=3))
	mmt[,1] = alldates
	mmt[didx,2] = tres[,4]
	mmt[didx,3] = tres[,5]
	names(mmt) = c('Date','mint','maxt')
	mmt
}

print.MWTpsat<-function(x,...){ 
	# headvec<-strsplit(x$header, split="\n")[[1]]
	"%+%"<-function(s1, s2)paste(s1, s2, sep="")
	out<-"\n\n#Microwave Telemetry PSAT\n"%+%x$tagID%+%"\n"%+%
		"\n#Days at Liberty: "%+%c(x$dayT-x$day0+1)%+%"\n"%+%
		"#Number of observations: "%+%nrow(x$MWTxy)%+%"\n"%+%
		"\n#Tagging Date: "%+%x$day0b%+%"\n"%+% 
		"#Tagging Latitude "%+%x$x0[1]%+%"\n"%+%
		"#Tagging Longitude "%+%x$x0[2]%+%"\n"%+%
		"\n#Report Date: "%+%x$dayTb%+%"\n"%+%
		"#Report Latitude "%+%x$xT[1]%+%"\n"%+%
		"#Report Longitude "%+%x$xT[2]%+%"\n"
	cat(out)
	# cat("Parameters:\n")
	# print(rbind("Estimates:"=x$estimates,"Std. dev.:"=x$std.dev))
	
	cat("\nThis object contains the following sub-items:\n")
	print(names(x))  
}


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
	if (is.null(sst.depth) == F) 
		Temp[Z <= (sst.depth)] = NaN
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

