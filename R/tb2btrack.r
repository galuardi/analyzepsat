
# function to extract a corrected track from tagbase. Temp/depth not included

#' Title
#'
#' @param tb 
#' @param tagid 
#' @param is.ptt 
#'
#' @return
#' @export
#'
#' @examples
tb2btrack = function(tb, tagid, is.ptt = F){
	require(RODBC)
	con = odbcConnectAccess(tb)
	print(paste("building btrack for Tag# ", tagid, sep = ""))

	if(is.ptt==T){
		taginfo = sqlFetch(con, "TagInfo")
		# attach(taginfo)
		tagidx = taginfo$TagPTTID == pttid
		tagid = taginfo$TagID[tagidx] 
	}

	track = sqlQuery(con, paste('select * from track where TagID = ', tagid, sep = ""))
	track$Year = as.numeric(format(track$SDate,'%Y')) 
	track$Month = as.numeric(format(track$SDate,'%m')) 
	track$Day = as.numeric(format(track$SDate,'%d')) 

	varx =  sqlQuery(con, paste('select * from Analysis_Outputs where ParameterID=204 AND TagID = ', tagid, sep=""))
	vary =  sqlQuery(con, paste('select * from Analysis_Outputs where ParameterID=205 AND TagID = ', tagid, sep=""))
	covxy =  sqlQuery(con, paste('select * from Analysis_Outputs where ParameterID=208 AND TagID = ', tagid, sep=""))

if(length(covxy[,1])==0){
covxy = data.frame(ParameterValue= rep(0, nrow(varx)))
# covxy$ParameterValue = rep(0, nrow(varx))
}
	
	dat = data.frame(Year = track$Year, Month = track$Month, Day = track$Day, V11 = varx$ParameterValue, V12 = covxy$ParameterValue, V21 = covxy$ParameterValue, V22 = vary$ParameterValue, Lon = track$Lon, Lat = track$Lat)
	dat
}

prepb.wc =  function (fit = fit, tbpath = "C:/Ben/UNH/TAGS/TAGBASE/Tagbase4-9BFT.mdb", 
    pttid = "91288", year = NULL, ...) 
{
    require(RODBC)
    maxzq = paste("SELECT Proc_WC_Observations.TagID, DateValue([DateTime]) AS dstamp, Min(Proc_WC_Observations.VariableValue) AS zmin, Max(Proc_WC_Observations.VariableValue) AS zmax\nFROM Proc_WC_Observations\nWHERE (((Proc_WC_Observations.VariableID) In (30,32)))\nGROUP BY Proc_WC_Observations.TagID, DateValue([DateTime]);", 
        sep = "")
    maxtq = paste("SELECT Proc_WC_Observations.TagID, DateValue([DateTime]) AS dstamp, Min(Proc_WC_Observations.VariableValue) AS zmin, Max(Proc_WC_Observations.VariableValue) AS zmax\nFROM Proc_WC_Observations\nWHERE (((Proc_WC_Observations.VariableID) In (30,32)))\nGROUP BY Proc_WC_Observations.TagID, DateValue([DateTime]);", 
        sep = "")
    con = odbcConnectAccess(tbpath)
    taginfo = sqlFetch(con, "TagInfo")
    taginfo = sqlFetch(con, "TagInfo")
    attach(taginfo)
    tagidx = TagPTTID == pttid
    if (!is.null(year)) {
        tyear = as.numeric(format(DateTime_Deploy, "%Y"))
        tagidx = year == tyear & TagPTTID == pttid
    }
    tagid = TagID[tagidx]
    day0b = DateTime_Deploy[tagidx]
    dayTb = as.POSIXct(trunc(DateTime_Popoff[tagidx], "day"))
    day0 = as.date(as.numeric(as.Date(day0b) + 365.25 * 10 + 
        1))
    dayT = as.date(as.numeric(as.Date(dayTb) + 365.25 * 10 + 
        1))
    x0 = rev(c(Lon_Deploy, Lat_Deploy)[tagidx])
    xT = rev(c(Lon_Popoff, Lat_Popoff)[tagidx])
    detach(taginfo)
    mmzall = sqlQuery(con, maxzq)
    zidx = mmzall[, 1] == tagid
    mmz = mmzall[mmzall$TagID == tagid, 2:4]
    mmz = mmz[mmz[, 1] <= dayTb, ]
    mmz[, 1] = as.POSIXct((trunc(mmz[, 1], "day")), origin = "1970-1-1", 
        tz = "GMT")
    fitdate = date.mdy(fit$date)
    fmat = data.frame(year = fitdate[[3]], month = fitdate[[1]], 
        day = fitdate[[2]], fit$var.most.prob.track, fit$most.prob.track, 
        mmz = 0, mmt = 0)
    names(fmat) = c("year", "month", "day", "V11", "V12", "V21", 
        "V22", "lon", "lat", "maxz", "sst")
    fmat = thin.trackit.dates(fmat)
	names(fmat) = c("year", "month", "day", "V11", "V12", "V21", 
        "V22", "lon", "lat", "maxz", "sst")
    # didx = na.omit(match(mmz[, 1], ISOdate(fmat[, 1], fmat[, 
        # 2], fmat[, 3], 0, 0, 0, tz = "GMT")))
	names(mmz)[1] = 'date'	
	fmat$date = ISOdate(fmat[, 1], fmat[, 2], fmat[, 3], 0, 0, 0, tz = "GMT")
	fmat = merge(fmat, mmz, 'date', all=T)
	fmat$maxz = -abs(fmat$zmax)
	fmat$maxz[is.na(fmat$maxz)] = 0
	fmat = fmat[!is.na(fmat$year),1:11]
	# didx = which(!is.na(match(mmz[, 1], ISOdate(fmat[, 1], fmat[, 2], fmat[, 3], 0, 0, 0, tz = "GMT"))))
    # fmat$maxz[didx] = -abs(mmz[, 3])
    fmat$data.name = pttid
    class(fmat) = c("btrack", "data.frame")
    fmat
}

 thin.trackit.dates = function(fmat) {
        temp = fmat
        tdate = temp[, 1:3]
        tdate = mdy.date(tdate[, 2], tdate[, 3], tdate[, 1])
        tfmat = as.data.frame(matrix(NA, length(unique(tdate)), 
            11))
        for (i in 1:length(unique(tdate))) {
            tidx = which(tdate == unique(tdate)[i])
            if (length(tidx) > 1) {
                tfmat[i, 1:3] = temp[tidx[1], 1:3]
                tfmat[i, 4:9] = apply(temp[tidx:(tidx + length(tidx) - 
                  1), 4:9], 2, mean)
                tfmat[i, 10:11] = temp[tidx[1], 10:11]
            }
            else {
                tfmat[i, ] = temp[tidx[1], ]
            }
            names(tfmat) = c("year", "month", "day", "V11", "V12", 
                "V21", "V22", "Lon", "Lat", "max_depth", "sst")
        }
        tfmat
    }


