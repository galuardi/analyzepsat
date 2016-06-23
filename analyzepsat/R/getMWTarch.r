getMWTarch <- function (xlsfile, tagID, tyear, taglocfile, dayT = NULL, xT = NULL, transmit=F) 
{
    require(RODBC)
    print(paste("Processing tag# ", tagID, "-", tyear, sep = ""))
    xfile = odbcConnectExcel(xlsfile)
    tabs = sqlTables(xfile)$TABLE_NAME
    print("Retrieving Archival Light, Temp and Depth ")
    atabs = tabs[grep("Archival", sqlTables(xfile)$TABLE_NAME)]
    len = length(atabs)
    atabs = atabs[seq(1, len, by = 2)]
    num = 1:length(atabs)
    atabs = paste("Archival Data ", num, sep = "")
    adat = NULL
    for (i in num) {
        print(paste("retrieving archival record ", i, sep = ""))
        temp = sqlFetch(xfile, atabs[i], as.is=1)
        temp = temp[2:nrow(temp), ]
		temp[,1] = as.POSIXct(strptime(temp[,1], format='%Y-%m-%d %H:%M:%S'))
        names(temp) = c("Date", "Tval", "Pval", "light", "extT", 
            "depth")
        adat = rbind(adat, temp)
    }
    names(adat) = c("Date", "Tval", "Pval", "light", "extT", 
        "depth")
    adat$uday = as.POSIXct(trunc(adat$Date, "days"))
	adat = na.omit(adat)
    maxz = tapply(adat$depth, adat$uday, min)
    maxt = tapply(adat$extT, adat$uday, max)
    x0 = .getTaggingLoc(taglocfile, as.numeric(substr(tagID, 
        1, nchar(tagID))), tyear)
    x0 = rev(x0)
    day0 = .getTaggingDay(taglocfile, as.numeric(substr(tagID, 
        1, nchar(tagID))), tyear)
    day0b = ISOdatetime(date.mdy(day0)[[3]], date.mdy(day0)[[1]], 
        date.mdy(day0)[[2]], 0, 0, 0)
    if (!is.null(dayT)) {
        dayTb = .GetDayTb(dayT)
    }
    else {
		if(transmit==T){
			argos = .GetArgosPSAT(xlsfile, odbc = T)
			dayT = .GetLastDay(argos)
			dayTb = .GetDayTb(dayT)
		}
		else{
			argos=NULL			
			dayTb = max(adat$uday)
			dayT =  mdy.date(as.numeric(format(dayTb,'%m')), as.numeric(format(dayTb,'%d')), as.numeric(format(dayTb,'%Y')))
		}
    }
    if (is.null(xT)&transmit==T) {
        argos = .GetArgosPSAT(xlsfile, odbc = T)
        xT = .GetLastLoc(argos)
    }else{
		xT=c(NA, NA)
	}
    print("Reading MWT produced locations")
    MWTxy = .getMWTxy(xlsfile, x0, xT, day0, dayT, odbc = T)
    if(transmit==T) fulldates = seq(day0, dayT) else fulldates = NULL
    print("Reading sunset/sunrise times")
    SRSS = .getSRSS(xlsfile, day0b, dayTb, odbc = T)
    dataout = list(tagID = tagID, x0 = x0, day0 = day0, day0b = day0b, 
        xT = xT, dayT = dayT, dayTb = dayTb, fulldates = fulldates, 
        SRSS = SRSS, MWTxy = MWTxy, LTD = adat, maxt = maxt, 
        maxz = maxz)
    dataout$LTD = na.omit(dataout$LTD)
    mmtdates = as.POSIXct(trunc(as.POSIXct(unique(dataout$LTD$uday)), 
        "day"), tz = "GMT")
    dataout$mmt = data.frame(date = mmtdates, minz = tapply(dataout$LTD$extT, 
        dataout$LTD$uday, min), maxz = tapply(dataout$LTD$extT, 
        dataout$LTD$uday, max))
    dataout$mmz = data.frame(date = mmtdates, minz = tapply(dataout$LTD$depth, 
        dataout$LTD$uday, max), maxz = tapply(dataout$LTD$depth, 
        dataout$LTD$uday, min))
    dataout$maxz = dataout$mmz[, 3]
    dataout$maxt = dataout$mmt[, 3]
    class(dataout) = c("MWTpsat", "list")
    rm(adat, tagID, x0, day0, day0b, xT, dayT, dayTb, fulldates, 
        SRSS, MWTxy, maxt, maxz, atabs, len, num, tabs, i)
    dataout
}
