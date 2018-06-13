
#' tb2psat
#'
#' @param tbpath 
#' @param pttid 
#' @param recovered 
#' @param is.ptt 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
tb2psat <- function(tbpath = 'C:\\Ben\\UNH\\TAGS\\TAGBASE\\Tagbase4-9BFT.mdb', pttid = "55288", recovered=F, is.ptt=T, ...){
require(RODBC)
con = odbcConnectAccess(tbpath)

print(paste("Processing Tag# ", pttid, sep=""))

#==============================================================#
# get basics: tag deploy and popoff info
#==============================================================#
print("Reading tagging data")

taginfo=sqlFetch(con, 'TagInfo')
# attach(taginfo)
	if(is.ptt==T){
		# taginfo = sqlFetch(con, "TagInfo")
		tagidx = taginfo$TagPTTID == pttid
		tagid = taginfo$TagID[tagidx]
	}else{
	 tagidx = pttid==taginfo$TagID
	 tagid = taginfo$TagID[tagidx]
	}
day0b = as.POSIXct(trunc(as.POSIXlt(taginfo$DateTime_Deploy[tagidx], tz='GMT'),'day'))
dayTb = as.POSIXct(trunc(as.POSIXlt(taginfo$DateTime_Popoff[tagidx], tz='GMT'),'day'))
day0 = as.date(as.numeric(as.Date(day0b)+365.25*10+1))
dayT = as.date(as.numeric(as.Date(dayTb)+365.25*10+1))
fulldates = as.POSIXct(trunc(seq(day0b, dayTb, 'day'),'day'))
x0 = rev(c(taginfo$Lon_Deploy,taginfo$Lat_Deploy)[tagidx])
xT = rev(c(taginfo$Lon_Popoff, taginfo$Lat_Popoff)[tagidx])
# detach(taginfo)
rm(taginfo)

#==============================================================#
# get MWT locations using tagid in previous step
#==============================================================#
print("Reading MWT produced locations")

mwtq = paste('SELECT Proc_MT_Location.TagID, Proc_MT_Location.DateTime, Proc_MT_Location.Longitude, Proc_MT_Location.Latitude FROM Proc_MT_Location WHERE (([TagID]=',tagid,'));', sep = "")
MWTxy = sqlQuery(con, mwtq)[,2:4]
MWTxy = MWTxy[MWTxy[,1]<=dayTb,]
MWTxy = data.frame(Year=as.numeric(format(MWTxy[,1],'%Y')), Month = as.numeric(format(MWTxy[,1],'%m')), Day = as.numeric(format(MWTxy[,1],'%d')), Lat = MWTxy[,3], Lon = MWTxy[,2])
MWTxy[1,4:5] = (x0)
MWTxy[nrow(MWTxy),4:5] = (xT)

#==============================================================#
# get temperature matrix
#==============================================================#
print("Reading temperature data")

tq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=2) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=1));',sep="")
tres = sqlQuery(con, tq)
popidx = tres[,1]<(dayTb+86400)
tres = tres[popidx,]
tres = tres[!is.na(tres[,1]),]

tmat = matrix(NaN, 96, length(fulldates))
tdates = seq(day0b, dayTb+86400, by=15*60) #[1:prod(dim(tmat))]
tidx = (match(tres[,1], tdates))
# tidx = na.omit(tidx)
tmat[tidx] = tres[,2]
tmat = t(tmat)

#==============================================================#
# get depth matrix
#==============================================================#
print("Reading pressure data")

pq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=1) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=1));',sep="")
pres = sqlQuery(con, pq)
popidx = pres[,1]<(dayTb+86400)
pres = pres[popidx,]
pres = pres[!is.na(pres[,1]),]

pmat = matrix(NaN, 96, length(fulldates))
pdates = seq(day0b, dayTb+86400, by=15*60) #[1:prod(dim(pmat))]
pidx = (match(pres[,1], pdates))
pmat[pidx] = pres[,2]*-1
pmat = t(pmat)

#==============================================================#
# get minmax temps
#==============================================================#
print("Reading minmax Temperature")
#### Need to match dates.... 
mintq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=2) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=201));',sep="")

maxtq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=2) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=202));',sep="")

mint = sqlQuery(con, mintq)
maxt = sqlQuery(con, maxtq)
mmt = data.frame(Date = mint[,1], mint = mint[,2], maxt = maxt[,2])
mmt[,1] = as.POSIXct(trunc(as.POSIXlt(mmt[,1], tz='GMT'),'day'))

popidx = mmt[,1]<=(dayTb)
mmt = mmt[popidx,]
mmt = mmt[!is.na(mmt[,1]),]
mmt = mmt[order(mmt[,1]),]

tdates = as.POSIXct(trunc(seq(day0b, dayTb, by='day'),'day'))
tidx = (match(mmt[,1], tdates))
tidx=na.omit(tidx)
mmtt = data.frame(Date = tdates, mint = NA, maxt=NA)
mmtt[tidx,2:3] = mmt[,2:3]

mmt = mmtt

#==============================================================#
# get minmax depth
#==============================================================#
print("Reading minmax Depth")
#### Need to match dates.... 
minpq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=1) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=201));',sep="")

maxpq = paste('SELECT Proc_MT_DepthTemp.DateTime, Proc_MT_DepthTemp.VariableValue_Geophysical
FROM Proc_MT_DepthTemp
WHERE (((Proc_MT_DepthTemp.ObservedVariableID)=1) AND ((Proc_MT_DepthTemp.TagID)=',tagid,') AND ((Proc_MT_DepthTemp.ObservationModeID)=202));',sep="")

minp = sqlQuery(con, minpq)
maxp = sqlQuery(con, maxpq)
mmz = data.frame(Date = minp[,1], minz = minp[,2]*-1, maxz = maxp[,2]*-1)
mmz[,1] = as.POSIXct(trunc(as.POSIXlt(mmz[,1], tz='GMT'),'day'))

popidx = mmz[,1]<=(dayTb)
mmz = mmz[popidx,]
mmz = mmz[!is.na(mmz[,1]),]
mmz = mmz[order(mmz[,1]),]

pdates = as.POSIXct(trunc(seq(day0b, dayTb, by='day'),'day'))
pidx = (match(mmz[,1], pdates))
pidx = na.omit(pidx)
mmpt = data.frame(Date = pdates, minp = NA, maxp=NA)
mmpt[pidx,2:3] = mmz[,2:3]

mmz = mmpt

#==============================================================#
# For Recovered tags only!
#==============================================================#

if(recovered==T){
print("Reading recovered data")
	arq = paste('SELECT Proc_Archival_TempLight.TagID, Proc_Archival_TempLight.DateTime, Proc_Archival_TempLight.Depth, Proc_Archival_TempLight.Temperature, Proc_Archival_TempLight.[Light Level], Proc_Archival_TempLight.IntTemp, Proc_Archival_TempLight.SurfaceLight, Proc_Archival_TempLight.CorrectDepth, Proc_Archival_TempLight.DayPart
FROM Proc_Archival_TempLight
WHERE (((Proc_Archival_TempLight.TagID)=',tagid,'))
ORDER BY Proc_Archival_TempLight.DateTime;', sep = "")

arch = sqlQuery(con, arq)
arch$DateTime =  as.POSIXct(as.POSIXlt(arch$DateTime, tz='GMT'))
arch$Depth = arch$Depth*-1
mmdates = as.POSIXct(trunc(arch$DateTime, 'day'))
mint = tapply(arch$Temperature, mmdates, min)
maxt = tapply(arch$Temperature, mmdates, max)
minz =  tapply(arch$Depth, mmdates, max)
maxz =  tapply(arch$Depth, mmdates, min)
mmt = data.frame(unique(mmdates), mint, maxt)
mmz =  data.frame(unique(mmdates), minz, maxz)

if(is.na(arch$IntTemp[1])){
tzmat = arch[,2:5]
names(tzmat)=c('date','depth','temp','light')
}else{
tzmat = arch[,2:10]
names(tzmat)=c('date','depth','temp','light', 'temp_int','surfacelight','correctdepth','daypart')
}

tmat = NULL
zmat = NULL

}



#==============================================================#
# Get Argos Popoff and floating Data
#==============================================================#
aq = paste("SELECT Proc_MT_GPSdata.DateTime, Proc_MT_GPSdata.Longitude, Proc_MT_GPSdata.Latitude, Proc_MT_GPSdata.LocationClass
FROM Proc_MT_GPSdata
WHERE (((Proc_MT_GPSdata.TagID)=",tagid,") AND ((Proc_MT_GPSdata.DeployPopoffStatus)='Popoff'));",sep="")

adata = sqlQuery(con, aq)

dataout = list(tagID = pttid, x0 = x0, day0 = day0, day0b = day0b, 
	xT = xT, dayT = dayT, dayTb = dayTb, fulldates = fulldates, 
    MWTxy = MWTxy, T = tmat, Z = pmat, 
	mmt = mmt, mmz = mmz, Argos = adata)

if(recovered==T) 	dataout = list(tagID = pttid, x0 = x0, day0 = day0, day0b = day0b, 
	xT = xT, dayT = dayT, dayTb = dayTb, fulldates = fulldates, 
    MWTxy = MWTxy, TZL = tzmat,
	mmt = mmt, mmz = mmz, Argos = adata)
	
class(dataout) = c("MWTpsat", "list")
odbcCloseAll()
dataout
}


