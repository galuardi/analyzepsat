# interpolation using CRAWL for ukfit, trackit and btrack style objects.


ifit <- function(ukfit, psat = NULL, bath = bath, plot = F, getgmt = F, map=map, cilev = .95, hemi=c('E', 'W'), ...){
require(crawl)
require(analyzepsat)
#trackit routine throws off days by one in some cases
if(!is.null(psat)){
if(class(ukfit)=='kfsst'||class(ukfit)=='kftrack'){
	d1 = mdy.date(ukfit$date[1,2],ukfit$date[1,3],ukfit$date[1,1])
	dateoff = psat$day0-d1
	ukfit$date = ukfit$date+dateoff
	}
if(class(ukfit)=='trackit'){
	d1 = ukfit$date[1]
	dateoff = psat$day0-d1
	ukfit$date = ukfit$date+dateoff
	}
}
# if(class(ukfit)=='btrack'){
	# d1 = mdy.date(ukfit[1,2],ukfit[1,3],ukfit[1,1])
	# dateoff = psat$day0-d1
	# ukfit$date = ukfit$date+dateoff
	# }

# need only one location/day
thin.trackit.dates = function(fmat){
temp = fmat
tdate=temp[,1:3]
tdate=mdy.date(tdate[,2],tdate[,3],tdate[,1])
tfmat=as.data.frame(matrix(NA,length(unique(tdate)),11))
	for(i in 1:length(unique(tdate))){
		tidx=which(tdate==unique(tdate)[i])
		if(length(tidx)>1){
			tfmat[i,1:3]=temp[tidx[1],1:3]
			tfmat[i,4:9] = apply(temp[tidx:(tidx+length(tidx)-1),4:9],2,mean)
			# tfmat[i,4:9]=(temp[tidx[1],4:9]+temp[tidx[2],4:9])/length(tidx)
			tfmat[i,10:11]=temp[tidx[1],10:11]
		}else{
			tfmat[i,]=temp[tidx[1],]
		}
	names(tfmat)=c('year','month','day','V11','V12','V21','V22','Lon','Lat','max_depth','sst')	
	}
tfmat
}

date.match = function(d1, d2){
 dvec1 = mdy.date(d1$month, d1$day, d1$year)
 dvec2 = mdy.date(d2$month, d2$day, d2$year)
 idx = match(dvec1, dvec2)
 idx
}


# for one fit from a KF object and the corresponding 'xtrack' dataframe. We will add the sst (maxTemp) and max depth later
if(class(ukfit)=='trackit'){
	dates = date.mdy(ukfit$date)
	attach(dates)
	tmptrack = as.data.frame(cbind(year,month, day, ukfit$var.most.prob.track, ukfit$most.prob.track, 0, 0))
	detach(dates)
	names(tmptrack)[4:9] = c('V11','V12','V21','V22', 'Lon','Lat')
	tmptrack = thin.trackit.dates(tmptrack)
}
if(class(ukfit)=='kftrack'||class(ukfit)=='kfsst'){
	#attach(ukfit$date)
	ukfit$date = as.data.frame(ukfit$date)
	tmptrack = as.data.frame(cbind(ukfit$date$year, ukfit$date$month, ukfit$date$day, ukfit$var.most.prob.track, ukfit$most.prob.track, 0, 0))
	#detach(ukfit$date)
	names(tmptrack)[1:9] = c('year','month','day','V11','V12','V21','V22', 'Lon','Lat')
	tmptrack = thin.trackit.dates(tmptrack)
}
if(class(ukfit)=='btrack'){
	tmptrack = ukfit
	names(tmptrack)[4:9] = c('V11','V12','V21','V22', 'Lon','Lat')
	tmptrack = thin.trackit.dates(tmptrack)
}


#==============================================================================================#	
# Generate a CRW fit object
#==============================================================================================#	
{
# index of points we want to keep
# if using a 'badidx' object, it can be included here..
if(hemi=='W') {
 tmptrack$Lon = tmptrack$Lon%%-180   # should put things in -180 to 180... at least for western hemi!
}
keepidx = sapply(1:nrow(tmptrack), function(i) .get.bath(tmptrack$Lon[i], tmptrack$Lat[i],bath))<0
tmptrack_o=tmptrack

# try taking out land points just after ukf routine and before anything else...
tmptrack = tmptrack[keepidx,]  

# our dates from ukf fit
if(class(ukfit)=='btrack'){
	tmptrack$time = as.numeric(mdy.date(tmptrack$month,tmptrack$day,tmptrack$year))
}else{
	tmptrack$time = as.numeric(mdy.date(tmptrack$month,tmptrack$day,tmptrack$year))
}

# object for CRW fit (single)	
crwfit = NULL



#start point. not sure the 0-360 matters but...
# tmptrack$Lon = tmptrack$Lon%%360
slon = tmptrack$Lon[1]
slat = tmptrack$Lat[1]

#initial latitude variance
latdrift = 1
#==============================================================================================#	
# run a while loop to make sure things are valid in the end..
	while(class(crwfit)!="crwFit"){
		print(latdrift)
	# State starting values
	initial.drift <- list(a1.x=c(slon, 0, 0), a1.y=c(slat, 0, 0),
		              P1.x=diag(c(0, 0.001, 0.001)),
		              P1.y=diag(c(0, latdrift, latdrift)))

	##Fit random drift model
	V11 = tmptrack[,4]
	V22 = tmptrack[,7]
	## the fixPar flags make the difference here. Still not sure why..
	crwfit <- try(crwMLE(mov.model=~1, err.model=list(x=~V11, y=~V22), drift.model=TRUE,
		      data=tmptrack, coord=c("Lon", "Lat"),
		      Time.name="time", initial.state=initial.drift, polar.coord=TRUE, fixPar = c(0,NA,0,NA,0,0,0,0),         
		      control=list(maxit=2000,trace=1, REPORT=10), attempts = 10),silent=T) #c(0,1,0,1.5,0,0,0,0)
	  latdrift = latdrift+.5
}

# the final crw fit.
crwfit
}
#==============================================================================================#	
# Predict from the CRW fit
#==============================================================================================#
{	

##Make daily location predictions
fit = crwfit

	# tmptrack = prepb(ukfit, xtrack)
	# tmptrack$time = as.numeric(mdy.date(tmptrack[,2],tmptrack[,1],tmptrack[,3]))

# vector of predicted times, equal to the total track length with no missing days.
predTime <- seq(ceiling(min(tmptrack$time)), floor(max(tmptrack$time)), 1)

# predict based on crwfit using predicted times
predObj <- crwPredict(object.crwFit = fit, predTime, speedEst = TRUE, flat = TRUE)

# find where we have our missing days
idx = which(is.na(predObj$Lon))

## Make a new data frame for storing interpolated values
ntrack = predObj[,3:13]

# use full date range
ntrack[,1:3] = cbind(date.mdy(predTime)[[3]],date.mdy(predTime)[[1]],date.mdy(predTime)[[2]])

# fill in the missing days with the predicted locations
ntrack[idx,8] = predObj$mu.x[idx]
ntrack[idx,9] = predObj$mu.y[idx]

# be consistent with 0-360 stuff... 
ntrack$Lon = ntrack$Lon

# CRAWL uses srqt(diag(var)) for stndard error here... so, reconvert for variance
ntrack[idx,4] = predObj$se.mu.x[idx]^2
ntrack[idx,7] = predObj$se.mu.y[idx]^2

# forget about covariance.. CRAWL does something different
ntrack[idx,5:6] = 0

}

#==============================================================================================#	
# Steps for bathymetric correction following CRW predict
#==============================================================================================#
{

# use the PSAT object usually generated by MWTextract (analyzepsat)
# build an object for bathymetric correction

# fill in the sst and max depth..
if(!is.null(psat)){
 attach(psat)
 trdates = fulldates
 if(any(class(fulldates)=='POSIXct')) trdates = mdy.date(as.numeric(format(fulldates,'%m')), as.numeric(format(fulldates,'%d')),as.numeric(format(fulldates,'%Y')))
 didx = match(trdates, predObj$time)
 
if(exists('mmt', mode = 'any')){
	ntrack$max_depth = mmz[didx,3]
	ntrack$sst =  mmt[didx,3]
	}else{
	ntrack$sst = apply(T,1,max, na.rm=T)[didx]
	ntrack$max_depth = apply(Z,1,min, na.rm=T)[didx]
	}
 detach(psat)
}else{
# fill in missing SST via local polynomial regression
# add the temperature and depth back from the original object
idx = date.match(tmptrack_o, ntrack)
ntrack$sst[idx] = tmptrack_o$sst
ntrack$max_depth[idx] = tmptrack_o$max_depth
ntrack$sst = .fill.vals(ntrack$sst)
}
ntrack$max_depth[is.na(ntrack$max_depth)] = 0
ntrack$max_depth[is.infinite(ntrack$max_depth)] = 0

# bathymetric correction...
cilev = cilev
nbtrack=NULL
while(class(nbtrack)!='data.frame'){
	nbtrack = try(make.btrack(ntrack, bath, ci = cilev), silent = T)
	cilev = cilev+.5
  }
    
#save it 
int.track = nbtrack
 }
#==============================================================================================#	
# plots
#==============================================================================================#	

if(plot==T){

# uncomment to generate a pdf for exploration of results
# pdf(height = 8, width = 12, file = 'interpolatedtracks.pdf')

# spatial polygons from analyzepsat
data(ATL)
# and...Anders GMT stuff
if(getgmt==T){
map = plotmap(-100, 20, -30, 60, res=3, save = T, grid = F, seacolor = NULL, landcolor ='khaki') ; 
dev.off(); 
map[,1] = map[,1]+360
}else{
 map=NULL
}
#1
crwPredictPlot(predObj, 'll');title(ukfit$data.name[1])
#2
# x11()
par(mfrow=c(1,3))
crwPredictPlot(predObj, 'map');title(ukfit$data.name[1])
if(getgmt==T) polygon(map)
segments(predObj$mu.x[idx]+predObj$se.mu.x[idx]*2, predObj$mu.y[idx], predObj$mu.x[idx]-predObj$se.mu.x[idx]*2, predObj$mu.y[idx], col=2)
segments(predObj$mu.x[idx],predObj$mu.y[idx]+predObj$se.mu.y[idx]*2, predObj$mu.x[idx], predObj$mu.y[idx]-predObj$se.mu.y[idx]*2, col=2)
segments(predObj$mu.x[idx],predObj$mu.y[idx]+predObj$se.mu.y[idx], predObj$mu.x[idx], predObj$mu.y[idx]-predObj$se.mu.y[idx], col=4, lwd=2)
segments(predObj$mu.x[idx]+predObj$se.mu.x[idx], predObj$mu.y[idx], predObj$mu.x[idx]-predObj$se.mu.x[idx], predObj$mu.y[idx], col=4, lwd=2)
lines(predObj$mu.x, predObj$mu.y, lwd=2, col=2)
points(predObj$mu.x[idx], predObj$mu.y[idx], col=3, pch = 19, cex=1)
#3
plot(ntrack[,8], ntrack[,9],typ='o')
polygon(map, col = 'khaki')
title('pre bath')
#4
plot.btrack(nbtrack, add=F, map = map2,...)
polygon(map, col = 'khaki')
box(lwd=2)
}
nbtrack
}
 
 