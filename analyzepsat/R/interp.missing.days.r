
#===================================================================================================================================#
# interpolate for missing days
#===================================================================================================================================#

# match ukfit dates with xtrack dates
.fill.vals <- function (vec, span = 0.25) 
{
    len = length(vec)
    vlen = 1:len
   # fidx = (vec < 5) 
	fidx1 = is.infinite(vec) 
	fidx2 = is.na(vec)
	fidx3 = is.nan(vec)
	fidx = as.logical(fidx1+fidx2+fidx3)
    vec[fidx] = NA
    if (any(fidx == T)) {
        ltmp = loess(vec ~ vlen, span = span)
		#ltmp = locfit(vec ~ vlen, alpha = span)
        vec[fidx] = predict(ltmp, newdata = vlen[fidx])
    }
    vec
}

interp.ll <- function(tag, ukfit, btrack =NULL, span = c(.25,.75)){  # span[1] is for points, span[2] is for cov.
	mwt = tag$MWTxy 
	udate = ukfit$date
	udate = mdy.date(udate[,2],udate[,3],udate[,1])
	mdate = mdy.date(mwt[,2],mwt[,3],mwt[,1])
	uidx = match(udate, mdate)
	ntrack = data.frame(mwt[,1:3], V11=0, V12=0,V21=0,V22=0,lon = NA, lat = NA,maxz = apply(tag$Z, 1, min, na.rm = T),maxt = apply(tag$T, 1, min, na.rm = T))
	if(!is.null(btrack)){
		ntrack[uidx,4:9] = btrack[,4:9]
		ntrack[,8] = ntrack[,8]+360
	}else{
		ntrack[uidx,4:9] = cbind(ukfit$var.most.prob.track, ukfit$most.prob.track)
		}
	lon = ntrack[,8]
	lat = ntrack[,9]
	# longitude loess
	nidx = which(is.na(ntrack[,8]))
	
	if(length(nidx)>0){
		len = 1:nrow(ntrack)
		mod1 = loess(lon~len,span = span[1])

		# latitude loess
		nidx = which(is.na(ntrack[,9]))
		len = 1:nrow(ntrack)
		mod2 = loess(lat~len,span = span[1])

		# interpolate covaariance estimates
		attach(ntrack)
		mod3 = loess(V11~len,span = span[2])
		mod4 = loess(V12~len,span = span[2])
		mod5 = loess(V21~len,span = span[2])
		mod6 = loess(V22~len,span = span[2])
		detach(ntrack)
		temp = cbind(predict(mod3,len[nidx]),predict(mod4,len[nidx]),predict(mod5,len[nidx]),predict(mod6,len[nidx]),predict(mod1,len[nidx]),predict(mod2,len[nidx]))
		temp[,4] = abs(temp[,4])
		temp[,7] = abs(temp[,7])
		ntrack[nidx,4:9] = temp
	}
	ntrack[,8]=ntrack[,8]-360
	ntrack[,10] = apply(tag$Z, 1, min, na.rm=T)
	zidx = is.infinite(ntrack[,10])
	ntrack[zidx,10] = 0
	ntrack[,11] = apply(tag$T, 1, max, na.rm=T)
	ntrack[,11] = .fill.vals(ntrack[,11])
	ntrack
}

# start points for linear interp along a vector with missing data
# i=8
	# tag = psat[[i]]
	# ukfit=ukfitsb[[i]]
	# btrack = btracks.r[[i]]
	
# from Wikipedia
# \textbf{P}_{k|k-1} = \textbf{F}_{k} \textbf{P}_{k-1|k-1} \textbf{F}_{k}^{\text{T}} + \textbf{Q}_{k} 


interp.ll.kf <- function(tag, ukfit, btrack =NULL, span = c(.25,.75)){  # span[1] is for points, span[2] is for cov.
	mwt = tag$MWTxy 
	udate = ukfit$date
	udate = mdy.date(udate[,2],udate[,3],udate[,1])
	mdate = mdy.date(mwt[,2],mwt[,3],mwt[,1])
	uidx = match(udate, mdate)
	ntrack = data.frame(mwt[,1:3], V11=0, V12=0,V21=0,V22=0,lon = NA, lat = NA,maxz = apply(tag$Z, 1, min, na.rm = T),maxt = apply(tag$T, 1, min, na.rm = T))
	if(!is.null(btrack)){
		ntrack[uidx,4:9] = btrack[,4:9]
		ntrack[,8] = ntrack[,8]+360
	}else{
		ntrack[uidx,4:9] = cbind(ukfit$var.most.prob.track, ukfit$most.prob.track)
		}

#===============================================================================================#
# fill lat
#===============================================================================================#
naidx = which(is.na(ntrack[,9]))
if(length(naidx) > 0){
ntrack[naidx,8] = NA
sidx = (naidx)[c(0,diff(naidx))!=1]-1
eidx = rev((rev(naidx))[c(0,diff(rev(naidx)))!=-1]+1)
fill.len = eidx-sidx-1
	for(j in 1:length(sidx)){
		#fill.len[j]
		first = seq(ntrack[sidx[j],9], ntrack[eidx[j],9], length = fill.len[j])
		second = seq(ntrack[eidx[j],9], ntrack[sidx[j],9], length = fill.len[j])
		fidx = which(naidx>sidx[j]&naidx<eidx[j])
		ntrack[naidx[fidx],9]= apply(cbind(first,second),1,min,na.rm=T)
	}
}

#===============================================================================================#		
# fill lon
#===============================================================================================#
naidx = which(is.na(ntrack[,8]))
if(length(naidx) > 0){
sidx = (naidx)[c(0,diff(naidx))!=1]-1
eidx = rev((rev(naidx))[c(0,diff(rev(naidx)))!=-1]+1)
fill.len = eidx-sidx-1
	for(j in 1:length(sidx)){
		#fill.len[j]
		first = seq(ntrack[sidx[j],8], ntrack[eidx[j],8], length = fill.len[j])
		second = seq(ntrack[eidx[j],8], ntrack[sidx[j],8], length = fill.len[j])
		fidx = which(naidx>sidx[j]&naidx<eidx[j])
		ntrack[naidx[fidx],8] = apply(cbind(first,second),1,min,na.rm=T)
	}
}

#===============================================================================================#
# Kalman update covariance
#===============================================================================================#

FF = matrix(c(1,0,0,1),2,2)
Q = matrix(c(.5,0,0,.5),2,2)
cc = abs(rnorm(t(c(0,0)),sqrt(Q)))

k.update = function(P1, FF, cc){
  #cc = abs(rnorm(t(c(0,0)),sqrt(Q)))
  FF*P1*t(FF)+cc
}

len = 2:(nrow(ntrack)-1)
naidxc = which(ntrack[len,4]==0)
tvec1 = tvec2 = tvec = ntrack[len,4:7]

for(j in naidxc){
	P1 = as.numeric(tvec2[j-1,])
	P1 = matrix(P1, 2,2)
	P = as.numeric(k.update(P1,FF,Q))
	tvec1[j,] = P
}

for(j in rev(naidxc)){
	P1 = as.numeric(tvec1[j+1,])
	P = as.numeric(k.update(P1,FF,Q))
	tvec2[j,] = P
}

for(i in 1:4){
 tvec[,i] = abs(apply(cbind(tvec1[,i],tvec2[,i]),1,min,na.rm=T))
}

ntrack[len,4:7] = tvec
	ntrack[,8]=ntrack[,8]-360
	ntrack[,10] = apply(tag$Z, 1, min, na.rm=T)
	zidx = is.infinite(ntrack[,10])
	ntrack[zidx,10] = 0
	ntrack[,11] = apply(tag$T, 1, max, na.rm=T)
	ntrack[,11] = .fill.vals(ntrack[,11])
	ntrack
}	

# use this one for MWT data with minmax data only
interp.ll.kf.minmax <- function(tag, ukfit, btrack =NULL, span = c(.25,.75)){  # span[1] is for points, span[2] is for cov.
	mwt = tag$MWTxy 
	udate = ukfit$date
	udate = mdy.date(udate[,2],udate[,3],udate[,1])
	#mdate = mdy.date(mwt[,2],mwt[,3],mwt[,1])
	mdate = seq(tag$day0, tag$dayT)
	uidx = match(udate, mdate)
	midx = match(mdate, udate)
	ntrack = as.data.frame(matrix(0, nrow= length(mdate), ncol = 11))
	names(ntrack) = c('Year','Month','Day','V11','V12','V21','V22','lon','lat','maxz','maxt')
	ntrack[,1:3] = cbind(date.mdy(mdate)[[3]],date.mdy(mdate)[[1]],date.mdy(mdate)[[2]])
	ntrack$lon=ntrack$lat=NA
	ntrack$maxz = tag$mmz[,3]
	ntrack$maxt = tag$mmt[,3]
	if(!is.null(btrack)){
		bdate =  mdy.date(btrack[,2],btrack[,3],btrack[,1])
	    bidx = match(bdate, mdate)
		ntrack[bidx,4:9] = btrack[,4:9]
		ntrack[,8] = ntrack[,8]+360
	}else{
		ntrack[uidx,4:9] = cbind(ukfit$var.most.prob.track, ukfit$most.prob.track)
		}

#===============================================================================================#
# fill lat
#===============================================================================================#
naidx = which(is.na(ntrack[,9]))
if(length(naidx) > 0){
ntrack[naidx,8] = NA
sidx = (naidx)[c(0,diff(naidx))!=1]-1
eidx = rev((rev(naidx))[c(0,diff(rev(naidx)))!=-1]+1)
fill.len = eidx-sidx-1
	for(j in 1:length(sidx)){
		#fill.len[j]
		first = seq(ntrack[sidx[j],9], ntrack[eidx[j],9], length = fill.len[j])
		second = seq(ntrack[eidx[j],9], ntrack[sidx[j],9], length = fill.len[j])
		fidx = which(naidx>sidx[j]&naidx<eidx[j])
		ntrack[naidx[fidx],9]= apply(cbind(first,second),1,min,na.rm=T)
	}
}

#===============================================================================================#		
# fill lon
#===============================================================================================#
naidx = which(is.na(ntrack[,8]))
if(length(naidx) > 0){
sidx = (naidx)[c(0,diff(naidx))!=1]-1
eidx = rev((rev(naidx))[c(0,diff(rev(naidx)))!=-1]+1)
fill.len = eidx-sidx-1
	for(j in 1:length(sidx)){
		#fill.len[j]
		first = seq(ntrack[sidx[j],8], ntrack[eidx[j],8], length = fill.len[j])
		second = seq(ntrack[eidx[j],8], ntrack[sidx[j],8], length = fill.len[j])
		fidx = which(naidx>sidx[j]&naidx<eidx[j])
		ntrack[naidx[fidx],8] = apply(cbind(first,second),1,min,na.rm=T)
	}
}

#===============================================================================================#
# Kalman update covariance
#===============================================================================================#

FF = matrix(c(1,0,0,1),2,2)
Q = matrix(c(.5,0,0,.5),2,2)
cc = abs(rnorm(t(c(0,0)),sqrt(Q)))

k.update = function(P1, FF, cc){
  #cc = abs(rnorm(t(c(0,0)),sqrt(Q)))
  FF*P1*t(FF)+cc
}

len = 2:(nrow(ntrack)-1)
naidxc = which(ntrack[len,4]==0)
tvec1 = tvec2 = tvec = ntrack[len,4:7]

for(j in naidxc){
	P1 = as.numeric(tvec1[j-1,])
	P1 = matrix(P1, 2,2)
	P = as.numeric(k.update(P1,FF,Q))
	tvec1[j,] = P
}

for(j in rev(naidxc)){
	P1 = as.numeric(tvec2[j+1,])
	P = as.numeric(k.update(P1,FF,Q))
	tvec2[j,] = P
}

for(i in 1:4){
 tvec[,i] = abs(apply(cbind(tvec1[,i],tvec2[,i]),1,min,na.rm=T))
}

	ntrack[len,4:7] = tvec
	ntrack[,8]=ntrack[,8]-360
	#ntrack[,10] = apply(tag$Z, 1, min, na.rm=T)
	zidx = is.infinite(ntrack[,10])
	ntrack[zidx,10] = 0	
	zidx = is.na(ntrack[,10])
	ntrack[zidx,10] = 0
	#ntrack[,11] = apply(tag$T, 1, max, na.rm=T)
	zidx = is.na(ntrack[,11])
	ntrack[zidx,11] = -1
	ntrack[,11] = .fill.vals(ntrack[,11])
	for(i in 1:11) {iidx = is.infinite(ntrack[,i]); ntrack[iidx,i] = 0}
	
	ntrack
}	

	# Kalman update position
# \hat{\textbf{x}}_{k|k-1} = \textbf{F}_{k}\hat{\textbf{x}}_{k-1|k-1} + \textbf{B}_{k} \textbf{u}_{k} 
	# j =306
	# x1 = as.numeric(ntrack[j-1,8:9])
	# x1 = matrix(x1, 2,1)
	# FF = matrix(c(1,0,0,1),2,2)
	# B = matrix(c(.1,.25),1,2)


	# for(i in 1:10){
	# xx = t(x1)%*%FF%*%t(FF)+B
	# points(xx, pch=19, col=2)
	# x1 = t(xx)
	# }

