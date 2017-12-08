
.get.bath.ras <- function(pt, bas){
	require(raster)
	if(as.character(pt@proj4string) != as.character(bas@crs)){
		stop("Projections not equal! Fix and try again.. ")	
	}else{
		raster::extract(bas, pt)
	}
}

make.btrack.ras <- function (fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, 
					npoints = 300, fulldist = T) 
{
	len = length(fmat[, 1])
	ntrack = as.data.frame(matrix(0, len, 6))
	ntrack[1, ] = c(0, 0, 0, 0, fmat[1, 8:9])
	ntrack[len, ] = c(0, 0, 0, 0, fmat[len, 8:9])
	sptmp = NULL
	
	for (i in 2:(length(fmat[, 1]) - 1)) {
		print(paste("Bathymetric point ", i, sep = ""))
		point = as.numeric(fmat[i, ])
		samp = .get.samp(point[4:9], npoints, ci = ci)
		samp = SpatialPoints(samp)
		samp@proj4string = bathy@crs
		samp.bath = .get.bath.ras(samp, bathy)
		# samp.bath = sapply(1:length(samp[, 1]), function(j) .get.bath(samp[j,1], samp[j, 2], bathy))
		sidx = samp.bath <= as.numeric(point[10])
		sidx[is.na(sidx)] = F
		samp = as.data.frame(samp[sidx, ])
		if (length(samp[,1]) < 3) {
			samp = sptmp[[i - 1]]
			samp[,1] = jitter(samp[,1])
			samp[,2] = jitter(samp[,2])  # 5% jitter if we use the same sampling as previous time step
		}
		if (mintype == 2) 
			ntrack[i, 5:6] = .get.min2(ntrack[i - 1, 5], ntrack[i - 1, 6], .denselect(samp)[1], .denselect(samp)[2],  samp)
		if (mintype == 3) 
			ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i +1, 6], ntrack[i - 1, 5], ntrack[i - 1, 6], .denselect(samp)[1],  .denselect(samp)[2], samp)
		if (mintype == 4) 
			ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 	1, 6], .denselect(samp)[1], .denselect(samp)[2],  samp)
		sptmp[[i]] = samp
		x0 = SpatialPoints(matrix(point[8:9], nrow = 1, ncol = 2), proj4string = bathy@crs)
		# b.init = .get.bath(as.numeric(point[8]), as.numeric(point[9]),  bathy)
		b.init = raster::extract(bathy, x0)
		print(c(b.init - as.numeric(point[10])))
		if (b.init <= as.numeric(point[10]) & fulldist == F) {
			ntrack[i, ] = fmat[i, 4:9]
		}else {
			tcov = sqrt(cov(samp))
			tcov[is.nan(tcov)] = 0
			ntrack[i, 1:4] = as.vector(tcov)
		}
	}
	
	btrack = cbind(fmat[, 1:3], ntrack, fmat[, 10:11])
	names(btrack) = c("Year","Month", "Day",  "V11", "V12", "V21", 
										"V22", "Lon_E", "Lat_N", "maxz", "maxt")
	attr(btrack,'Header') = "#Bathymetric corrected track"
	if (save.samp) {
		list(btrack, sptmp)
	}
	else {
		btrack
	}
}