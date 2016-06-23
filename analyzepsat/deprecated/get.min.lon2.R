get.min.lon2 <-
function(lon1,lon2,lat2,samp){           #  lon1 is the kftrack lon; lon2 and lat2 should be from the previous location
  medlat<-median(samp[,2])
  idx<-which.min((lon1-samp[,1])^2+medlat+(lon2-samp[,1])^2+(lat2-samp[,2])^2)
  c(samp[idx,1],samp[idx,2])
}

