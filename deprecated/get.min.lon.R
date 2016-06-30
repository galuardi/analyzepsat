get.min.lon <-
function(lon1,samp){
  medlat<-median(samp[,2])
  idx<-which.min((lon1-samp[,1])^2+medlat)
  c(samp[idx,1],samp[idx,2])
}

