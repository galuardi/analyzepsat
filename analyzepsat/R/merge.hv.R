merge.hv <-
function(tracks, TZ){

# if(is.null(tracks$tagID)==T) {
 # for(i in 1:length(tracks)){
 # tracks$tagID = names(tracks)[i]
# }
# }

if(class(tracks)=='list'){
  ttracks = NULL
 for(i in 1:length(tracks)){
  ttracks = rbind(ttracks, tracks[[i]])
 }
 tracks = ttracks
 rm(ttracks)
 names(tracks)[12] = 'tagID'
}

if(max(tracks$Day)>35) names(tracks)[1:3] = c('Year','Month','Day')

if(length(unique(TZ$tagID))==length(unique(tracks$tagID))){


uidx = unique(tracks$tagID)
tdates = as.POSIXct(trunc(ISOdate(tracks$Year, tracks$Month, tracks$Day, tz='GMT'),'day'))
tzdates = as.POSIXct(trunc(TZ$tzdat.Date, 'day'))
allidx = numeric(nrow(TZ))

for(i in 1:length(uidx)){
print(paste("Now merging tag# ", uidx[i], sep=""))
subidx1 = TZ$tagID==uidx[[i]]
subidx2 = which(tracks$tagID==uidx[[i]])
dateidx = match(tzdates[subidx1], tdates[subidx2])
allidx[subidx1] = subidx2[dateidx]
rm(subidx1, subidx2, dateidx)
}

# combine all data frames!
allhv = cbind(TZ, tracks[allidx,])
allhv = allhv[!is.na(allhv$maxt),]
allhv = allhv[!is.na(allhv$Ext_T),]
allhv = allhv[!is.na(allhv$depth),]
}else{
print("STOP! Number of fits does not equal number of original tracks! Make sure you have the proper Tag IDs and labels")
}

}

# mhv = merge.hv(list(interp.fits[[1]]), mtz)


# mhv = merge.hv(interp2, mtz)
