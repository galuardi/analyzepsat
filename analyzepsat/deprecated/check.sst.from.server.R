check.sst.from.server <-
function (sstfile){
	tfile=read.table(sstfile)
	nidx = !is.nan(tfile[,3])
	tfile = tfile[nidx,]
	nidx = !is.na(tfile[,3])
	tfile = tfile[nidx,]
		if(any(tfile[,2]<0)){
			nidx = tfile[,2]<0
			tfile[nidx,2] = tfile[nidx,2]+360
		}
	write.table(tfile,file=sstfile,row.names=F,col.names=F)
}

