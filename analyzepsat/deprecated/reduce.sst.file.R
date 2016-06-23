reduce.sst.file <-
function(sstfile, xtrack, outfile, buffer = 2){
	print(paste('Reducing file ',sstfile,sep = ""))
	 tfile = read.table(sstfile)
	 lonr = range(xtrack[,4], na.rm = T)
	 if(any(lonr < 0)) lonr = lonr + 360
	 latr = range(xtrack[,5], na.rm = T)
	 lonidx = tfile[,2]>=(lonr[1]-buffer)&tfile[,2]<=(lonr[2]+buffer)
	 latidx = tfile[,1]>=(latr[1]-buffer)&tfile[,1]<=(latr[2]+buffer)
	 allidx = lonidx==T&latidx==T
	 
	 tfile = tfile[allidx,]
	 write.table(tfile, file = outfile, row.names = F, col.names = F)
}

