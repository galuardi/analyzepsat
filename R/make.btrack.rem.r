make.btrack.rem <-
function(fmat,bathy,save.samp=F,mintype=2,ci=.95,npoints=300,fulldist=T){	
   len=length(fmat[,1])   
   ntrack=as.data.frame(matrix(0,len,6))
   ntrack[1,]=c(0,0,0,0,fmat[1,8:9])
   ntrack[len,]=c(0,0,0,0,fmat[len,8:9])
   sptmp=NULL
   nidx = numeric(length(ntrack[,1]))
   nidx[1] = nidx[len] =1
      for(i in 2:(length(fmat[,1])-1)){		
		print(paste('Bathymetric point ',i,sep=""))    #"'		
		point=fmat[i,]
		samp=.get.samp(point[4:9],npoints,ci=ci)  # uses the rmnrom function!
		samp.bath=sapply(1:length(samp[,1]),function(j) .get.bath(samp[j,1],samp[j,2],bathy))
		sidx=samp.bath<=as.numeric(point[10])
		samp=samp[sidx,]
        if(length(samp[sidx])<3){
          #samp=sptmp[[i-1]]
		  ntrack[i,] = point[4:9]
		  #ntrack[i,5:6] = 
          }else{
		  nidx[i] = 1
         # ntrack[i,5:6]=get.min2(ntrack[i-1,5],ntrack[i-1,6],mean(samp[,1],na.rm=T),mean(samp[,2],na.rm=T),samp)
		if(mintype==2) ntrack[i,5:6] = .get.min2(ntrack[i-1,5],ntrack[i-1,6],.denselect(samp)[1],.denselect(samp)[2],samp)  
		if(mintype==3)ntrack[i,5:6] = .get.min3(ntrack[i+1,5],ntrack[i+1,6],ntrack[i-1,5],ntrack[i-1,6],denselect(samp)[1],denselect(samp)[2],samp)
		if(mintype==4)ntrack[i,5:6] = .get.min3(ntrack[i+1,5],ntrack[i+1,6],.denselect(samp)[1],.denselect(samp)[2],samp)		
		 
        sptmp[[i]]=samp
		
		b.init=.get.bath(as.numeric(point[8]),as.numeric(point[9]),bathy)
		print(c(b.init-as.numeric(point[10])))
	      if(b.init<=as.numeric(point[10]) & fulldist==F){
			ntrack[i,]=fmat[i,4:9]
			}else{
        tcov=sqrt(cov(samp))
        tcov[is.nan(tcov)]=0
        ntrack[i,1:4]=as.vector(tcov)   
					}
				}
			}
	ntrack[nidx==0,6] = NA
	btrack=cbind(fmat[,1:3],ntrack,fmat[,10:11])
    names(btrack)=c('Year','Month','Day','V11','V12','V21','V22','Lon_E','Lat_N','maxz','maxt')
	
	if(save.samp){	
       return(btrack,sptmp)
      }else{
	  return(btrack)
	  }	  
 }

