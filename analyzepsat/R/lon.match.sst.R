lon.match.sst <-
function(fmat,sstmat,sst.tol=.5,lat.tol=5,lon.mult=1){
   sst.tol.o=sst.tol;lat.tol.o=lat.tol;lon.mult.o=lon.mult;
   len=length(fmat[,1])
   vec=1:len
   fidx=fmat[,11]<5
   fmat[fidx,11]=NA
   if(any(fidx==T)){
    ltmp=loess(fmat[,11]~vec,span=.25)
    fmat[fidx,11]=predict(ltmp,newdata=vec[fidx])
   }
   datediff=c(0,diff(as.numeric(mdy.date(fmat[,2],fmat[,1],fmat[,3]))))
   ntrack=as.data.frame(matrix(0,len,6))
   ntrack[1,]=c(0,0,0,0,fmat[1,8:9])
   ntrack[len,]=c(0,0,0,0,fmat[len,8:9])
   sptmp=list()  
   psave=NULL   

for(i in 3:((len)-1)){
        print(paste('SST point ',i,sep=""))
        point=as.numeric(fmat[i,])
        maxt=point[11]
		maxz=point[10]
		if(maxt<20)lat.tol=lat.tol*2 else lat.tol=lat.tol;
		#sst.tol=.5;lonmult=1.5
		samp.sst.pts = .get.sst.lon.points(point,sstmat,sst.tol=sst.tol,lat.tol=lat.tol,lon.mult=lon.mult,npoints=1000)
		ii=1		
		while(!is.data.frame(samp.sst.pts)&ii<25){
		  print(ii)
		  sst.tol=sst.tol+ii/4;lat.tol=lat.tol+ii;lon.mult=lon.mult+ii/10;
		  samp.sst.pts= .get.sst.lon.points(point,sstmat,sst.tol,lat.tol,lon.mult,npoints=1000) #+ii/4
		  print(paste('iteration ',ii,':  sst.tol=',1+ii/10,': lat.tol=',1+ii,': lon.mult=',1+ii/10) ) #'
		  ii=ii+1			  	
		}
		psave=rbind(psave,c(sst.tol,lat.tol,lon.mult))		
		sst.tol=sst.tol.o;lat.tol=lat.tol.o;lon.mult=lon.mult.o;

		if(!is.data.frame(samp.sst.pts)){
		 samp.sst.pts=sptmp[[i-1]]
		}
		psave=rbind(psave,c(sst.tol,lat.tol,lon.mult))
		if(dim(samp.sst.pts)[1]<2){
			while(dim(samp.sst.pts)[1]<2&ii<50){
			 print(ii)
			  samp.sst.pts = .get.sst.lon.points(point,sstmat,sst.tol=1+ii/4,lat.tol=5+ii,lon.mult=1+ii/10,npoints=1000)
			  print(paste('iteration ',ii,':  sst.tol=',1+ii/4,': lat.tol=',1+ii,': lon.mult=',1+ii/10) ) #'
			  ii=ii+1
			}
		}
		ntrack[i,5:6]=.get.min2(ntrack[i-1,5],ntrack[i-1,6],.denselect(samp.sst.pts)[1],.denselect(samp.sst.pts)[2],samp.sst.pts)			
		tcov=sqrt(cov(samp.sst.pts[,1:2]))	
		ntrack[i,1:4]=as.vector(tcov)  
		sptmp[[i]]=samp.sst.pts
	    print('sst good!')		
	}	
   ntrack[2,]=fmat[2,4:9] 
   fmat2=fmat
   fmat2[,4:9]=ntrack
   fmat2[is.nan(fmat2[,5]),5]=0
   fmat2[is.nan(fmat2[,6]),6]=0
   fmat2
}

.get.sst.lon.points <-
function(point,sstmat,sst.tol=2,lat.tol=10,lon.mult=2,npoints=1000){
    X=sstmat$lon
    Y=sstmat$lat
    lonsd=lon.mult*sqrt(point[4])	#2*
    lonidx=point[8]+c(lonsd,lonsd*-1)
    lonidx=X<=lonidx[1]&X>=lonidx[2]
    
    latidx=point[9]+c(lat.tol*2,lat.tol*-1) # sometime this needs to switch based on where the land is... north or south
    latidx=Y<=latidx[1]&Y>=latidx[2]
	maxt=point[11]
    # ddate=mdy.date(point[2],point[1],point[3])
	ddate=ISOdate(point[1],point[2],point[3],0,0,0)
	zidx=which.min((as.numeric(ddate)-as.numeric(sstmat$sstdates))^2)
	ztemp=as.vector(sstmat$DATA[lonidx,latidx,zidx])
	didx=which(abs(maxt-ztemp)<=sst.tol)
	temp=expand.grid(sstmat$lon[lonidx],sstmat$lat[latidx])
	if(sum(didx)>0) return(cbind(temp[didx,],ztemp[didx]))
	else(print("no matches! try a wider sst tolerance (or get a life)"))	  
  }


