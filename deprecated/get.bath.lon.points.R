get.bath.lon.points <-
function(point,bathy,lat.tol=10,lonmult=2){
    X=bathy$lon
    Y=bathy$lat
	if(point[4]==0) point[4]=1e-6
    lonsd=lonmult*sqrt(point[4])	#2*
    lonidx=unlist(c(lonsd+point[8],point[8]+lonsd*-1))
    lonidx=X<=lonidx[1]&X>=lonidx[2]
    latidx=unlist(c(point[9]+lat.tol,point[9]+lat.tol*-2)) # sometime this needs to switch based on where the land is... north or south
    latidx=Y<=latidx[1]&Y>=latidx[2]
	maxz=point[10]
   	ztemp=as.vector(t(bathy$data)[lonidx,latidx])
	didx=which(ztemp<=maxz)
	temp=expand.grid(bathy$lon[lonidx],bathy$lat[latidx])
	if(sum(didx)>0) return(cbind(temp[didx,],ztemp[didx]))
	else(print("no matches! Where is your fish going???? "))	  
  }

