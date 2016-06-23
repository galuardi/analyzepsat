get.bath.points <-
function(x,BATH,npoints=100,level=.95){
    maxz=x[7]
    t.quan <- sqrt(qchisq(level, 2))
	    centre <- x[5:6]

	    xm <- matrix(x[1:4], 2, 2)
	    r <- xm[1, 2]
	    scale <- sqrt(diag(xm))
	      if (scale[1] > 0) {
		r <- r/scale[1]
	    }
	    if (scale[2] > 0) {
		r <- r/scale[2]
	    }
	    r <- min(max(r, -1), 1)
	    d <- acos(r)
	    a <- seq(0, 2 * pi, len = npoints)
      polymat=(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
                t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,2))
      SR=Polygon(rbind(polymat,polymat[1,]))
      SR=Polygons(list(SR),"p1")
      SR=SpatialPolygons(list(SR),1)
      samp=dotsInPolys(SR, npoints, f = "random")
      samp=coordinates(samp)
      y1=samp[,1];y2=samp[,2]
      bath.v<-sapply(1:length(y1), function(i)get.bath(y1[i],y2[i], BATH))
      if(maxz<0){
      samp[bath.v<-10&bath.v<=maxz,]
      }else{
        samp[bath.v<(-10),]
        }
      #as.vector(c(samp2[idx,1],samp2[idx,2]))
  }

