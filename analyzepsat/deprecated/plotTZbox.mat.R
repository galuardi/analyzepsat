plotTZbox.mat <-
function(tzdata,zlim=NULL,fcn=mean,font=1){
   jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
   # require(date)
   # require(matlab)
   dataZ=t(tzdata$Z)
   dataT=t(tzdata$T)

  if(!is.null(zlim)){
   dataT[dataT<(zlim)]=NaN
  }

 #layout(matrix(c(rep(1,10),2),1,11,byrow=T)) 
  #par(mar = c(7,5,2,1) + 0.1)
  boxplot.matrix(dataZ,col=jet.colors(33)[apply(dataT,2,fcn,na.rm=T)],varwidth=T,axes=F,font=font)
  xidx=(dim(dataT)[2])#dim(dataT)[1]*
  day0=tzdata$day0
  dayT=tzdata$dayT
   if(day0>700000){
    day0=day0-715876+1 # matlab date conversion
	dayT=dayT-715876+1
   }
   tzdates=seq(day0,dayT+1)
   xp <- par("xaxp")
   xcut <- round(seq(xp[1], xidx, length = 12))
   dcut=seq(day0, dayT, length = 12)
   xcut[1]=1
   axis(1,at=xcut,label=paste(as.date(dcut),font=font),las=2);axis(2,font=font)
   box()
  }

