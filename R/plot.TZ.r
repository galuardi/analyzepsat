#' plot.TZ
#'
#' @param tzdata 
#' @param Z 
#' @param dcol 
#' @param axes 
#' @param trend 
#'
#' @return
#' @export
#'
#' @examples
plot.TZ <-
function(tzdata,Z=1,dcol=2,axes=T,trend = F){   
   # require(date)
  if(Z==1){
   dataT=t(tzdata$Z)
   # dcol=4
   ylabel='Depth (m)'
  }else{
   dataT=t(tzdata$T)
   # dcol=2
   ylabel='Temperature (C)'
  }
  day0=tzdata$day0
  dayT=tzdata$dayT
 # par(mar = c(6,4,2,1) + 0.1)
   plot(ts(as.vector(dataT)),axes=F,type='l',cex=.2,col=dcol,xlab="",ylab=ylabel)
   points(ts(as.vector(dataT)),col=dcol,cex=.2,pch=19)   
   if(trend) .add.trend(t(dataT))
   d1=dim(dataT)[1]
   d2=dim(dataT)[2]
   xidx=(d1*d2)
   tzdates=seq(day0,dayT+1)   
   
   xp <- par("xaxp")
   xcut <- round(seq(xp[1], xidx, length = 12))
   dcut=seq(day0, dayT, length = 12)
   xcut[1]=1  
   if(axes) axis(1,at=xcut,label=paste(as.date(dcut)),las=2);
   axis(1,at=xcut,labels=F,las=2);
   axis(2)  
   box() 
   }
   
   .add.trend <- function(dataT,fcn=mean){
	d1=dim(dataT)[1]
    d2=dim(dataT)[2]
	lines(seq(1,d1*d2,length=d1),apply(dataT,1,fcn,na.rm=T),lwd=2)
}



