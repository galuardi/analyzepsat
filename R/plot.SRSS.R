#' plot.SRSS
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
plot.SRSS <-
function(dat){
 # require(date)
 #win.graph(height=6,width=17)
 SRSS=dat$SRSS
 SRSS[which(SRSS[,3]<500),3]=SRSS[which(SRSS[,3]<500),3]+1400
 SRSS[which(SRSS[,2]>1400),3]=SRSS[which(SRSS[,2]>1400),2]-1400
 dat$SRSS=SRSS 

par(mar = c(6,4,2,1) + 0.1)
   plot(SRSS[,2],ylim=c(0,1500),axes=F,pch=19,col=2,xlab="",ylab="Minutes after midnight")
   points(SRSS[,3],pch=19,col=4)
   date1=SRSS[1,1]
   xdate=date.mdy(SRSS[,1])$month
   xp <- par("xaxp")
   xcut <- round(seq(xp[1], length(SRSS[,1]), length = 12))
   xcut[1]=1  
   axis(1,at=xcut,label=paste((SRSS[xcut,1])),las=2);axis(2)
   box()
   #title(names(dat)[i])
   }

