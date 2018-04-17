
#' Title
#'
#' @param allTZ 
#' @param tbrks 
#' @param xdiv 
#' @param xseq 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plotThist = function(allTZ, tbrks = seq(3,35,by=1), xdiv = 4, xseq = .25, ...){
###   TEMP
attach(allTZ)
tot = sum(!is.na(allTZ$Ext_T))
# tbrks = seq(3,31,by=1)
dhist=hist(allTZ[dn=='Day',3],breaks = tbrks,plot=F);
nhist=hist(allTZ[dn=='Night',3],breaks = tbrks,plot=F);
usr <- par("usr")
axidx=seq(1,length(dhist$mids),2)
len = sum(nhist$count)
barplot(dhist$counts,horiz=T,density=dhist$density,space=0,xlim=c(-1*len/xdiv,len/xdiv),axes=F)
barplot(nhist$counts*-1,horiz=T,col="grey80",space=0,xlim=c(-1*len/xdiv,len/xdiv),axes=F,add=T)

# ax1idx = c(seq(par()$xaxp[1],0, length = 5),seq(0, par()$xaxp[2], length = 5)[2:5])
# abline(v=ax1idx, col = 'grey90', lty=2)
# axis(1, labels = paste(c(round(ax1idx[1:4]/sum(nhist$count)*-100),round(ax1idx[5:9]/sum(dhist$count)*100)),"%", sep=""), at = ax1idx, las = 2)

ax1idx=c(rev(-sum(nhist$count)*seq(0,1,by=xseq)),sum(nhist$count)*seq(0,1,by=xseq))
abline(v=ax1idx, col = 'grey90', lty=2)
axis(1, at=ax1idx, labels=paste0(c(rev(seq(0,1,by=xseq)), seq(0,1,by=xseq))*100,'%'))

# axis(1,labels=paste(round(c(rev(pretty(1:60000)),0,pretty(1:60000))/tot,2)*100,'%',sep=""),at=c(rev(pretty(1:60000))*-1,0,pretty(1:60000)), las = 2)
# barplot(dhist$counts,horiz=T,density=dhist$density,space=0,add=T, axes=F, xlim=c(len*-1,len), col  = 'white')      # xlim=c(-10000,10000),
# barplot(nhist$counts*-1,horiz=T,col="grey80",space=0,axes=F,add=T, xlim=c(len*-1,len))   # xlim=c(-10000,10000),

barplot(dhist$counts,horiz=T,density=dhist$density,add=T,space=0,xlim=c(-1*len/xdiv,len/xdiv),axes=F)
barplot(nhist$counts*-1,horiz=T,col="grey80",space=0, xlim=c(-1*len/xdiv,len/xdiv),axes=F,add=T)


#axis(1,labels=paste(round(c(15000,10000,5000,0,5000,10000,15000)/tot,2)*100,'%',sep=""),at=c(-15000,-10000,-5000,0,5000,10000,15000), las = 2)
axis(2,labels=ceiling(dhist$mids[axidx]),at=axidx,las=2);
title(main = '',xlab="Frequency",ylab = expression(paste('Temperature (', C^o, ')', sep='')))
text(-1*len/19,4,'Night', cex = 1.5, font=2);text(len/19,4,'Day', cex = 1.5, font=2)
box()
detach(allTZ)
}
   
###    DEPTH

plotZhist = function(allTZ, interval = 10, plotmin = -200, xdiv=4,  xseq = .25, ...){
attach(allTZ)
tot = sum(!is.na(allTZ$depth))
minz = min(allTZ$depth,na.rm=T)
brks = c(seq(minz, plotmin, by = plotmin-(minz)),seq(plotmin+50, 1, by = interval))
dhist=hist(allTZ[dn=='Day',2],breaks = brks, plot=F,xlim=c(-200,0))
nhist=hist(allTZ[dn=='Day',2], breaks = brks,plot=F,xlim=c(-200,0))
udepth= rev(round(sort(unique(allTZ$depth))))
len = sum(nhist$count)
barplot(dhist$counts,horiz=T,density=dhist$density,space=0,axes=F, xlim=c(len/xdiv*-1,len/xdiv))      # xlim=c(-10000,10000),
barplot(nhist$counts*-1,horiz=T,col="grey80",space=0,axes=F,add=T, xlim=c(len/xdiv*-1,len/xdiv))   # xlim=c(-10000,10000),

#len = sum(nhist$count)
# ax1idx = c(seq(par()$xaxp[1],0, length = 5),seq(0, par()$xaxp[2], length = 5)[2:5])
ax1idx=c(rev(-sum(nhist$count)*seq(0,1,by=xseq)),sum(nhist$count)*seq(0,1,by=xseq))
abline(v=ax1idx, col = 'grey90', lty=2)
# axis(1, labels = paste(c(round(ax1idx[1:4]/sum(nhist$count)*-100),round(ax1idx[5:9]/sum(dhist$count)*100)),"%", sep=""), at = ax1idx, las = 2)
axis(1, at=ax1idx, labels=paste0(c(rev(seq(0,1,by=xseq)), seq(0,1,by=xseq))*100,'%'))
# axis(1,labels=paste(round(c(rev(pretty(1:60000)),0,pretty(1:60000))/tot,2)*100,'%',sep=""),at=c(rev(pretty(1:60000))*-1,0,pretty(1:60000)), las = 2)
barplot(dhist$counts,horiz=T,density=dhist$density,space=0,,add=T, axes=F, xlim=c(len/xdiv*-1,len/xdiv), col  = 'white')      # xlim=c(-10000,10000),
barplot(nhist$counts*-1,horiz=T,col="grey80",space=0,axes=F,add=T, xlim=c(len/xdiv*-1,len/xdiv))   # xlim=c(-10000,10000),
brklabs = as.character(brks)
brklabs[1]='> -200'
axis(2, at = 0:(length(brks)-1), label = brklabs, las=2)
title(main = '',xlab='Frequency',ylab='Depth (m)')
text(-1*(len/5),4,'Night', cex = 1.5, font=2);text((len/5),4,'Day', cex = 1.5, font=2)
box()
detach(allTZ)
}


# par(mfrow=c(1,2))
# plotThist(allTZ, xdiv=1)
# plotZhist(allTZ, interval=10, xdiv=1, xseq=.1)

# plotThist(allTZ, xdiv=2)
# plotZhist(allTZ, interval=10, xdiv=2)

# plotThist(allTZ, xdiv=4)
# plotZhist(allTZ, interval=10, xdiv=4)
