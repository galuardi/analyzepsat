makeaxes <-
function(xmin=-100,xmax=10,xtick=10,ymin=0,ymax=90,ytick=10,font=2,lwd=2,xlab=' W',ylab=' N',col=1,axis1=T,axis2=T){
 if(axis1==T){ axis(1,at=seq(xmin,xmax,by=xtick),labels=paste(seq(xmin,xmax,by=xtick)*-1,xlab,sep=""),font=font,lwd=lwd,col=col)
 }else{
	axis(1,at=seq(xmin,xmax,by=xtick),labels=NULL,xlab="",col.axis='white',font=font,lwd=lwd,col=col)
   }
 if(axis2==T) { axis(2,at=seq(ymin,ymax,by=ytick),labels=paste(seq(ymin,ymax,by=ytick),ylab,sep=""),font=font,lwd=lwd,col=col)
 } else{
	axis(2,at=seq(ymin,ymax,by=ytick),labels=NULL,ylab="",col.axis='white',font=font,lwd=lwd,col=col)
   }
}

