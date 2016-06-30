.add.trend <-
function(dataT,fcn=mean){
	d1=dim(dataT)[1]
    d2=dim(dataT)[2]
	lines(seq(1,d1*d2,length=d1),apply(dataT,1,fcn,na.rm=T),lwd=2)
}

