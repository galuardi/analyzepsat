minmax_extract <-
function(tagID,tag,plot=F){	
		xlsfile=paste(tagDir,'/',tagID,'/',tagID,'.xls',sep="");
		tagcon=odbcConnectExcel(xlsfile,readOnly=T)
		#------------------------------------------------------------------------------------------------------------------------------------#
		dayT=tag$dayT
		day0=tag$day0
		tdates=seq(day0,dayT)
		#------------------------------------------------------------------------------------------------------------------------------------#

		tres=sqlFetch(tagcon,'Temp Data (MinMax)') #'
		idx=!is.na(tres[,1])
		tmin=tres[idx,3]
		tmax=tres[idx,7]
		tres=tres[idx,]

		year=as.numeric(substr(tres[,1],1,4))
		month=as.numeric(substr(tres[,1],6,7))
		day=as.numeric(substr(tres[,1],9,10))

		mdate=mdy.date(month,day,year)
		midx=mdate>=day0&mdate<=dayT
		tmin=tmin[midx]
		tmax=tmax[midx]
		mdate=mdate[midx]
		didx=match(mdate,tdates)
		didx=didx[!is.na(didx)]
		# didx=which(mdate>=day0&mdate<=dayT)

		tminmax=as.data.frame(matrix(NA,length(tdates),3))
		tminmax[,1]=tdates
		tminmax[didx,2:3]=cbind(tmin,tmax)
		names(tminmax)=c('date','min','max')

		#------------------------------------------------------------------------------------------------------------------------------------#
		pres=sqlFetch(tagcon,'Press Data (MinMax)')  #'
		idx=!is.na(pres[,1])
		pmin=pres[idx,3]
		pmax=pres[idx,7]
		pres=pres[idx,]

		year=as.numeric(substr(pres[,1],1,4))
		month=as.numeric(substr(pres[,1],6,7))
		day=as.numeric(substr(pres[,1],9,10))

		mdate=mdy.date(month,day,year)
		midx=mdate>=day0&mdate<=dayT
		pmin=pmin[midx]
		pmax=pmax[midx]
		mdate=mdate[midx]
		didx=match(mdate,tdates)
		didx=didx[!is.na(didx)]
		# didx=which(mdate>=day0&mdate<=dayT)

		pminmax=as.data.frame(matrix(NA,length(tdates),3))
		pminmax[,1]=tdates
		pminmax[didx,2:3]=cbind(pmin,pmax)
		names(pminmax)=c('date','min','max')

		# tag$pminmax=pminmax
		# tag$tminmax=tminmax
		return(tminmax,pminmax)

		odbcCloseAll()
if(plot){
			par(mar=c(6,4,4,6))
			plot(as.ts(pminmax[,c(3)]),ylim=c(-1100,0),col=4,pch=19,typ='p',ylab='Depth (m)',xlab="",las=2,axes=F)
			points(as.ts(pminmax[,c(2)]),ylim=c(-1100,0),col='lightblue',pch=19)
			xidx=dim(pminmax)[1]
			segments(1:xidx,pminmax[,2],1:xidx,pminmax[,3],col='lightblue')
			tzdates=seq(day0,dayT+1)   
			xp <- par("xaxp")
			xcut <- round(seq(xp[1], xidx, length = 12))
			dcut=seq(day0, dayT, length = 12)
			xcut[1]=1  
			axis(1,at=xcut,label=paste(as.date(dcut)),las=2);axis(2)
			
			par(new=T)
			plot(tminmax[,c(1,3)],ylim=c(0,30),col=2,pch=19,axes=F,xlab="",ylab="")
			points(tminmax[,c(1,2)],col='pink',pch=19)
			segments(tminmax[,1],tminmax[,2],tminmax[,1],tminmax[,3],col='orange')
			axis(4)
			mtext('Temp C',side=4,line=2)
			title(paste(tagID,' Min and Max Depth/Temp'))
		}
}

