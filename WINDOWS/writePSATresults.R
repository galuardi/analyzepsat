writePSATresults <-
function(xtracks,file='PSAT_output.xls',Temp=F,Depth=F,psat=NULL){
# library(RODBC)
plen=length(xtracks)
fnames=names(xtracks)
#outfile='Geolocation_output_NS2007.xls'
psatout=odbcConnectExcel(file,readOnly=F)

for(i in 1:length(xtracks)){
	print(paste('Saving Final track ',i,sep=""))
 if(!is.null(xtracks[[i]]))	{
   sqlSave(psatout,data.frame(ID=1:length(xtracks[[i]][,1]),xtracks[[i]]),tablename=fnames[i],rownames=F)
 }

 if(Temp){
	tname=paste(fnames[i],'T',sep="")
	len=dim(psat[[i]]$T)[1]
	datout=rbind(seq(0,23.75,by=.25),psat[[i]]$T)
	#colnames(datout)=seq(0,23.75,by=.25)
	try(sqlSave(psatout,as.data.frame(datout),tablename=tname,rownames=F,colnames=F))#cbind(psat[[i]]$fulldates,
  }
   if(Depth){
	tname=paste(fnames[i],'Z',sep="")
	len=dim(psat[[i]]$Z)[1]
	datout2=datout=rbind(seq(0,23.75,by=.25),psat[[i]]$Z)
	#colnames(datout2)=seq(0,23.75,by=.25)
	try(sqlSave(psatout,as.data.frame(datout2),tablename=tname,rownames=F,colnames=F))#cbind(psat[[i]]$fulldates,
  }
 }
odbcClose(psatout)
}

