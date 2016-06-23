getSRSSdates <-
function(res,MWTdates,day0,dayT,day0b,dayTb){
if(class(res[1,1])=='Date'){ 
	SRSSdates=sort(as.date(levels(res[,1])))   # get the dates from the data         
	SRSSdates= SRSSdates[SRSSdates<=dayT&SRSSdates>=day0]   # get the dates of fish at liberty
	}else{
		SRSSdates=sort(res[,1]) 
		SRSSdates= SRSSdates[SRSSdates<=dayTb&SRSSdates>=day0b]   # get the dates of fish at liberty
	}

didx=(1:length(SRSSdates))*0
for(i in 1:length(SRSSdates)){
  didx[i]=which.min(as.numeric(MWTdates-SRSSdates[i])^2)
}
didx=unique(didx)
didx
}

