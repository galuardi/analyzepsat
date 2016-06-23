plot.by.month <-
function(ttrack, saveplot=F, filename=NULL, cex=2, pch = 21){
    attach(ttrack)

month.colors=cbind(c(8:12,1:7),
  c(rgb(115/255,0,76/255),
    rgb(0,76/255,115/255),
    rgb(0,92/255,230/255),
    rgb(115/255,223/255,1),
    rgb(190/255,232/255,1),
    rgb(1,1,190/255),
    rgb(230/255,230/255,0),
    rgb(1,170/255,0),
    rgb(1,120/255,0),
    rgb(1,0,197/255),
    rgb(1,0,0),
    rgb(168/255,0,0)
  )
)

    mons=unique(Month)
    for(i in 1:length(mons)){
      lines(ttrack[Month==mons[i],8:9],typ='o',cex=cex,pch=pch,bg=month.colors[month.colors[,1]==mons[i],2])
      
     if(saveplot){
        savePlot(filename,type='eps')
        savePlot(filename,type='png',res=100)
        }
    }
   detach(ttrack)
  }

