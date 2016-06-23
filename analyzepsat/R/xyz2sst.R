#-----------------------------------------------------------------------------#
# Function to output a matrix from an xyz style file
#
# Author: Ben Galaurdi
# Date: 4-29-2009
#
# Contact: galuardi@eco.umass.edu
# 
# Usage:
# Z=xyz2sst('C:/sstfile.dat')
#-----------------------------------------------------------------------------#


xyz2sst=function(tfile,...){
require(adehabitat)
g1=read.table(tfile)
if(g1[1,2]<0){
 g1[,2]=g1[,2]+360
}
names(g1)=c('y','x','z')
attach(g1)
Y=sort(unique(g1[,1]))
X=sort(unique(g1[,2]))

Z=array(NaN,c(length(X),length(Y)))

for(i in 1:length(Y)){
     yidx=g1[Y[i]==y,]
     xidx=yidx[,2]
  for(j in 1:length(yidx[,2])){
     xidx[j]=find(X==yidx[j,2])
      }
    Z[xidx,i]=yidx[,3]
}
detach(g1)  
#Z=list(X,Y,Z)
#names(Z)=c('X','Y','Z')
as.asc(Z, xll = min(X), yll=min(Y), cellsize = diff(X)[1],...)
}