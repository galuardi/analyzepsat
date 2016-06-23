\name{get.sst.lon.points}
\Rdversion{1.1}
\alias{get.sst.lon.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.sst.lon.points(point, sstmat, sst.tol = 2, lat.tol = 10, lon.mult = 2, npoints = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{point}{
%%     ~~Describe \code{point} here~~
}
  \item{sstmat}{
%%     ~~Describe \code{sstmat} here~~
}
  \item{sst.tol}{
%%     ~~Describe \code{sst.tol} here~~
}
  \item{lat.tol}{
%%     ~~Describe \code{lat.tol} here~~
}
  \item{lon.mult}{
%%     ~~Describe \code{lon.mult} here~~
}
  \item{npoints}{
%%     ~~Describe \code{npoints} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(point,sstmat,sst.tol=2,lat.tol=10,lon.mult=2,npoints=1000){
    X=sstmat$lon
    Y=sstmat$lat
    lonsd=lon.mult*sqrt(point[4])	#2*
    lonidx=point[8]+c(lonsd,lonsd*-1)
    lonidx=X<=lonidx[1]&X>=lonidx[2]
    
    latidx=point[9]+c(lat.tol*2,lat.tol*-1) # sometime this needs to switch based on where the land is... north or south
    latidx=Y<=latidx[1]&Y>=latidx[2]
	maxt=point[11]
    ddate=mdy.date(point[2],point[1],point[3])
	zidx=which.min((ddate-sstmat$sstdates)^2)
	ztemp=as.vector(sstmat$DATA[lonidx,latidx,zidx])
	didx=which(abs(maxt-ztemp)<=sst.tol)
	temp=expand.grid(sstmat$lon[lonidx],sstmat$lat[latidx])
	if(sum(didx)>0) return(cbind(temp[didx,],ztemp[didx]))
	else(print("no matches! try a wider sst tolerance (or get a life)"))	  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
