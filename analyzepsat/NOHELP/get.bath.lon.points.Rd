\name{get.bath.lon.points}
\Rdversion{1.1}
\alias{get.bath.lon.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.bath.lon.points(point, bathy, lat.tol = 10, lonmult = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{point}{
%%     ~~Describe \code{point} here~~
}
  \item{bathy}{
%%     ~~Describe \code{bathy} here~~
}
  \item{lat.tol}{
%%     ~~Describe \code{lat.tol} here~~
}
  \item{lonmult}{
%%     ~~Describe \code{lonmult} here~~
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
function(point,bathy,lat.tol=10,lonmult=2){
    X=bathy$lon
    Y=bathy$lat
	if(point[4]==0) point[4]=1e-6
    lonsd=lonmult*sqrt(point[4])	#2*
    lonidx=unlist(c(lonsd=point[8],point[8]+lonsd*-1))
    lonidx=X<=lonidx[1]&X>=lonidx[2]
    latidx=unlist(c(point[9]+lat.tol*2,point[9]+lat.tol*-1)) # sometime this needs to switch based on where the land is... north or south
    latidx=Y<=latidx[1]&Y>=latidx[2]
	maxz=point[10]
   	ztemp=as.vector(t(bathy$data)[lonidx,latidx])
	didx=which(ztemp<=maxz)
	temp=expand.grid(bathy$lon[lonidx],bathy$lat[latidx])
	if(sum(didx)>0) return(cbind(temp[didx,],ztemp[didx]))
	else(print("no matches! Where is your fish going???? "))	  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
