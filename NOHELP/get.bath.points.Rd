\name{get.bath.points}
\Rdversion{1.1}
\alias{get.bath.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.bath.points(x, BATH, npoints = 100, level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{BATH}{
%%     ~~Describe \code{BATH} here~~
}
  \item{npoints}{
%%     ~~Describe \code{npoints} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
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
function(x,BATH,npoints=100,level=.95){
    maxz=x[7]
    t.quan <- sqrt(qchisq(level, 2))
	    centre <- x[5:6]

	    xm <- matrix(x[1:4], 2, 2)
	    r <- xm[1, 2]
	    scale <- sqrt(diag(xm))
	      if (scale[1] > 0) {
		r <- r/scale[1]
	    }
	    if (scale[2] > 0) {
		r <- r/scale[2]
	    }
	    r <- min(max(r, -1), 1)
	    d <- acos(r)
	    a <- seq(0, 2 * pi, len = npoints)
      polymat=(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
                t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,2))
      SR=Polygon(rbind(polymat,polymat[1,]))
      SR=Polygons(list(SR),"p1")
      SR=SpatialPolygons(list(SR),1)
      samp=dotsInPolys(SR, npoints, f = "random")
      samp=coordinates(samp)
      y1=samp[,1];y2=samp[,2]
      bath.v<-sapply(1:length(y1), function(i)get.bath(y1[i],y2[i], BATH))
      if(maxz<0){
      samp[bath.v<-10&bath.v<=maxz,]
      }else{
        samp[bath.v<(-10),]
        }
      #as.vector(c(samp2[idx,1],samp2[idx,2]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
