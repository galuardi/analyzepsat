\name{CI2shp}
\alias{CI2shp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export confidence intervals of an estimated track to a shapefile. 
}
\description{
May be used on a dataframe containing multiple tracks.
}
\usage{
CI2shp(track, fname = "testshp", level = 0.95, npoints = 100, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
track in the form of fmat or btrack \code{\link{make.fmat}}, \code{\link{make.btrack}}
}
  \item{fname}{
Output filename omitting file extensions
}
  \item{level}{
Confidence interval level desired (default 95\%)
  }
  \item{npoints}{
Number of points for each confidence interval (default 100)
}
  \item{proj4string}{
Projection string (see \code{\link{maptools}})
}
}
\details{
}
\value{
 output is a shapefile
}
\references{

}
\author{
Benjamin Galuardi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{make.fmat}}, \code{\link{make.btrack}},\code{\link{maptools}}
}
\examples{


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
