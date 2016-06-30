\name{track2shp}
\alias{track2shp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export an estimated track as a point shapefile
}
\description{

}
\usage{
track2shp(track, fname = "testshp", proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
track in the form of fmat or btrack \code{\link{make.fmat}}, \code{\link{make.btrack}}
}
  \item{fname}{
Output filename omitting file extensions
}
  \item{proj4string}{
Projection string (see \code{\link{maptools}})
}
}
\details{

}
\value{
output is a point shapefile
}
\references{
%% ~put references to the literature/web site here ~
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
