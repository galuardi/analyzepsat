\name{get.bath}
\Rdversion{1.1}
\alias{get.bath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample bathymetry
}
\description{
SAmple bathymetry at a particular location from a bathymetric list.
}
\usage{
get.bath(lon, lat, BATH)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
Longitude 
}
  \item{lat}{
Latitude
}
  \item{BATH}{
Bathymetry object. Here, this is a list containing 3 objects: Longitude vector, Latitude vector, matrix of depth values. The data provided in this package is the E-Topo 2 minute bathymetry for the North Atlantic Ocean.  Here, this list has names lon lat and data. This function requires these names to work properly.
}
}

\value{
A bathymetric value.
}
\references{
http://www.ngdc.noaa.gov/mgg/global/etopo2.html
Frnacois Royer originally wrote this as a matlab function. 
}
\author{
Benjamin Galuardi
}

\seealso{
\code{\link{get.samp}}
}
\examples{
bathy <- data(etopo2_R,package = 'analyzepsat')
point=c(-70,45)

get.bath(point,bathy)

# Vectorize

lon=seq(-70,-60,length=20)
lat=seq(35,45,length=20)
xy=cbind(lon,lat)

sapply(1:20,function(i) get.bath(xy[i,],bathy))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}
\keyword{sampling}% __ONLY ONE__ keyword per line
