\name{get.SST}
\Rdversion{1.1}
\alias{get.SST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample Sea Surface Temperature values
}
\description{
Draws SST from a 3D matrix of SST. 
}
\usage{
get.SST(lon, lat, SST, date)
make.sstmat(ncsst, type="Blended")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
Longitude
}
  \item{lat}{
Latitude
}
  \item{SST}{
List containing 1) Vector of Longitude 2) Vector of Latitude 3) the Matrix of SST in the form: [lon,lat,time] and 4) Dates of class 'date' Vector of dates corresponding to dim[3] of SST
}
  \item{date}{
Date the location was recorded
}
}
\details{
SST is a named list having names: lon, lat, DATA and sstdates. This must be the case for thefunction to work properly.
}
\value{
An SST value
}

\author{
Benjamin Galuardi
}

\seealso{
\code{\link{get.blended.ukf}}
}

\examples{
fake = data.frame(day = c(1,30), month = c(1,1), year = c(2007,2007), lon = c(280,290), lat=c(40,50))
sstfile = get.blended.ukf(fake, folder = getwd(), daycomp = 8, save =T)
sst2007 = 'request.nc'
sstmat = make.sstmat(sst2007, type="Blended")
get.SST(213, lat = 36, sstmat, date = ISOdate(2007, 1, 15, tz = "GMT"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SST}

