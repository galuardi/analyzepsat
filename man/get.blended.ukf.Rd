\name{get.blended.ukf, get.blended.ukf.trackit, get.reynolds}
\Rdversion{2.1}
\alias{get.blended.ukf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download SST data and parse input files for ukfsst and trackit.
}
\usage{
get.blended.ukf(track, folder = tempdir(), daycomp = c(5,8), save = F) 
get.blended.ukf.trackit(track, lonlow, lonhigh, latlow, lathigh, folder = tempdir(), daycomp = c(5,8), save = F) 
get.reynolds(track, folder = tempdir(), removeland = TRUE, save = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
an input track for ukfsst or trackit
}
  \item{lonlow, lonhigh, latlow, lathigh}{
Longitude and latitude limits. Not needed in the ukf versions
}
  \item{folder}{
A place to save your data
}
  \item{daycomp}{
An 8 or 5 day composite. May also be able to use different lengths (i.e. 3 or 10 etc.)
}
}

\value{
files for use in ukfsst or trackit estimation
}

\author{
Benjamin Galuardi and Chi Hin Lam
}

\seealso{
\code{\link{get.SST}}
}
\examples{

# setwd("Your working directory or SST data directory")
#====================================================#
# Multiple day example using limits of a track
#====================================================#
fake.track = data.frame(day = c(1,14), month = c(8,8), year = c(2008, 2008), lon = c(-80,-50), lat = c(20,50), sst = c(10,10))
get.blended.ukf(fake.track, folder = getwd(), daycomp = 8, save = T) 

sst = make.sstmat('request.nc', 'Blended')
par(mfrow=c(2,1))
image.plot(sst$lon, sst$lat, sst$DATA[,,1])
image.plot(sst$lon, sst$lat, sst$DATA[,,2])

#====================================================#
# Single day example
#====================================================#
sstfile = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdBAssta5day.nc?sst[(2011-08-8T12:00:00Z)][(0.0)][(20.0):(50.0)][(260.0):(310.0)]&.draw=surface&.vars=longitude|latitude|sst&.colorBar=Rainbow|C|Linear|0|32|"

fname = "sst.nc"
download.file(sstfile, fname, mode="wb")

sst = make.sstmat(fname, 'Blended')
image.plot(sst$lon, sst$lat, sst$DATA)
}
\keyword{SST}

