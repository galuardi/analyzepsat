\name{lon.match.sst}
\Rdversion{1.1}
\alias{lon.match.sst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Longitude Sea Surface Temperature matching
}
\description{
Performs longitudinal SST matching using kalman filter estimates of position and SST
}
\usage{
lon.match.sst(fmat, sstmat, sst.tol = 0.5, lat.tol = 5, lon.mult = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmat}{
data frame containing the following columns:
Day 	Month 	Year      V11         V12         V21     	V22   	Lon_E  Lat_N 	max_depth 	 maxTemp

}
  \item{sstmat}{
List containing 1) Vector of Longitude 2) Vector of Latitude 3) the Matrix of SST in the form: [lon,lat,time] and 4) Dates of class 'date' Vector of dates corresponding to dim[3] of SST
}
  \item{sst.tol}{
The tolerance for the SST matching. This is used as +/- so choosing 1 for example will create a 2 degree window of +/- 1 degree from the ambient temperature recorded
}
  \item{lat.tol}{
Latitude tolerance for SST search. This may be small or large depending on several factors including the confidence in the latitudes from the first pass of kftrack or kfsst. 
}
  \item{lon.mult}{
Multiplier for the longitudinal tolerance. This is described as standard deviation using the estimated covariance. For example, a value of 1 = 1 standard deviation from the estiamted longitude.
}
}
\details{
This function attempts to follow the methodology layed out in Teo et al. 2004 and Domeier 2005. Longitude matching for light based geolocation was first proposed by Smith and Goodman (1986) and has been used in a multitude of publications dealign with marine fish tracking. This function has a built in loess smoothing algorithm for estimating maximum daily temperature when none was recorded for that day. Sometimes this does not so well on end points of a track and must be determined manually (use get.SST on the end points for a rough idea). If SST is missing on the endpoints this will produce errors. Likewise, if there exists an SST value which is far outside of the range, this will cause errors. An example is if we have a week where average SST is 20C and in the middle there is an 8C reading for SST. This is most likely caused by partial measurements for that day. In these cases SST may be estimated using get.SST at the original point, or loess smoothing. 

}
\value{
Data frame of the same form as fthe fmat input. 
}
\references{
Teo, S.; Boustany, A.; Blackwell, S.; Walli, A.; Weng, K. & Block, B. Validation of geolocation estimates based on light level and sea surface temperature from electronic tags Marine Ecology Progress Series, 2004, 283, 81-98

Domeier, M. L.; Kiefer, D.; Nasby-Lucas, N.; Wagschal, A. & O'Brien, F. Tracking Pacific bluefin tuna (Thunnus thynnus orientalis) in the northeastern Pacific with an automated algorithm that estimates latitude by matching sea-surface-temperature data from satellites with temperature data from tags on fish Fish. Bull., 2005, 103, 292

Smith, P. & Goodman, D. Determining fish movements from an "archival" tag: precision of geographical positions made from a time series of swimming temperature and depth NOAA Tech. Memo. NMFS SWFC, 1986, 60, 1-1


}
\author{
Benjamin Galuardi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.SST}}
}
\examples{
data(blue.shark)
blue.shark$year = 2007 # data is from 2001, before this blended sst is available
sst = get.blended.ukf(blue.shark, folder = getwd(), daycomp = 8, save = T) 
bsst = make.sstmat('request.nc', 'Blended')

# or 
# sst = get.sst.from.server(blue.shark, folder = getwd()) 
# bsst = make.sstmat('oisst.nc', 'Reynolds')

blue.shark$maxz = 0
fit = kftrack(blue.shark[,1:5])
fmat = prepb(fit, blue.shark)

sdata = lon.match.sst(fmat,bsst,sst.tol=1,lat.tol=5,lon.mult=2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SST}
\keyword{longitudinal}
\keyword{Sea surface temperature}% __ONLY ONE__ keyword per line
