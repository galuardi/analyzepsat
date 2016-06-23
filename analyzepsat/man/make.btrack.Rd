\name{make.btrack}
\Rdversion{1.1}
\alias{make.btrack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bathymetric Correction
}
\description{
Uses bathymetry to correct a kftrack style estimated PSAT track. 
}
\usage{
make.btrack(fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, npoints = 300, fulldist = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmat}{
data frame containing the following columns:
 Year	Month	Day      V11         V12         V21     	V22   	Lon_E  Lat_N 	max_depth 	 maxTemp

}
  \item{bathy}{
Bathymetry object. Here, this is a list containing 3 objects: Longitude vector, Latitude vector, matrix of depth values. The data provided in this package is the E-Topo 2 minute bathymetry for the North Atlantic Ocean. 
}
  \item{save.samp}{
This function samples bathymetry at each estimated point. If you want to save all these sampled points, set this to 'T'
}
  \item{mintype}{
Minimization type. Possible choices are, 2, 3, and 4. This function weights each sample with the weighted mean of the sample and the previous days final estimated point. By default, this is option 2. Option 3 weights with these and the next days estimated location as well. Option 4 weights based on the weighted mean of the sample and the next days final estimated location. Avoid using this under normal circumstances.
}
  \item{ci}{
Confidence level used for the bathymetric sampling.
}
  \item{npoints}{
number of points in the sample
}
  \item{fulldist}{
This function may be used in a manner where bathymetric correction is only performed when the maximum daily depth is shallower than the bathymetry at that point in the most probable track. In this case, set this parameter to 'T'. 
}
}
\details{
This function adjusts a kftrack fitted track based on bathymetric sampling within the estimated covariance (confidence ) intervals. 
}
\value{
Data frame of the same form as fthe fmat input. If save.samp = T, then a list is returned where the second element is all samples fro mthe estimation.
}
\references{

ETOPO2v2 Global Gridded 2-minute Database, National Geophysical Data Center, National Oceanic and Atmospheric Administration, U.S. Dept. of Commerce, http://www.ngdc.noaa.gov/mgg/global/etopo2.html

Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.

Becker, J. J., D. T. Sandwell, W. H. F. Smith, J. Braud, B. Binder, J. Depner, D. Fabre, J. Factor, S. Ingalls, S-H. Kim, R. Ladner, K. Marks, S. Nelson, A. Pharaoh, G. Sharman, R. Trimmer, J. vonRosenburg, G. Wallace, P. Weatherall., Global Bathymetry and Elevation Data at 30 Arc Seconds Resolution: SRTM30_PLUS, revised for Marine Geodesy, January 20, 2009

Galuardi, B.; Royer, F.; Golet, W.; Logan, J.; Nielson, J. & Lutcavage, M. Complex Migration Routes of Atlantic Bluefin Tuna Question Current Population Structure Paradigm, Canadian Journal of Fisheries and Aquatic Sciences, 2010, Vol. 67(1), 966-976

}
\author{
Benjamin Galuardi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# load libraries
library(analyzepsat)

# load data
data(big.241)

# get bathymetry
bath = get.bath.data(-180,-150,-10,50)

# add dummy columns
big.241$sst = 10
big.241$maxz=-10

# run the Kalman filter routine
fit1 = kftrack(big.241[,1:5])

# prepare for bathymetric correction
ftrack1 = prepb(fit1, big.241)

# make sure longitude is -180 to 180. We here use the summy column for maxz
ftrack1$Lon_E = ftrack1$Lon_E-360

# Do the bathymetric correction
btrack1 = make.btrack(ftrack1, bath)

# plot it (not run)
#data(gmt3, package='trackit')
#data(myramps)
#plot(gmt3, xlim = c(-160, -140)+360, ylim = c(10,25), typ='l', axes=F)
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue")
#plot.btrack(btrack1, map2, offset=0, add=T, ci=T)
#polygon(gmt3, col = 'black')
#degAxis(1)
#degAxis(2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}

