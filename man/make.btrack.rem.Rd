\name{make.btrack.alt}
\Rdversion{1.1}
\alias{make.btrack.alt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bathymetric Correction where land points are removed
}
\description{
Uses bathymetry to correct a kftrack style estiamted PSAT track. This differs from \code{\link{make.btrack}}. When a point occurs far on land, and all sampled points (from within the confidence interval) fall on land, the previous version simply re-uses the previous days bathymetric sample. In this way, the points are always kept off land, but the result is often that the final estimated location is identical to the previous days. 

Using this alternative version avoids this by removing those days where bathymetry cannot be obtained from the estimated points confidence interval. This has only been tested using the Reynolds SST, which is very coarse in resolution.  In the future, this may be avoided by using a finer SST (on a realy fast computer...), which should have the desired efffect of estiamting the fish's location as in the water or close to shore.

The moral of the story is bathymtric correction works best when kalman filter estimated tracks are close to, or in the water. 
}
\usage{
make.btrack.rem(fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, npoints = 300, fulldist = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmat}{
data frame containing the following columns:
Day 	Month 	Year      V11         V12         V21     	V22   	Lon_E  Lat_N 	max_depth 	 maxTemp

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
http://www.ngdc.noaa.gov/mgg/global/etopo2.html
}
\author{
Benjamin Galuardi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{make.btrack}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}

