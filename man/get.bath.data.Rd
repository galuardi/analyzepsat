\name{get.bath.data}
\Rdversion{2.0}
\alias{get.bath.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download bathymetry
}
\description{
Downloads bathymetry at two resolutions: 1 minute (ETOPO1) and 30 second (SRTM). 
}
\usage{
get.bath.data(lonlow, lonhigh, latlow, lathigh, folder = tempdir(), seaonly = T, res = c(.5,1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lonlow}{
Longitude minimum
}
  \item{lonhigh}{
Longitude maximum
}
  \item{latlow}{
Latitude minimum
}
  \item{lathigh}{
Latitude maximum
}
  \item{folder}{
storage location is desired
}
  \item{res}{
Resolution, in minutes
}

}
\value{
Bathymetry object. Here, this is a list containing 3 objects: Longitude vector, Latitude vector, matrix of depth values. 
}

\references{

http://www.ngdc.noaa.gov/mgg/global/global.html

http://coastwatch.pfeg.noaa.gov/erddap/info/usgsCeSrtm30v6/index.html

Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.

Becker, J. J., D. T. Sandwell, W. H. F. Smith, J. Braud, B. Binder, J. Depner, D. Fabre, J. Factor, S. Ingalls, S-H. Kim, R. Ladner, K. Marks, S. Nelson, A. Pharaoh, G. Sharman, R. Trimmer, J. vonRosenburg, G. Wallace, P. Weatherall., Global Bathymetry and Elevation Data at 30 Arc Seconds Resolution: SRTM30_PLUS, revised for Marine Geodesy, January 20, 2009

}
\author{
Benjamin Galuardi
}

\examples{

# nice blue cascade for bathymetry
bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

#***OR****#
# bath.colors <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))
library(fields)
bath1 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 1)
bath.5 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = .5)

# plot 
par(mfrow=c(1,2))
par(mar=c(8,4,4,4))
image(bath1[[1]],(bath1[[2]]),t(bath1[[3]]),col=bath.colors(100), zlim=c(-10000,0), xlab = '', ylab='')
image(bath.5[[1]],(bath.5[[2]]),t(bath.5[[3]]),col=bath.colors(100), zlim=c(-10000,0), xlab = '', ylab='')

# add a scale bar
ticks <- c(-10000,-8000,-6000,-4000,-2000,-1000,-500,0)
par(cex=0.9)
image.plot(matrix(1), zlim=c(-10000,0), horizontal=T, col = bath.colors(100),
	 axis.args=list(at=ticks,labels=ticks*-1), legend.shrink = 0.8, 
	 legend.args=list(text="  Bottom \n depth (m)", cex=0.8, side=4, line=1, las=1),
	 legend.only=T)
par(cex=1)
 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}
