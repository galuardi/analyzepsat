\name{analyzepsat-package}
\Rdversion{3.1}
\alias{analyzepsat-package}
\alias{analyzepsat}
\docType{package}
\title{Functions for Microwave Telemtry PSAT analysis}
\description{
Functions for reading Microwave Telemetry PSAT data into R,  analyzing it via state space methods,  and producing useful output. Emphasis is placed on data management and structures used in state space analysis and plotting. 

Currently, the package supports MTI fish reports for regular and archival information from recovered tags. An alternate workflow allows use of Tagbase (\code{\link{http://code.google.com/p/tagbase/}}) for creating the psat structure for MTI tags. The new functionality also allows bathmetric correction of Wildlife Computer tag data. For the new workflow, import your data into Tagbase and use either the \code{\link{tb2psat}} function or, fit your WC generated track, and use the \code{\link{prepb.wc}} function.

As long as the your data conforms to the btrack class, you may use the bathymetric correction functionality of this package. Most plotting and other features are currently supported only for MTI tag data. 
}
\details{
\tabular{ll}{
Package: \tab analyzepsat\cr
Type: \tab Package\cr
Version: \tab 3.0\cr
Date: \tab 2012-10-11\cr
License: \tab GPL version 2 \cr
LazyLoad: \tab yes\cr
}
Package requires gdata, ukfsst, maptools, ellipse, ncdf
}
\author{
Benjamin Galuardi
Large Pelagics Research Center
University of Massachusetts, Amherst

Maintainer:  <galuardi@eco.umass.edu>

Contributions from Chi Hin Lam, PhD
}
\references{
Many thanks to Francois Royer who created the basis for many functions herein as well as the logic involved. 
}

\keyword{sst}
\keyword{Kalman Filter}
\keyword{kftrack}

\seealso{
\code{\link{kftrack}}
\code{\link{ukfsst}}
\code{\link{trackit}}
\code{\link{gdata}}
\code{\link{maptools}}
\code{\link{GenKern}}
\code{\link{ncdf}}
\code{\link{maptools}}
\code{\link{adehabitat}}
\code{\link{RODBC}}
}
\examples{
# Example script

library(analyzepsat)
data(ATL, myramps)
# Data arranged by year in the DATA folder
Mydir = 'C:/MYDATA/';   
# Year fish was tagged 
tyear = 2008;		
# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.				  
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	
tagID = '12345'
tyear = 2008
#  If you follow this syntax you can easily do more than one tag within a loop, change years etc. 
xlsfile = paste(Mydir, '/',tyear, '/',tagID, '/',tagID, '.xls', sep = "")		

# This creates a list object filled with PSAT data. Change minmax flag to T if your fish report has these sheets. Use the 'try' command in case something doesn't work..
tagdata = MWTextract(tagID,  tyear,  xlsfile,  taglocfile, minmax = F)		

xtrack = prepf(tagdata, xmin=-100, xmax=0, ymin=10, ymax=55)
 
# Kalman filter without SST
fit = kftrack(xtrack[,1:5])			
 
#Load the bathymetry and map polygons for the N. Atlantic
bathy = get.bath.data(-100, 0, 0, 60, folder = getwd(), seaonly = T, res = 1)
# makes a data frame for bathymetric and/or longitudinal SST correction	
fmat = prepb(fit,xtrack,fill.sst = T)			
# Bathymetric correction 
btrack = make.btrack(fmat, bathy)						

#plot it!
data(ATL) # north Atlantic. Import shapefiles for other maps.
plot.btrack(btrack, bathy=bathy, add=F, map = map2)
 
 
#-----------------------------------------------------------------------------#
# 	To make the color ramps used in this package:
#-----------------------------------------------------------------------------#

# Mimics the jet colors from matlab. Good for SST
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# nice blue cascade for bathymetry
bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

#  ***OR***  use this if you get tired of all blues
bath.colors2 <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))

# nice earthy colors for land
land.colors<-  colorRampPalette(c("darkgreen", "greenyellow","burlywood4"))

# Also included via data(monames)
monames=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec')

}
