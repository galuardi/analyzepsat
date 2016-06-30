\name{tracks.kml}
\alias{tracks.kml}

\title{
Plot/animate finished tracks on Google Earth with satellite data
}
\description{
plots your finished tracks on Google Earth. Also adds SST or other imagery as a background. Final kml files include time stamps for animations.
}
\usage{
tracks.kml(tracklist = c("fit"), name = "", description = "", folder = getwd(), kmlname = "1set.kml", iconscale = 1, turnon = c(1, 0, 0, 1), showpath = c(1, 0, 0, 0), showpoint = c(1, 1, 1), npoints = 20, level = 0.95, getimage = T, product = c("TBAssta"), interval = c(5), local = F, lon360 = T, coastwatch.erddap = F, variable = c("sst"), colorbar = NA, imgname = NA, ...)
}
\arguments{
  \item{tracklist}{
objects to be passed to tracks.kml. This can be single or multiple objects and must be in quotations. The function can take either fitted objects from kftrack, ukfsst or trackit as well as a single data frame with the following 8 columns: year, month, day, hour, minute, second, longitude, latitude
}
  \item{name}{
the document name to appear in Google Earth's left-handed panel
}
  \item{description}{
the description to appear in Google Earth's left-handed panel
}
  \item{folder}{
the location to output the kml file and other supporting files
}
  \item{kmlname}{
the file name for the generated kml file
}
  \item{iconscale}{
scale of points and endpoints
}
  \item{turnon}{
output the following
a)most probable track
b)predicted track
c)nominal track
d)confidence intervals (variance on most probable track)
}
  \item{showpath}{
turn on/ make "visibile" the following tracks on Google Earth
a) most probable track
b) predicted track
c) nominal track
d) confidence interval
}
  \item{showpoint}{
turn on/ make "visible" the following waypoints on Google Earth
a) release point
b) recapture point
c) the collection of waypoints
}
  \item{npoints}{
number of points to describe confidence intervals
}
  \item{level}{
level for confidence intervals (usually 95\%)
}
  \item{getimage}{
True/ False flag to indicate whether satellite imagery should be obtained as a png file
}
  \item{product}{
specify the product names (7-character code) to be obtained from CoastWatch; 
	support multiple names, e.g. c("TBAssta", "TMHchla")
}
  \item{interval}{
how often (in days) imagery should be obtained, 
	for each of the specified products. e.g. c(5,8) for multiple products}
  \item{local}{
True/ False flag to indicate whether to pre-download the actual picture files 
	(in form of transparent portable network format files, .png). 
	True for faster performance in Google Earth
	False for streaming data through the Internet
}
  \item{lon360}{
True/ False flag indicating whether your data is better represented as 0-360 longitude (good for the Pacific) or -180 to 180 (good for Atlantic/Med)
}
  \item{coastwatch.erddap}{
True/ False flag for using the ERDDAP website as the satellite imagery source. False defaults to Bloomwatch
}
  \item{variable}{
list of variables from within satellite products. This is neccesary since several products have multiple variables (i.e. sst and sst anomoly)
}
  \item{colorbar}{
Flag for adjusting colorbar. See ERDDAP website for details
}
  \item{imgname}{
name of downloaded png image
}

}

\value{
none. external kml files returned
}
\references{

http://coastwatch.pfeg.noaa.gov/erddap/index.html
http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?
http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW180.jsp?
}
\author{
Chi Hin (Tim) Lam and Benjamin Galuardi
}
\note{

}

\seealso{
\code{\link{write.kml}}
}
\examples{
#single fit
data(big.241)
big.241$year = big.241$year+5  # most satellite products don't go back to 1999 :(
fit = kftrack(big.241)
tracks.kml('fit')

# multiple tracks

fit2 = kftrack(big.241, D.a=F)
fit3 = kftrack(big.241, D.a=F, sy.init = 1000, sy.a=F)

tracks.kml(ls(pattern = 'fit'))

# multiple products
tracks.kml("fit", name="Bigeye 241", product=c("TGAssta", "TMHchla"), interval=c(8,8), local=F)

}

\keyword{kml}
\keyword{satellite data}
