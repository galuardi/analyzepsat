\name{prepf}
\Rdversion{1.1}
\alias{prepf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate an object for kftrack or kfsst (or ukfsst) estimation
}
\description{
This function takes an object created by \code{\link{MWTextract}} and returns a data frame with 7 columns suitable for kf estimation. This also sets some limits on where in the world the fish may have gone. These limits are meant to be extremely broad and exist to filter only the most egregiously erroneous light measurements.
}
\usage{
prepf(tag, xmin = 100, xmax = 0, ymin = 10, ymax = 55, keepall = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tag}{
object returned from MWTextract
}
  \item{xmin}{
Southern longitude cutoff
}
  \item{xmax}{
Northern longitude cutoff

}
  \item{ymin}{
Southern latitude cutoff

}
  \item{ymax}{
Northern latitude cutoff
}
  \item{keepall}{
If you do not wish to use these cutoffs, keepall = T. Default is keepall = F
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Dataframe with 7 columns: Day, Month, Year, Longitude, Latitude, Max Daily Depth, Max Daily Ambient Temperature
}
\references{

}
\author{
Benjamin Galuardi
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kftrack}},\code{\link{kfsst}}
}
\examples{

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste('Mydir', tyear, tagID, '/', tagID, '/', '.xls', sep = "")

tagdata <- MWTextract(tagID, tyear, xlsfile, taglocfile)

xdata=prepf(tagdata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{kalman filter}
\keyword{}% __ONLY ONE__ keyword per line
