\name{prepb}
\Rdversion{1.1}
\alias{prepb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a dataframe for bathymetric and/or longitudinal sst correction
}
\description{
Makes a dataframe for bathymetric and/or longitudinal sst correction. This format is also useful for export to Excel, ArcMap, or other software. This does not currently work with trackit but future versions will have this capability. 

}
\usage{
prepb(kfit, prepf, fill.sst = F, span = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kfit}{
A fitted object returned from kftrack, ukfsst, kfsst
}
  \item{xtrack}{
The original input dataframe to kftrack, ukfsst or kfsst
}
  \item{fill.sst}{
Logical. Should days with missing SST be interpolated? If TRUE, loess smoothing is applied.
}
  \item{span}{
span, or bandwidth, for loess smoothing if fill.sst = TRUE
}
}
\details{
LOESS smoothing is more accurate the less data is missing. Bandwidth is always an important consideration and should be reduced if there is a long time series with relatively little missing data. It is also worth noting that interpolating SST, which is most cases is considered the maximum temperature per day, will be biased towards colder regions if the fish is consistently deep over the measured time period. 
}
\value{
A dataframe with columns for Year, Month, Day, V11, V12, V21, V22, Longitude, Latitude, Maxdepth and Max Temp
}
\references{
}
\author{
Benjamin Galuardi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{prepf},\code{\link{make.btrack}}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sst}
\keyword{ loess}% __ONLY ONE__ keyword per line
