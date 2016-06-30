\name{make.env}
\alias{make.env}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct vertical habitat envelopes for concurrent temperature and depth data
}
\description{
Vertical habitat envelopes are constructed from a data frame containing concurrent temperature, depth and time information. 
This is done by summing the occurence of each combined temperature and depth occurence across a range of temperatue and depth bins.
Since MTI tags are delivered in time series fashion, \% time can't be used. 
This function may be used on recovered tag data as well (i.e. built from \code{\link{MWTextract.arch}}).

}
\usage{
make.env(allhv, fixz = F, plot = F, mbz = c(-250), mbt = c(25), log = T, mmar = c(4, 6, 2, 6), mcex = 1.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{allhv}{
data frame with date, dpeth, and temperature information. For MTI tags, this is typically delivered in a 15 minute or one hour time series. The allhv object may be contructed with the \code{\link{merge.hv}} and \code{\link{merge.tz}} functions.
}
  \item{fixz}{
For MTI tags, there is a limitation in the precision of depth related to change in depth between 15 minute hourly increments. If you didn't deal with this previously, it might be a good idea to flag this to TRUE
}
  \item{plot}{
Plots the log of the count of each ocurence of temperature and depth bin
}
  \item{mbz}{
Maximum depth bin used
}
  \item{mbt}{
Maximum temperature bin used
}
  \item{log}{
If TRUE, yields the log of the counts
}
  \item{mmar}{
If plotting, controls the margins
}
  \item{mcex}{
If plotting, controls the character size
}
}
\details{

}
\value{
A list containing a table of counts, temperature bins and depth bins
}
\references{
Luo, J., Prince, E. D., Goodyear, C. P., Luckhurst, B. E. & Serafy, J. E. Vertical habitat utilization by large pelagic animals: a quantitative framework and numerical method for use with pop-up satellite tag data Fisheries Oceanography, 2006, 15, 208-229
}
\author{
Benjamin Galuardi
}

\seealso{
\code{\link{merge.hv}},\code{\link{merge.tz}}
}
\examples{

# Random example
TT = rgamma(1000,10)
ZZ = rgamma(1000,20,.2)*-1
dat = data.frame(Ext_T = TT, depth = ZZ)
make.env(dat, plot = T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{habitat}

