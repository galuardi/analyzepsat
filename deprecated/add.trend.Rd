\name{add.trend}
\Rdversion{1.1}
\alias{add.trend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a daily trend to a time series style temperature or depth plot
}
\description{
Adds a daily trend to a time series style temperature or depth plot. This may be any summary style statistic such as mean, median, max, etc.
}
\usage{
add.trend(dataT, fcn = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataT}{
Temperature or Depth object parsed from a MWT_extract4 object
}
  \item{fcn}{
Function to be plotted for each day
}
}

\author{
Benjamin Galuardi
}

\seealso{
\code{\link{plot.psat.TZ}},\code{\link{plotTZprofile}}
}
\examples{
  dataT=matrix(rnorm(25000,0,1),500,500)	
  plot.psat.TZ(dataT,Z=1,axes=F)
  add.trend(psat[[k]]$Z,mean)
}
\keyword{trend}

