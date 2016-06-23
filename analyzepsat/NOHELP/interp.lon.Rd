\name{interp.lon}
\Rdversion{1.1}
\alias{interp.lon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolates longitude and SST via loess smoothing
}
\description{
Since UKFSST and KFSST accept data frames with missing Latitudes but not longitudes, interpolating ligh based measurements which are extreme outliers allows one to keep as much information as possible prior to state space estimation. This function implement this for longitude and SST simultaneously. Longitudes which fall outside two standard deviations from the loess residual (loess - observation) are removed as bad data. 

This should be used judiciously and is not recommended if there is a very large amount of missing data. This is ideally used when there is a few days of missing or outlying observaions. 
}
\usage{
interp.lon(xtrack, ukf = F, span = 0.25, fill.sst = F, span.sst = 0.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtrack}{
data frame constructed through \code{\link{make.xtrack}}
}
  \item{ukf}{
If the track will be analyzed using ukfsst or kfsst, set this to T
}
  \item{span}{
Bandwidth for the longitude loess smoothing. If the track is long, this may be safely reduced.
}
  \item{fill.sst}{
If the track will be analyzed using ukfsst or kfsst, this may be set to T. This can be ignored if xtrack has no missing SST information.
}
  \item{span.sst}{
Bandwidth for the SST loess smoothing. If the track is long, this may be safely reduced.
}
}

\value{
A dataframe with no missing longitudes, no missing SST, and outlier latitudes flagged as NA
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}

\seealso{
\code{\link{loess}},\code{\link{make.xtrack}}
}
