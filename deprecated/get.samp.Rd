\name{get.samp}
\Rdversion{1.1}
\alias{get.samp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Samples froma multivariate normal
}
\description{
This is just a wrapper for rmnorm2
}
\usage{
get.samp(vec, npoints, ci = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
lon and lat
}
  \item{npoints}{
number of points to sample
}
  \item{ci}{
Confidence interval from which to draw samples
}
}

\author{
Benjamin Galuardi
}

\seealso{
\code{\link{rmnorm2}},\code{\link{lon.match.sst}},\code{\link{make.btrack}},
}
