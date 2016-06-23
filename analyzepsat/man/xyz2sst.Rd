\name{xyz2sst}
\Rdversion{2.0}
\alias{xyz2sst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
xyz2sst grid
}
\description{
converts an xyz file of sea surface temperature created for ukfsst (or trackit) to a matrix for display
}
\usage{
xyz2sst(sstfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{sstfile}{an xyz file}
}

\value{
an asc grid of sea surface temperature
}

\author{
Benjamin Galuardi
}

\examples{
library(fields)
data(blue.shark)
get.reynolds(blue.shark)
sstfiles = .sstFileVector
image.plot(xyz2sst(sstfiles[1]))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}
