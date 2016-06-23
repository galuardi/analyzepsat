\name{UD.area}
\alias{UD.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate area of UD
}
\description{
Returns the area for a range of utilization distribution levels (contours).
}
\usage{
UD.area(x, levels = seq(0.2, 0.95, by = 0.05))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A UD grid in asc format. This may be generated using \code{\link{kern2UD}}
}
  \item{levels}{
UD levels for area calculations
}
}

\value{
A data frame of levels and corresponding area in km^2
}

\author{
Benjamin Galuardi
}
\seealso{
\code{\link{kern2UD}}, \code{\link{track2KD}}
}