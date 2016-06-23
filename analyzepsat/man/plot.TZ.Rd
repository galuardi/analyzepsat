\name{plot.psat.TZ}
\Rdversion{1.1}
\alias{plot.TZ}

\title{
Plot either temperature or depth from a PSAT tag
}
\description{
Plots either temperature or depth from a PSAT tag from an object of type returned from MWT_extract5 containing a 'T' and 'Z' component.
}
\usage{
plot.TZ(tzdata, Z = 1, dcol = 2, axes = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tzdata}{
Object of type returned from \code{\link{MWTextract}} containing a 'T' and 'Z' component.
}
  \item{Z}{
Either '1' or '2'. '1' is for plotting Depth, '2' is for plotting temperature.
}
  \item{dcol}{
Color of plotted points and lines.
}
  \item{axes}{
Should axes be plotted; Default is T
}
}
\details{

}
\value{
A time series style plot of either depth or temperature
}
\references{
}
\author{
Benjamin Galuardi
}
\note{

}

\seealso{

}
\examples{

}
