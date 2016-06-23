\name{plotTZprofile}
\Rdversion{1.1}
\alias{plotTZprofile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the depth and temperature using the depth time series colored by temperature.
}
\description{
Plots the depth and temperature using the depth time series colored by temperature.
This uses temperature only where depth measurements were recorded.
}
\usage{
plotTZprofile(tzdata, zlim = NULL, pch = 21, cex = 1.2, font = 1, cex.lab = 1.2, cbrks = 33, ylab = "Depth (m)", cex.axis = 1, MWT = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tzdata}{
Object of type returned from \code{\link{MWTextract}} containing a 'T' and 'Z' component.
}
  \item{zlim}{
If desired this will restrict the depth limits for the plot. This may be useful if tehre are one or two spurious depth measurements.
}
  \item{cbrks}{
Number of color breaks for temperature
}
  \item{legend}{
Plots an SST legend
}
  \item{MWT}{
Indicates if this data frame is of class psat
}
\item{}{
All other plot inputs are standard plot commands \code{\link{par}}
}
}
\details{
}
\value{
Returns a time series style plot 
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

