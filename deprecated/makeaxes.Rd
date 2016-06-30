\name{makeaxes}
\Rdversion{1.1}
\alias{makeaxes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes axes on a map plot.
}
\description{
This actually isn't going to be used unless you make a map from the plotmap function in kftrack...
}
\usage{
makeaxes(xmin = -100, xmax = 10, xtick = 10, ymin = 0, ymax = 90, ytick = 10, font = 2, lwd = 2, xlab = " W", ylab = " N", col = 1, axis1 = T, axis2 = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmin}{
Longitude minimum
}
  \item{xmax}{
Longitude maximum
}
  \item{xtick}{
X tick interval
}
  \item{ymin}{
Latitude minimum
}
  \item{ymax}{
Latitude maximum
}
  \item{ytick}{
Latitude tick interval
}
  \item{font}{
Font type
}
  \item{lwd}{
Width of the axis line
}
  \item{xlab}{
X label
}
  \item{ylab}{
Y label
}
  \item{col}{
Color of the Axis
}
  \item{axis1}{
Default it T. If you don't want this one plotted, switch to 'F'
}
  \item{axis2}{
Default it T. If you don't want this one plotted, switch to 'F'
}
}
\details{

}
\value{
}
\references{

}
\author{
Benjamin Galuardi
}
\note{
Probaly this will not be used... The maptools mapping options are much nicer...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Rgshhs}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{axes }
\keyword{map }% __ONLY ONE__ keyword per line
