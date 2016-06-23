\name{fill.sst.vals}
\Rdversion{1.1}
\alias{fill.sst.vals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fills values using loess smoothing
}
\description{
This function is designed to interpolate missing SST values in an xtrack dataframe prior to ukfsst or other sst correction. 
}
\usage{
fill.sst.vals(xtrack, span = 0.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtrack}{
Dataframe returned from \code{\link{make.xtrack}}
}
  \item{span}{
Span or smoothing bandwidth for \code{\link{loess}} smoothing
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe identical to the xtrack input except with the MaxTemp column interpolated.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{
This function may not work if there is too many days of missing temperature data near the start and/or end dates.  This is easlit rectified by filling in MaxTemp for start and end points by referencing SST charts.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{loess}},\code{\link{make.xtrack}},\code{\link{make.fmat}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{loess}
\keyword{sst}% __ONLY ONE__ keyword per line
