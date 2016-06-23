\name{makeCI}
\Rdversion{1.1}
\alias{makeCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
genrate a confidence interval for a kftrack estimated point
}
\description{
this is exactly the same as .CI.reg in the kftrack package except that this generates solid or translucent polygons.
}
\usage{
makeCI(point,col=rgb(.7,.7,.7,alpha=.15),border=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{point}{
Six column vector containing the covariance matrix and estiamted position. Typically in the form V11 V12 V21 V22 Lon Lat
}
 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A confidence interval.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{
 These may be saved for future plotting.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.by.month}}
}
\examples{
not normally used by itself.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval}
\keyword{ covariance matrix }% __ONLY ONE__ keyword per line
