\name{check.sst.from.server}
\Rdversion{1.1}
\alias{check.sst.from.server}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fixes a small glitch in the sst download capability within kfsst, ukfsst and trackit
}
\description{
NaN's are not allowed in SST files for the state space models kfsst, ukfsst and trackit to operate properly. This function reads in an individual SST file, removes any lines where NaN's occur and writes a new output file with an identical name. This will ovewrite the original file.
}
\usage{
check.sst.from.server(sstfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sstfile}{
A file downloaded via \code{\link{get.sst.from.server}}, or \code{\link{get.blended.sst}}
}
}
\details{
This may be used in a loop or vectorized to process entire folders. 
}
\value{
None. Outputs a corrected SST file, overwriting the old one.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{
There also seems to be a bug in the download in that manual unzipping is necessary... This has happened on several machines under both windows and Linux... Future versions of this function may address this.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.sst.from.server}}, \code{\link{get.blended.sst}}, \code{\link{kfsst}},\code{\link{trackit}}
}
\examples{
setwd('Myfolder')
library(ukfsst)
data(blue.shark)
get.sst.from.server(blue.shark, folder = 'Myfolder')


# unzip the downloaded file(s) manually...

sstfiles = list.files('Myfolder', pattern = '.xyz', full.names = T)
check.sst.from.server(sstfiles)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sst}
\keyword{ kfsst}% __ONLY ONE__ keyword per line
\keyword{ukfsst}
\keyword{trackit}
