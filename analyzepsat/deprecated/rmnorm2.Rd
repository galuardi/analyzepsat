\name{rmnorm2}
\Rdversion{1.1}
\alias{rmnorm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a sample from a multivariate normal distribution
}
\description{
This is identical to a function found in the QRMlib library. I copied it here for ease of use without having another library to load.
}
\usage{
rmnorm2(n, Sigma = equicorr(d, rho), mu = rep(0, d), d = 2, rho = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of points to generate
}
  \item{Sigma}{
Covariance matrix
}
  \item{mu}{
mean of the distribution
}
  \item{d}{
number of columns for the output
}
  \item{rho}{
not used.... 
}
}
\details{

}
\value{
dataframe of two columns, in this case it will be longitudes and latitudes drawn from a bivariate normal distribution.
}
\references{
Alexander McNeil for S-Plus original; R port by Scott Ulman (2008). QRMlib: Provides R-language code to examine
  Quantitative Risk Management concepts. R package version 1.4.4. http://www.ma.hw.ac.uk/~mcneil/book/index.html
}
\author{
Alexander McNeil for S-Plus original; R port by Scott Ulman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rmnorm}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
\keyword{normal }% __ONLY ONE__ keyword per line
