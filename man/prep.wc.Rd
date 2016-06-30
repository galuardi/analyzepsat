\name{prepb.wc}
\Rdversion{2.0}
\alias{prepb.wc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tagbase to prepb object
}
\description{
Extracts a tag record from Tagbase for bathymetric correction. This applies only for WC tags. Additionally, a kf style fit is needed for this function. Additional work flow not included here is necessary}

\usage{
prepb.wc(fit = fit, tbpath = 'C:/mydata/Tagbase-fish.mdb', pttid = "123456", year=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{fit}{kf style fit. This may be kftrack, ukfsst or trackit}
  \item{tbpath}{path to Tagbase}
  \item{pttid}{PTT identifier}
}

\value{
dataframe. see \code{\link{prepb}}
}

\author{
Benjamin Galuardi
}

\examples{

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Tagbase}