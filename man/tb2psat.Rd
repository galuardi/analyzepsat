\name{tb2psat}
\Rdversion{2.0}
\alias{tb2psat}
\alias{tb2btrack}
\alias{prepb.wc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tagbase to psat or btrack object
}
\description{
tb2psat extracts a Microwave Telemetry, Inc. tag record, from Tagbase, for analyzepsat analysis. 

tb2btrack extracts previously processed tracks. This can then be used in subsequent plots or utilizaition distribution analysis. 

prepb.wc returns a data frame for bathymetric correction of a Wildlife Computers PAT.

These functions require use of Tagbase (https://code.google.com/p/tagbase/) for storage of PSAT data. These functions query the database and return objects for analysis and display. 
}
\usage{
tb2psat(tbpath = 'C:/mydata/Tagbase-fish.mdb', pttid = "123456", recovered=F, ...)
tb2btrack(tbpath = 'C:/mydata/Tagbase-fish.mdb', tagid = 100, is.ptt = F)
prepb.wc(fit = fit, tbpath = 'C:/mydata/Tagbase-fish.mdb', pttid = "123456", year=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{tbpath}{path to Tagbase database}
  \item{pttid}{PTT identifier}
  \item{fit}{a kftrack, ukfsst or trackit object}
  \item{recovered}{logical; does the tag record corespond to a recovered tag?}
  \item{tagid}{May be either indexed or PTT}
  \item{is.ptt}{If using PTT number, flag to TRUE}
}

\value{
list. see \code{\link{MWTextract}}
-or-
data.frame
}

\author{
Benjamin Galuardi
}

\examples{

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Tagbase}