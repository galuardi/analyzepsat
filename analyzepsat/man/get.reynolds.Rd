\name{get.reynolds}
\Rdversion{2.1}
\alias{get.reynolds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download SST data and parse input files for ukfsst.
}
\usage{
get.reynolds(track, folder = tempdir(), removeland = TRUE, save = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
an input track for ukfsst
}
  \item{folder}{
A place to save your data
}

}

\value{
files for use in ukfsst
}

\author{
Benjamin Galuardi and Chi Hin Lam
}

\examples{

data(blue.shark)
get.reynolds(blue.shark)
fit = kfsst(blue.shark)
plot(fit)
}

\keyword{SST}

