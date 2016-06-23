\name{thin.trackit.dates}
\Rdversion{1.1}
\alias{thin.trackit.dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thins results from Trackit to one location per day
}
\description{
Thins results from Trackit to one location per day. Please feel free to modify this anyway you see fit.
}
\usage{
thin.trackit.dates(fmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmat}{
fmat is a dataframe returned from \code{\link{make.fmat}} which contains columns for Year, Month, Day, V11, V12, V21, V22, Longitude, Latitude, Maxdepth and MaxTemp
}
}
\details{
}
\value{
a dataframe exactly like the input except thinned to one location per day.
}

\author{
Benjamin Galuardi
}

\seealso{
\code{\link{make.fmat}},
\code{\link{trackit}},
\code{\link{date}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ trackit}
\keyword{date}
