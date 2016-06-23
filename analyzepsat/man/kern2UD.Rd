\name{kern2UD}
\alias{kern2UD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a kernel density grid into a utiliation distribution
}
\description{
Converts a kernel density grid into a utiliation distribution.
This is represented in a percent volume.
}
\usage{
kern2UD(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A kernel density grid built using \code{\link{track2KD}}
}
}
\details{
This function is a wrapepr for methods in \code{\link{adehabitat}} applied to estimated tracks generated using \code{\link{ukfsst}} and/or \code{\link{trackit}}.
}
\value{
Utilization distribution (volume) grid in asc format. 
}
\references{
Calenge, C. (2006) The package adehabitat for the R software: a tool for the analysis of space and habitat use by animals. Ecological Modelling, 197, 516-519 
}
\author{
Benjamin Galuardi
}

\seealso{
\code{\link{adehabitat}},\code{\link{track2KD}}, \code{\link{ukfsst}}, \code{\link{trackit}}
}


