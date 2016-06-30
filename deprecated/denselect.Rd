\name{denselect}
\Rdversion{1.0}
\alias{denselect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select by density
}
\description{
Function to select the highest probable xy location from a group of locations. This uses the 'density' command to find the highest density among a histogram with several peaks. Essentially, this delivers a location where most data is clustered. 
}
\usage{
denselect(samp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samp}{
two column matrix or dataframe with 'x' and 'y' components
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
xy pair, typically used for a longitude latitude pair.

}

\author{
Benjamin Galuardi
}
\note{

}

\examples{
samp=cbind(rnorm(100,01,1),rgamma(100,2,.01))
denselect(samp)

}

