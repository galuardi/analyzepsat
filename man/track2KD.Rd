\name{track2KD}
\alias{track2KD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a kernel density grid from a ukfsst or trackit estimated track.
}
\description{
This function uses a kernel density framework and generates a probability density (kernel) grid based on the uncertainy bounds estimated in the fit estimation. 
This function converts each location and its uncertainty into a gridded density.
The individual days are combined into a representative kernel density for the entire track.
This may be used on one or many animals simultaneously. 
}
\usage{
track2KD(track, xsize = 0.1, ysize = 0.1, range.x = c(-100, -5), range.y = c(20, 50))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
An estimated track follwing the format in \code{\link{make.btrack}}
}
  \item{xsize}{
Pixel size in the Longitudinal direction
}
  \item{ysize}{
Pixel size in the Latitudinal direction
}
  \item{range.x}{
Longitudinal limits
}
  \item{range.y}{
Latitude limits
}
}
\details{
This function uses functionality of the GenKern package.
}
\value{
%%  ~Describe the value returned
A probability density grid
}
\seealso{
\code{\link{kern2UD}}
}
\references{
Galuardi, B., Royer, F., Golet, W., Logan, J., Nielson, J. & Lutcavage, M. Complex Migration Routes of Atlantic Bluefin Tuna Question Current Population Structure Paradigm Can J. Fish. Aquat. Sci, 2010, 67, 966-976
}
\author{
Benjamin Galuardi
}

\keyword{Kernel}

