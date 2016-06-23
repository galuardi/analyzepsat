\name{reduce.sst.file}
\Rdversion{1.1}
\alias{reduce.sst.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduces an sst file downloaded during ukfsst or kfsst (or trackit) analysis
}
\description{
This function is useful if a large amount of files, covering a large  spatial area, were downloaded for a group of tracks. This function is used to reduce the file to a more manageable size. Normally, this is not used by itself but is built into \code{\link{get.sst.file.vector}}
}
\usage{
reduce.sst.file(sstfile, xtrack, outfile, buffer = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sstfile}{
Full name of the sst file
}
  \item{xtrack}{
The track to be analyzed. 
}
  \item{outfile}{
Name of the output file. Normally, this is identical to the input with a different home folder preceding the name.
}
  \item{buffer}{
To avoid points falling outside the 'Quadmap' (annoying error by the way...), a few degrees are added to the borders of the sst region.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an sst file in xyz format, with no NaN's 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.sst.file.vector}}, \code{\link{get.sst.from.server}}
}

