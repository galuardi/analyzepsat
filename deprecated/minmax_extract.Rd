\name{minmax_extract}
\Rdversion{1.1}
\alias{minmax_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts the minimum and maximum tempeerature and depth from a MTI fish report
}
\description{
This is only used in special circumstances when there is a dedicated sheet provided by MTI with this information.
}
\usage{
minmax_extract(tagID, tag, plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tagID}{
PTT ID or other unique tag identifier
}
  \item{tag}{
This is a list object created by the \code{\link{MWT_extract5}} function
}
  \item{plot}{
if TRUE, a plot will be produced. Usually set to FALSE.
}
}
\details{
This was only used once when a programming error in the tags failed to record archived data prior to transmission. Minimum and max temperatures and depth were recorded, hence the writing of this function.
}
\value{
\item{tminmax}{Data frame of Date, minimum temperature and maxiimum temperature}
\item{pminmax}{Data frame of Date, minimum depth and maxiimum depth}
}
\references{
This framework was modified from work done by Francois Royer during his post-doc at the Large Pelagics Research Center.
}
\author{
Benjamin Galuardi
}
\note{
Not normally run by itself so no example provided.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MWT_extract5}}
}
\examples{
}
