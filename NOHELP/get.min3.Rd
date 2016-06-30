\name{get.min3}
\Rdversion{1.1}
\alias{get.min3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.min3(lon1, lat1, lon2, lat2, lon3, lat3, samp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
%%     ~~Describe \code{lon1} here~~
}
  \item{lat1}{
%%     ~~Describe \code{lat1} here~~
}
  \item{lon2}{
%%     ~~Describe \code{lon2} here~~
}
  \item{lat2}{
%%     ~~Describe \code{lat2} here~~
}
  \item{lon3}{
%%     ~~Describe \code{lon3} here~~
}
  \item{lat3}{
%%     ~~Describe \code{lat3} here~~
}
  \item{samp}{
%%     ~~Describe \code{samp} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lon1,lat1,lon2,lat2,lon3,lat3,samp){
  idx<-which.min((lon1-samp[,1])^2+(lat1-samp[,2])^2+(lon2-samp[,1])^2+(lat2-samp[,2])^2+(lon3-samp[,1])^2+(lat3-samp[,2])^2)
  c(samp[idx,1],samp[idx,2])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
