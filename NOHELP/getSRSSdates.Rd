\name{getSRSSdates}
\Rdversion{1.1}
\alias{getSRSSdates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSRSSdates(res, MWTdates, day0, dayT, day0b, dayTb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{MWTdates}{
%%     ~~Describe \code{MWTdates} here~~
}
  \item{day0}{
%%     ~~Describe \code{day0} here~~
}
  \item{dayT}{
%%     ~~Describe \code{dayT} here~~
}
  \item{day0b}{
%%     ~~Describe \code{day0b} here~~
}
  \item{dayTb}{
%%     ~~Describe \code{dayTb} here~~
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
function(res,MWTdates,day0,dayT,day0b,dayTb){
if(class(res[1,1])=='Date'){ 
	SRSSdates=sort(as.date(levels(res[,1])))   # get the dates from the data         
	SRSSdates= SRSSdates[SRSSdates<=dayT&SRSSdates>=day0]   # get the dates of fish at liberty
	}else{
		SRSSdates=sort(res[,1]) 
		SRSSdates= SRSSdates[SRSSdates<=dayTb&SRSSdates>=day0b]   # get the dates of fish at liberty
	}

didx=(1:length(SRSSdates))*0
for(i in 1:length(SRSSdates)){
  didx[i]=which.min(as.numeric(MWTdates-SRSSdates[i])^2)
  }
didx=unique(didx)
didx
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
