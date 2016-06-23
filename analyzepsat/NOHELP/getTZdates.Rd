\name{getTZdates}
\Rdversion{1.1}
\alias{getTZdates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTZdates(tres, day0b, dayTb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tres}{
%%     ~~Describe \code{tres} here~~
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
function(tres,day0b,dayTb){
  tdates=strptime(tres[,1],"\%m/\%d/\%Y \%H:\%M")
 if(is.na(as.numeric(tdates[2]))){
     tdates=strptime(tres[,1],"\%m-\%d-\%Y \%H:\%M")
 }
  if(is.na(as.numeric(tdates[2]))){
     tdates=strptime(tres[,1],"\%Y-\%m-\%d \%H:\%M")
 }
tidx=find(tdates<=dayTb+86400&tdates>=day0b)
tidx=tidx[1:(length(tidx)-1)]
res=res[tidx,]
T=res[,2]
tdates=tdates[tidx]
tdates=tdates[tdates>0]
tdates=tdates[!is.na(tdates)]
T=T[T>0];
T=T[!is.na(T)];
timeidx=diff(tdates)
tstep=as.numeric(min(timeidx[timeidx>0])/60)
if(tstep<1){
 tstep=as.numeric(min(timeidx[timeidx>0]))/60
  }
if(dim(res)[2]==3){
   Tgain=tres[,3]
   return(tdates,tres,T,tstep,Tgain)
 }else{
   return(tdates,tres,T,tstep)
  }
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
