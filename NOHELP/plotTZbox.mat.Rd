\name{plotTZbox.mat}
\Rdversion{1.1}
\alias{plotTZbox.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotTZbox.mat(tzdata, zlim = NULL, fcn = mean, font = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tzdata}{
%%     ~~Describe \code{tzdata} here~~
}
  \item{zlim}{
%%     ~~Describe \code{zlim} here~~
}
  \item{fcn}{
%%     ~~Describe \code{fcn} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
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
function(tzdata,zlim=NULL,fcn=mean,font=1){
   jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
   require(date)
   require(matlab)
   dataZ=t(tzdata$Z)
   dataT=t(tzdata$T)

  if(!is.null(zlim)){
   dataT[dataT<(zlim)]=NaN
  }

 #layout(matrix(c(rep(1,10),2),1,11,byrow=T)) 
  #par(mar = c(7,5,2,1) + 0.1)
  boxplot.matrix(dataZ,col=jet.colors(33)[apply(dataT,2,fcn,na.rm=T)],varwidth=T,axes=F,font=font)
  xidx=(dim(dataT)[2])#dim(dataT)[1]*
  day0=tzdata$day0
  dayT=tzdata$dayT
   if(day0>700000){
    day0=day0-715876+1 # matlab date conversion
	dayT=dayT-715876+1
   }
   tzdates=seq(day0,dayT+1)
   xp <- par("xaxp")
   xcut <- round(seq(xp[1], xidx, length = 12))
   dcut=seq(day0, dayT, length = 12)
   xcut[1]=1
   axis(1,at=xcut,label=paste(as.date(dcut),font=font),las=2);axis(2,font=font)
   box()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
