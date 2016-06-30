\name{plot.minmax}
\Rdversion{1.1}
\alias{plot.minmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the minimum and maxixmum daily remperature or depth. Not ususally used...
}
\description{
Plots the minimum and maximum depth and/or temprature returned from a MTI fish report
}
\usage{
plot.minmax(minmax, new = F, Temp = F, Depth = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minmax}{
An object returned by minmax_extract.r function.
}
  \item{new}{
New plot. Defautlt is 'F'
}
  \item{Temp}{
Temperature is plotted
}
  \item{Depth}{
Depth is plotted
}
}
\details{
The minmax object may or may not be part of an object of type MWT_extract4
}
\value{
A plot of min amx depth or temperature
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste('Mydir', tyear, tagID, '/', tagID, '/', '.xls', sep = "")

tagdata <- MWT_extract5(tagID, tyear, xlsfile, taglocfile)

mmdata <- minmax_extract(tagID,tagdata,plot=F)
par(mfrow=c(1,2))
plot.minmax(mmdata,new=F,Temp=T,Depth=F)
plot.minmax(mmdata,new=F,Temp=F,Depth=T)
}
