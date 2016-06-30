\name{plot.by.month}
\Rdversion{1.1}
\alias{plot.by.month}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots an estimated track according to month
}
\description{
Plots an estimated track according to month. Uses a month.colors scheme where summer months (northern hemisphere) are orange to red and winter months are blues. 
}
\usage{
plot.by.month(ttrack, saveplot = F, filename = NULL, cex = 2, pch = 21)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ttrack}{
estimated track to be plotted
}
  \item{saveplot}{
Should the plot be saved for later?
}
  \item{filename}{
If saveplot = T, what should the filename be?
}
  \item{cex}{
Size of points to be plotted
}
  \item{pch}{
Type of points to be plotted
}
}
\details{
NOTE: this does not produce a stand alone plot. This is meant to be added to a basemap or other plot.
}
\value{
A plot.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{
Somewhat deprecated. This function is now wrapped in the plot.btrack function. Still, the example should work...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.btrack}}
}
\examples{
library(kftrack)
library(maptools)
bathy <- data(etopo2,package = 'analyzepsat')
map2 <- data(ghssh.NATL,package = 'analyzepsat')

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste('Mydir', tyear, tagID, '/', tagID, '/', '.xls', sep = "")

tagdata <- MWT_extract5(tagID, tyear, xlsfile, taglocfile)

xdata=make.xtrack(tagdata)
fit=kftrack(xdata[,1:5])
fmat <- as.data.frame(cbind(xdata[,1:3],fit$var.most.prob.track,fit$most.prob.track,xdata[,c(7:6)]))
btrack <- make.btrack(fmat,bathy,mintype=2)
btrack[,8]=btrack[,8]+360
plot(map2$SP, col="khaki", pbg="azure2", xaxs="i", yaxs="i", axes=TRUE)
plot.by.month(btrack)
}




