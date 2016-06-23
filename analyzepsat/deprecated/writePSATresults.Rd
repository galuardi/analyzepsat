\name{writePSATresults}
\Rdversion{1.1}
\alias{writePSATresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes an Excel file of all estimated track results
}
\description{
Writes an Excel file of all estimated track results utilizing RODBC functionality
}
\usage{
writePSATresults(xtracks, file = "PSAT_output.xls", Temp = F, Depth = F, psat = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtracks}{
List of all final estimated tracks. This must be a named list.
}
  \item{file}{
filename for output file. Once this is spcified it may not be specified again unless you erase the original.
}
  \item{Temp}{
Should temperature matrix be saved?
}
  \item{Depth}{
Should depth matrix be saved?
}
  \item{psat}{
If either Temp or Depth = T, this is the corresponding object created by MWT_extract4. Typically, this is a large list of objects corrsponding to a group of PSATs. This should be named exactly as the xtarcks object so that the appropriate Temperature/Depth is matched with the final estimated track.
}
}
\details{

}
\value{
Returns an Excel file (.xls) containing worksheets with names corresponding to the PTT ID or other unique identifier. 
}
\references{

}
\author{
Benjamin Galuardi
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

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

writePSATresults(btrack,file='output.xls',Temp=T,Depth=T,psat=tagdata)

}

