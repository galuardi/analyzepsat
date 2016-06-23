\name{get.sst.file.vector}
\Rdversion{1.1}
\alias{get.sst.file.vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves a list of previously downloaded sst files for use in kfsst or ukfsst (or trackit) analysis
}
\description{
This function was inspired to reduce processing time if you have downloaded a large numuber (and/or spatial extent) of sst files during the kfsst or ukfsst procedures. Without this step, the functions will use all files included in the 'localsstfolder'. This function will retrieve only those relevent in time to the track being analyzed. There is also functionality via(\code{\link{reduce.sst.file}}) to reduce the file size spatially to the limits of the tracks observations. 

}
\usage{
get.sst.file.vector(track, sstdir, tempdir = NULL, reduce = F, buffer = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
The track to be analyzed
}
  \item{sstdir}{
Folder conatinaing all your downloades sst files
}
  \item{tempdir}{
Folder to store the subset of files. This may be cleared afterwards without losing any downloaded files.
}
  \item{reduce}{
Toggles the file size and spatial reduction
}
  \item{buffer}{
Amount of buffer. A good default is 2 (degrees).
}
}

\value{
an 'invisible' file vector used in ukfsst, kfsst (or trackit) analysis, and modified sst files stored in tempdir. 
}

\author{
Benjamin Galuardi
}
\note{
This entire procedure shouldbe used AFTER using \code{\link{check.sst.from.server}}
}


\seealso{
\code{\link{check.sst.from.server}},\code{\link{reduce.sst.file}}
}
\examples{
library(ukfsst)

# Track to be analyzed
data(blue.shark)

# download too much sst data..
faketrack = data.frame(day = c(1, 30), month = c(6,10),year = c(2000, 2002),lon = c(150, 250),lat=c(0, 50))
sstdir = 'C:/SST/REYNOLDS'
get.sst.from.server(faketrack, folder = sstdir)

# make a place to store the sst subset
tempdir = 'C:/SST/REYNOLDS/subset/'

get.sst.file.vector(blue.shark, sstdir = sstdir, tempdir = tempdir, reduce = T, buffer = 2)

fit = kfsst(blue.shark, localsstfolder = tempdir)

}
