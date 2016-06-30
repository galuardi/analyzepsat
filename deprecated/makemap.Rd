\name{makemap}
\Rdversion{1.1}
\alias{makemap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes a map from the GSHHS coastline dataset
}
\description{
This is a wrapper for functions in maptools library. It creates a spatial data object 
}
\usage{
makemap(xmin = -100, xmax = 0, ymin = 20, ymax = 65,mapdata="~/DATA/coast/gshhs/gshhs_i.b")
Follow the directions in maptools to download the gshhs dataset!
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmin}{
longitude minimum
}
  \item{xmax}{
Longitude maximum
}
  \item{ymin}{
Latitude minimum
}
  \item{ymax}{
Latitude maximum
}
}
\details{
The projection used for this function will be WGS 1984. If you want to change it, use the functionality in maptools and write your own function.
}
\value{

Taken from Rgshhs help file....

for polygon data, a list with the following components:
polydata 	data from the headers of the selected GSHHS polygons
belongs 	a matrix showing which polygon belongs to (is included in) which polygon, going from the highest level among the selected polygons down to 1 (land); levels are: 1 land, 2 lake, 3 island_in_lake, 4 pond_in_island_in_lake.
new_belongs 	a ragged list of polygon inclusion used for making SP
SP 	a SpatialPolygons object; this is the principal output object, and will become the only output object as the package matures
SP 	a SpatialLines object
}
\references{
http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html,

http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html (only old format - use for information only); data downloaded from

ftp://ftp.soest.hawaii.edu/pwessel/gshhs/ 

Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical, High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996. 

Nicholas J. Lewin-Koh, Roger Bivand, contributions by Edzer J. Pebesma, Eric Archer, Adrian Baddeley, Hans-Jorg Bibiko,
  Stephane Dray, David Forrest, Patrick Giraudoux, Duncan Golicher, Virgilio Gomez Rubio, Patrick Hausmann, Thomas
  Jagger, Sebastian P. Luque, Don MacQueen, Andrew Niccolai and Tom Short (2009). maptools: Tools for reading and handling
  spatial objects. R package version 0.7-23. http://CRAN.R-project.org/package=maptools
  
}
\author{
Benjamin Galuardi
}
\note{
I have this function set up for level 4, which is to include everything; islands, ponds, etc. This function was used to create the coastline dataset included in this package 
data(ghssh_map_100-30_0-60,package = 'analyzepsat')
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Rgshhs}},\code{\link{plotmap}}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ map}
\keyword{ gshhs }
