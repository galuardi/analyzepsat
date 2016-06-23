\name{MWT_extract4}
\Rdversion{1.1}
\alias{MWT_extract4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in data from a Microwave Telemetry Inc. fish report from a PSAT.
}
\description{
Reads in all data from a Microwave Telemetry Inc. fish report and parses all pieces into a list. Currently, the only information not readin is the light based locations while the PSAT tag is floating at the surface. This uses RODBC library and several sub functions to connect to an excel file and read in the contents.

}
\usage{
MWT_extract4(tagID, tyear, tagDir, taglocfile)
}

\arguments{
  \item{tagID}{
PTT number or other unique identifier of the tag to be read in
}
  \item{tyear}{
Year the fish was tagged
}
  \item{tagDir}{
Where the data is stored. A good way to have this set up is by year, with all data from that year in the folder.
}
  \item{taglocfile}{
A text file containing tagging date and location in the following format (note that header is not neccesary):

TagID	Year	Month	Day		Longitude	Latitude
0001	2000	9		21		-35.41		40.11
0002	2001	10		22		-10.51		55.80
0003	2002	10		23		-90.10		21.54
}
}
\details{

}
\value{
A list with the following:
\item{x0}{Tagging Location}
 \item{day0}{Tagging date of Class 'date'} 
 \item{day0b}{Tagging date of Class 'POSIXct'}
 \item{xT}{pop-off location}
 \item{dayT}{pop-off Date of Class 'date'}
 \item{dayTb}{pop-off Date of Class 'POSIXct'}
 \item{fulldates}{Alldates fish was at liberty of Class 'date'. This does not neccesarily correspond to how many days of data are returned}
 \item{ SRSS }{Data frame of Sunrise and Sunset times. Columns are: Date, Sunrise and Sunset with SR and SS in minutes past midnight. Date is of Class'date' }  
\item{MWTxy}{Data Frame of Microwave Telemetry produced light based geolocations and corresponding dates. Columns are: Year, Month, Day,Latitude and Longitude (-180 to 180)}
 \item{T}{Matrix of ambient temperatures. Columns correspond to time of day GMT (typically 0-23.75 hours) and Rows corrspond to days at liberty. This matrix contains both observations and any times missing observations}
\item{Z}{Matrix of depth. Columns correspond to time of day GMT (typically 0-23.75 hours) and Rows corrspond to days at liberty. This matrix contains both observations and any times missing observations}     
\item{Argos}{Data Frame of Argos data returned while tag was floating. Columns are: Date-Time (character class), Argos Location Class, Latitude and Longitude.}
}
\references{
This framework was modified from work done by Francois Royer during his post-doc at the Large Pelagics Research Center.
}
\author{
Benajmin Galuardi
}
\note{

}

\seealso{
\code{\link{getTaggingLoc},\link{GetArgosPSAT},\link{getTaggingLoc},{getMWTxy},{getSRSS},{getPSATtemp},{getPSATdepth}}
}
\examples{
TagLocfile< - data(taglocfile.example,package = 'analyzepsat')
tagDir<- 'myTagDir'
tyear<- 2000
tagID<- 0001

tagdata<- MWT_extract4(TagLocfile,tagDir,tyear,tagID)

}

