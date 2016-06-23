\name{MWT_extract}
\Rdversion{1.1}
\alias{MWT_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in data from a Microwave Telemetry Inc. fish report from a PSAT.
}
\description{
Reads in all data from a Microwave Telemetry Inc. fish report and parses all pieces into a list. Currently, the only information not readin is the light based locations while the PSAT tag is floating at the surface. Previously, this function used the RODBC library and several sub functions to connect to an excel file and read in the contents. This did not work for Linux users. The surrent version uses the functionality of the \code{\link{gdata}} package. While this is appreciably slower to implement, it it now cross platform.

}
\usage{
MWT_extract(tagID, tyear, xlsfile, taglocfile)
}

\arguments{
  \item{tagID}{
PTT number or other unique identifier of the tag to be read in. Must be of class 'character'
}
  \item{tyear}{
Year the fish was tagged. Must be of class 'numeric'
}
  \item{xlsfile}{
Fish report from MTI containing all your valuable data.
}
  \item{taglocfile}{
A text file containing tagging date and location in the following columns: Id, Year, Month, Day, Longitude (-180 to 180), Latitude. Do not use a header.
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

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste('Mydir', tyear, tagID, tagID, '.xls', sep = "/")

tagdata <- MWT_extract(tagID, tyear, xlsfile, taglocfile)

}

