\name{getMWTarch}
\Rdversion{2.1}
\alias{getMWTarch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in data from a Microwave Telemetry Inc. fish report from a recovered PSAT.
}
\description{
Reads in all data from a Microwave Telemetry Inc. fish report for a recovered tag. This function parses all parts of the fish report into a list. Currently, this is for Windows only (due to ODBC requirements)
}
\usage{
getMWTarch(xlsfile, tagID, tyear, taglocfile, dayT=NULL, xT=NULL)
}

\arguments{
  \item{xlsfile}{
Fish report from MTI containing all your valuable data.
}  
\item{tagID}{
PTT number or other unique identifier of the tag to be read in. Must be of class 'character'
}
  \item{tyear}{
Year the fish was tagged. Must be of class 'numeric'
}
  \item{taglocfile}{
A text file containing tagging date and location in the following columns: Id, Year, Month, Day, Longitude (-180 to 180), Latitude. Do not use a header.
}
\item{dayT}{if known, the last day of the track (e.g. recapture date). Most times this is not known}

\item{xT}{if known, the last position of the track (e.g. recapture point). Most times this is not known}
}

\details{
NOTE! If you have a recovered, unreported tag,  you MUST input an endpoint AND a  last day. 
For example: xT = c(-40,72), dayT = mdy.date(10,1,2011)
Otherwise you will incur an error.
}
\value{
		
A list with the following:
\item{tagID}{provided ID for the tag}
\item{x0}{Tagging Location}
 \item{day0}{Tagging date of Class 'date'} 
 \item{day0b}{Tagging date of Class 'POSIXct'}
 \item{xT}{pop-off location}
 \item{dayT}{pop-off Date of Class 'date'}
 \item{dayTb}{pop-off Date of Class 'POSIXct'}
 \item{fulldates}{Alldates fish was at liberty of Class 'date'. This does not neccesarily correspond to how many days of data are returned}
 \item{ SRSS }{Data frame of Sunrise and Sunset times. Columns are: Date, Sunrise and Sunset with SR and SS in minutes past midnight. Date is of Class'date' }  
\item{MWTxy}{Data Frame of Microwave Telemetry produced light based geolocations and corresponding dates. Columns are: Year, Month, Day,Latitude and Longitude (-180 to 180)}
\item{LTD}{All archived light, temperature and depth}
\item{maxt}{daily maximum temperature}
\item{maxz}{daily maximum (deepest) depth}
\item{mmt}{daily minimum and maximum temperature}
\item{mmz}{daily minimum and maximum depth}
}

\author{
Benjamin Galuardi
}
\note{

}


