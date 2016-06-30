\name{merge.hv}
\alias{merge.hv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge lat/long data with temperature and depth for Microwave Telemetry tags
}
\description{
Generate a data frame utlizing estimated tracks from MTI tagged animals and corresponding temperature and depth data. 
Utilizes the data frame structure in \code{\link{make.btrack}} and \code{\link{MWTextract}}. 
}
\usage{
merge.hv(tracks, TZ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tracks}{
either a single data frame of estimated tracks, with 'tagID' as an identifier, or a named list of estimated tracks named with tag IDs.
In either case, tagID must match between tracks and TZ
}
  \item{TZ}{
A data frame of temperature and depth. Must contain tagID and date in ISO format
}
}

\value{
A data frame of linked temperature, dpeth and lat/long data. There will be a 'one to many' relationship between a single temp/depth and a location since we get one location/day.
}
\author{
Benjamin Galuardi
}


\seealso{
\code{\link{merge.tz}}, \code{\link{make.env}}
}

