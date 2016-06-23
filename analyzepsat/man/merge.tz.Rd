\name{merge.tz}
\alias{merge.tz}
\alias{merge.tz.wctb}
\title{
Merge temperature and depth records from individual tags 
}
\description{
Build a single data frame of all temperature and depth records for a group of tags.
Also determines sunrise and sunset on a macro scale.  

Currently, this is supported only for MTI fish reports. Archival tag information should already contain merged temperature and depth information. For example, in a recovered MTI tag list (built from \code{\link{MWTextract.arch}}),the field tag$LTD contains a comparable list. The estimated track is used to determine day/night for observations.
}
\usage{
merge.tz(btrack, psat, tagID = NULL)
merge.tz.wctb(btrack, tagdata, tagID = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{btrack}{
Estimated fit from ukfsst or trackit converted to class btrack (see \code{\link{prepb}}). This may be a list of several tags, named with tagID. 
}
  \item{psat}{
List of psat tag data. Each item follows the data structure built using \code{\link{MWTextract}}. This may be a list of several tags, named with tagID. 
}
  \item{psat}{
List of psat tag data extracted from Tagbase \code{\link{tb2psat}}. 
}
  \item{tagID}{
If using a single psat record (not a list), you need to specify a tagID.
}
}
\value{
A dataframe of temperature, depth and day night designation.
}

\author{
Benjamin Galuardi
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{merge.hv}}, \code{\link{MWTextract}}
}

