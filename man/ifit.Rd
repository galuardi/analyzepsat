\name{ifit}
\Rdversion{2.0}
\alias{ifit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate a fitted kf object
}
\description{
This is a wrapper for the crawl package for R (see references), modified to benefit kf fitted objects. CRAWL uses Kalman filtering to smooth and interpolate where dat is missing. We use the estimated positions and variance structure from kf objects as inputs. 
}
\usage{
ifit <- function(ukfit, psat = NULL, bath = bath, plot = F, getgmt = F, map=map, cilev = .95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ukfit}{any kf fitted object (kftrack, ukfsst, trackit), or, a btrack object. In the case of a btrack object, psat may remain NULL.}
\item{psat}{a psat object, if needed (see \code{\link{MWTextract}})}
\item{bath}{a bathymetric list (see \code{\link{get.bath.data}})}
\item{plot}{logical. Plots diagnostic plots of intermediate steps in the interpolation. Best if used to create a pdf.}
\item{getgmt}{if GMT is installed, this grabs GMT polygons. This is touchy!}
\item{map}{Spatial polygons map object (i.e., data(ATL))}
\item{cilev}{variance level. Should be 95\%}

}

\value{
btrack object. see \code{\link{prepb}}
}

\details{
This currently works only in the Western hemisphere. A future version will be more robust.
}

\author{
Benjamin Galuardi
}
\references{
Johnson D (2008) crawl: Fit continuous-time correlated random walk models for animal movement data. Available: R package version 1.0-1.

Johnson, D. S, J. M London, M. A Lea, and J. W Durban. “Continuous-time Correlated Random Walk Model for Animal Telemetry Data.” Ecology 89, no. 5 (2008): 1208–1215.

Sippel, Tim, John Holdsworth, Todd Dennis, and John Montgomery. “Investigating Behaviour and Population Dynamics of Striped Marlin (Kajikia Audax) from the Southwest Pacific Ocean with Satellite Tags.” PLoS ONE 6, no. 6 (June 14, 2011): e21087.
}

\examples{

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Tagbase}