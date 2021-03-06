% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getInfo.R
\name{getInfo}
\alias{getInfo}
\title{Extract dataset information for a given dtype name or number}
\usage{
getInfo(dtype)
}
\arguments{
\item{dtype}{- character string or integer (1-138) for given dataset}
}
\value{
prints out dataset information
}
\description{
\code{getInfo} displays the dataset information for a given
 dtype or dtypename
}
\section{Details}{

getInfo gives the dataset information for the given dataset. This includes:
dtypename,datasetname,longname,varname.  It will return the following
information about any dataset that matches the string:
\itemize{
\item dtypename
\item datasetname
\item longname
\item varname
\item hasAlt
\item latSouth
\item lon360
\item minLongitude
\item maxLongitude
\item longitudeSpacing
\item minLatitude
\item maxLatitude
\item latitudeSpacing
\item minAltitude
\item maxAltitude
\item minTime
\item maxTime
\item timeSpacing
\item infoUrl
 }
}

\examples{
getInfo('atsstamday')
}
