#' Extract dataset information for a given dtype name or number
#'
#' @keywords internal
#' \code{getMaxTime} retrieves the latest time for the given dataset
#'  @param dataStruct A structure describing the dataset from erddapStruct.rda
#'  @param urlbase A character string giving the base URL of the ERDDAP server
#'  @return dataStruct with dataStruct$maxTime updated
#'

getMaxTime <- function(dataStruct, urlbase1='https://coastwatch.pfeg.noaa.gov/erddap/tabledap/allDatasets.csvp?') {
    myURL <- paste(urlbase1,'maxTime&datasetID="', dataStruct$datasetname, '"', sep = "")
    myURL <- utils::URLencode(myURL)
    #tmp <- utils::read.csv(myURL, skip=2, stringsAsFactors=FALSE, header=FALSE, quote = "")
    r1 <- httr::GET(myURL)
    tmp = suppressMessages(readr::read_csv(r1$content)[[1]])
    dataStruct$maxTime <- as.Date(tmp, origin = '1970-01-01', tz = "GMT")
  return(dataStruct)
}


