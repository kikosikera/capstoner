#' Clean the NOAA dataset
#'
#' This function is in charge of creating a new column, \code{DATE} based on the \code{YEAR},
#' \code{MONTH} and \code{DAY} columns.
#' It also converts the \code{LATITUDE} and \code{LONGITUDE} columns to a numeric class.
#' Finally clean the \code{LOCATION_NAME} column to be able to plot later
#' with the relevant annotations.
#'
#' @param raw_data Data frame from \emph{U.S. National Oceanographic and Atmospheric
#' Administration \strong{(NOAA)}}.
#' Below the download link \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @return A \emph{clean dataframe} without nans and clean columns to plot in the next steps.
#' If the dataframe is not the one corresponding to NOAA, it will return an error
#' when not finding the columns corresponding to it.
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_
#' @importFrom dplyr filter_
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim('path_to_data', quote = "", delim = "\t")
#' data <- eq_clean_data(data)
#' }
#'
eq_clean_data <- function(raw_data){
  eq_clean_location(raw_data) %>%
    # Create new column data and parse lat and long columns.
    dplyr::mutate_(DATE = ~lubridate::ymd(paste(YEAR, MONTH, DAY)),
                   LATITUDE = ~as.numeric(LATITUDE),
                   LONGITUDE = ~as.numeric(LONGITUDE))# %>%

    # Clean nan of the columns cleaned.
    dplyr::filter_(~!is.na(DATE)) %>%
    dplyr::filter_(~!is.na(LATITUDE)) %>%
    dplyr::filter_(~!is.na(LONGITUDE)) %>%
    dplyr::filter_(~!is.na(LOCATION_NAME)) %>%
    dplyr::filter_(~LOCATION_NAME != "")
}


#' Helper for \code{eq_clean_data} function. Clean the column \code{LOCATION_NAME}.
#'
#' This function covers the \code{LOCATION_NAME} column, extracts the country and
#' leaves the corresponding \code{LOCATION} visible.
#'
#' @param raw_data Data frame from \emph{U.S. National Oceanographic and Atmospheric
#' Administration \strong{(NOAA)}}.
#' Below the download link \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @return A \emph{clean dataframe} with the \code{LOCATION_NAME} column clean.
#' If the dataframe is not the one corresponding to NOAA, it will return an error
#' when not finding the columns corresponding to it.
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#' data <- eq_clean_location(data)
#' }
#'
eq_clean_location <- function(raw_data){
  raw_data %>%
    dplyr::mutate_(LOCATION_NAME = ~stringr::str_replace(LOCATION_NAME, pattern = ".*:", replacement = ""),
                   LOCATION_NAME = ~stringr::str_trim(LOCATION_NAME),
                   LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME))
}
