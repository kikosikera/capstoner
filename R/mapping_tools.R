#' Plot a leaflet map of earthquakes
#'
#' This function at takes an argument data containing the filtered data frame with
#' earthquakes to visualize and creates a \code{leaflet} map of selected earthquakes.
#'
#' @param data A filtered data frame containing cleaned NOAA earthquake data.
#' @param annot_col A character. The name of the column in the data that should
#' be used as descriptor.
#'
#' @return A leaflet map with earthquakes and annotations. It will return an error
#' when not finding the right data frame or the \code{annot_col} is not of the
#' indicated type.
#'
#' @export
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data <- readr::read_delim('path_to_data', quote = "", delim = "\t")
#'
#' data %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'    eq_map(annot_col = "popup_text")
#' }
#'
eq_map <- function(data, annot_col) {
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                              radius = data$EQ_PRIMARY, weight = 1,
                              popup = data[[annot_col]])
  return(map)
}


#' Generates a label for a leaflet map
#'
#' This function generates a label for the \code{leaflet} map based on location
#' name, magnitude and casualties from NOAA earthquake data.
#'
#' @param data A filtered data frame containing cleaned NOAA earthquake data.
#'
#' @return A character vector with labels.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \dontrun{
#' library(dplyr)
#'
#' data <- readr::read_delim('path_to_data', quote = "", delim = "\t")
#'
#' data %>%
#'    capstoner::eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'    eq_map(annot_col = "popup_text")
#' }
#'
eq_create_label <- function(data) {
  popup_text <- with(data, {
    part1 <- ifelse(is.na(LOCATION_NAME), "",
                    paste("<strong>Location:</strong>",
                          LOCATION_NAME))
    part2 <- ifelse(is.na(EQ_PRIMARY), "",
                    paste("<br><strong>Magnitude</strong>",
                          EQ_PRIMARY))
    part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                    paste("<br><strong>Total deaths:</strong>",
                          TOTAL_DEATHS))
    paste0(part1, part2, part3)
  })
}
