## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(capstoner)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

## ---- eval=FALSE--------------------------------------------------------------
#  file_name <- system.file("inst/extdata", package = "capstoner")
#  data <- readr::read_delim(file_name, quote = "", delim = "\t")

## ---- eval=FALSE--------------------------------------------------------------
#  data <- eq_clean_data(data)

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#
#  data %>%
#     capstoner::eq_clean_data() %>%
#     dplyr::filter(filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#     ) %>%
#     ggplot2::ggplot(aes(
#         x = date,
#         y = country,
#         size = eq_primary,
#         colour = log(total_deaths)
#     )) +
#     geom_timeline() +
#     geom_timeline_label(n_max = 3)

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#
#  data %>%
#     capstoner::eq_clean_data() %>%
#     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#     dplyr::mutate(popup_text = eq_create_label(.)) %>%
#     eq_map(annot_col = "popup_text")

