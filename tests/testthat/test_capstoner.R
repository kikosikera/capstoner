# Import packages------------------------------------------------------------------------------------
library(testthat)

# Firs import date to test---------------------------------------------------------------------------
file_name <- system.file("extdata/signif.txt", package = "capstoner")
data <- readr::read_delim(file_name, quote = "", delim = "\t")



# Tests Clean----------------------------------------------------------------------------------------

context("Test clean functions from the capstoner package")

test_that("eq_data_read returns a tbl_df", {
  expect_is(eq_clean_data(data), "data.frame")
})

test_that("eq_data_read returns the columns with the correct data type", {
  clean_data <- eq_clean_data(data)
  expect_is(clean_data$LATITUDE, "numeric")
  expect_is(clean_data$LATITUDE, "numeric")
  expect_is(clean_data$DATE, "Date")
})

test_that("eq_clean_location returns the columns location_name cleaned", {
  expect_equal(eq_clean_location(data)$LOCATION_NAME[1], "Bab-A-Daraa,Al-Karak")
})


# Test Geom-----------------------------------------------------------------------------------------

context("Test geom timeline functions from the capstoner package")

test_that("geom_timeline returns a ggplot object with their correct atributtes", {
  p <- data %>%
       eq_clean_data() %>%
       dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
         ggplot2::ggplot(aes(
           x = DATE,
           y = COUNTRY,
           size = EQ_PRIMARY,
           colour = log(TOTAL_DEATHS)
         )) +
         geom_timeline() +
         ggplot2::labs(size = "Richter scale value", color = "# deaths") +
         ggplot2::theme(legend.position='bottom')

  expect_is(p, "ggplot")
  expect_identical(p$labels$x, "DATE")
  expect_identical(p$labels$y, "COUNTRY")
})

test_that("geom_timeline_label returns a ggplot object with their correct atributtes", {
  p <- data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
      ggplot2::ggplot(aes(
        x = DATE,
        y = COUNTRY,
        size = EQ_PRIMARY,
        colour = log(TOTAL_DEATHS)
      )) +
      geom_timeline() +
      geom_timeline_label(aes(label=LOCATION_NAME), n_max = 4) +
      ggplot2::labs(size = "Richter scale value", color = "# deaths") +
      ggplot2::theme(legend.position='bottom')

  expect_is(p, "ggplot")
  expect_identical(p$labels$x, "DATE")
  expect_identical(p$labels$y, "COUNTRY")
})



# Test Map-----------------------------------------------------------------------------------------

context("Test mapping tools functions from the capstoner package")


test_that("eq_map returns a leaflet object", {
  p <- data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "popup_text")

  expect_is(p, "leaflet")
})

test_that("after use eq_create_label eq_map returns a ggplot object with their correct atributtes", {
  p <- data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")

  expect_is(p, "leaflet")
})
