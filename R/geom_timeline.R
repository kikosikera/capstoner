#' Geom for plotting a time line of earthquakes
#'
#' This function return a \strong{ggplot layer} that representing
#' representing a time line of earthquakes.
#' The size of the points is relatative to the earthquakes's magnitude, and
#' the color is related to the total number of deaths.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @details Aesthetics:
#' \code{geom_timeline} understands the following aesthetics
#'   (required in bold):
#' \itemize{
#'   \item \strong{x}: (Date) of earthquakes
#'   \item y: (factr) stratification. If present multiple time lines
#'            will be plotted for each level of the factor
#'            (e.g. country).
#'   \item colour: of the points
#'   \item size: of the points
#'   \item alpha: trasparency for the points
#'   \item fill
#'   \item linetype
#'   \item linesize
#'   \item fontsize
#'   \item stroke
#' }
#'
#'
#' @export
#'
#' @importFrom ggplot2 layer
#'
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data <- readr::read_delim('path_to_data', quote = "", delim = "\t")
#'
#' data %>%
#'    capstoner::eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    ggplot2::ggplot(aes(
#'       x = DATE
#'       y = COUNTRY,
#'       size = EQ_PRIMARY,
#'       colour = log(TOTAL_DEATHS)
#'       )) +
#'    geom_timeline() +
#'    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
#     ggplot2::theme(legend.position='bottom')
#' }
geom_timeline <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show.legend = NA, inherit.aes = TRUE,
  ..., na.rm = FALSE
) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data,
    stat = stat, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}


#' Creates a theme for timeline visualization.
#'
#' This function make a simple theme that makes \code{\link{geom_timeline}}
#' visualize better.
#'
#' @export
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar polylineGrob gList
#' @importFrom scales alpha
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,

   required_aes = c("x"),

   default_aes = ggplot2::aes(

     y = 0.25,
     colour = "grey",
     size = 1,
     alpha = 0.25,
     shape = 19,
     fill = "black",
     linesize = 0.5,
     linetype = 1 ,
     fontsize = 10,
     stroke = 1
   ),

   draw_key = ggplot2::draw_key_point,

   draw_panel = function(data, panel_scales, coord) {
     coords <- data %>%
       coord$transform(panel_scales)

     if (length(unique(coords$y)) == 1) {
       coords$y <- 0.25
     }

     points <- grid::pointsGrob(
       x = coords$x,
       y = coords$y,
       size = grid::unit(coords$size / 5, "char"),
       pch = coords$shape,
       gp = grid::gpar(
                col = coords$colour %>%
                scales::alpha(coords$alpha),
                fill = coords$fill %>%
                scales::alpha(coords$alpha),
         fontsize = grid::unit(coords$fontsize, "points")
       )
     )

     y_lines <- unique(coords$y)

     lines <- grid::polylineGrob(
       x = grid::unit(
         rep(c(0, 1), each = length(y_lines)),
         "npc"
       ),
       y = grid::unit(c(y_lines, y_lines), "npc"),
       id = rep(seq_along(y_lines), 2),
       gp = grid::gpar(
         col = "grey",
         lwd = grid::unit(coords$linesize[1], "mm")
       )
     )

     grid::gList(points, lines)
   }
)



#' Labels for plotting a time line of earthquakes
#'
#' This functions add lebels more descriptive to the time line plot of earthquakes.
#'
#' Return a \code{\link[ggplot2]{layer}} representing earthquakes annotations to
#' be added after \code{\link{geom_timeline}}.
#'
#' @param n_max Max number of lebels to add in the plot.
#'
#' @details Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics
#'   (required in bold):
#' \itemize{
#'   \item \strong{x}: (Date) of earthquakes.
#'   \item \strong{label}: (chr) text annotation.
#'   \item y: (factr) stratification. If present multiple time lines
#'            will be plotted for each level of the factor
#'            (e.g. country).
#'   \item colour: of the points.
#'   \item n_max: annotation to drowm.
#'   \item size: of the points (if provided with \code{n_max} too,
#'         the \code{n_max} largest eartquakes will be annotated).
#' }
#'
#' @inheritParams ggplot2::geom_text
#'
#' @export
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data <- readr::read_delim('path_to_data', quote = "", delim = "\t")
#'
#' data %>%
#'    capstoner::eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    ggplot2::ggplot(aes(
#'       x = DATE,
#'       y = COUNTRY,
#'       size = EQ_PRIMARY,
#'       colour = log(TOTAL_DEATH),
#'       label = LOCATION_NAME
#'    )) +
#'    geom_timeline() +
#'    geom_timeline_label(aes(label=LOCATION_NAME), n_max = 4) +
#     ggplot2::labs(size = "Richter scale value", color = "# deaths") +
#     ggplot2::theme(legend.position='bottom')
#' }
#'
geom_timeline_label <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show.legend = NA, inherit.aes = TRUE,
  n_max = NULL, ..., na.rm = FALSE
) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n_max = n_max, na.rm = na.rm, ...)
  )
}

#' GeomTimelineLabel
#'
#' See \code{\link{geom_timeline_label}} for description.
#'
#' @export
#'
#' @format NULL
#'
#' @usage NULL
#'
#' @importFrom ggplot2 ggproto Geom draw_key_blank
#' @importFrom dplyr group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
#'
#' @importFrom ggplot2 ggproto Geom draw_key_blank
#' @importFrom dplyr group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
#'
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel", ggplot2::Geom,

  required_aes = c("x", "label"),


  default_aes  = ggplot2::aes(
    y = 0.25,
    colour = "black",
    size = 1,
    alpha = 0.25,
    shape = 19,
    linesize = 0.5,
    linetype = 1,
    fontsize = 10,
    stroke = 1,
    angle = 60
  ),

  draw_key = ggplot2::draw_key_blank,

  setup_data = function(data, params) {


    if (!is.null(params$n_max)) {
      message(paste(params$n_max, "annotation will be drown."))
      data <- data %>%
        dplyr::group_by_("y") %>%
        dplyr::mutate(size_rank = dplyr::row_number(size)) %>%
        dplyr::top_n(params$n_max, size_rank) %>%
        dplyr::ungroup() %>%
        dplyr::select(-size_rank)
    }

    data
  },

  draw_panel = function(data, panel_scales, coord, n_max) {

    coords <- coord$transform(data, panel_scales)

    if (length(unique(coords$y)) == 1) {
      coords$y <-  0.25
    }

    if (!("size" %in% names(coords))) {
      coords$size <- 0.25
    }

    n_grp  <- length(unique(data$y))
    offset <- 0.2 / n_grp

    lines <- grid::polylineGrob(
      x = grid::unit(c(coords$x, coords$x), "npc"),
      y = grid::unit(c(coords$y, coords$y + offset), "npc"),
      id = rep(seq_len(nrow(coords)), 2),
      gp = grid::gpar(
        col = coords$colour,
        lwd = grid::unit(coords$linesize, "mm"),
        lty = coords$linetype
      )
    )

    names <- grid::textGrob(
      x = grid::unit(coords$x, "npc"),
      y = grid::unit(coords$y + offset, "npc"),
      label = coords$label,
      just = c("left", "bottom"),
      rot = 60,
      gp = grid::gpar(
        col = coords$colour,
        fontsize = grid::unit(coords$fontsize, "points")
      ),
      check.overlap = FALSE
    )

    grid::gList(lines, names)
  }
)
