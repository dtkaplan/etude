#' Functions for use with gradethis exercise checking
#'
#' @param x input from `gradethis` checker, typically  `.result`
#'
#' @export
is_ggplot <- function(x) inherits(x, "ggplot")

#' @export
is_contour <- function(x) inherits(x$layers[[2]]$geom, "GeomContour")

#' @export
is_slice_plot <- function(x) inherits(x$layers[[1]]$geom, "GeomLine")

#' @export
y_plot_var <- function(x) x$labels[2]

#' @export
x_plot_var <- function(x) x$labels[1]
