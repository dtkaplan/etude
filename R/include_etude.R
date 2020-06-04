#' Inserting exercises into a document.
#'
#' Inserts an etude into a mother document
#'
#' @param fname The path name for the etude `.Rmd` file.
#' @param title Character string containing the immediate header
#' for the etude, e.g., `"Problem 17.3:"` or `"### Exer A\n\n"`
#'
#' @export
include_etude <- function(fname, title) {
  if (missing(title)) {
    warning("Using etude file name as title. See title= argument to include_etude()")
    title = paste("### Source file:",fname,"\n\n")
  }
  assign(".the_title.", title, envir = etude:::title.env)

  # return the knitted document
  knitr::knit_child(fname, envir = etude:::title.env)
}
#' @export
exercise_title <- function() {
  if (".the_title." %in% names(title.env))
    etude:::title.env$.the_title.
  else "TITLE WILL GO HERE:"
}


title.env <- new.env()
