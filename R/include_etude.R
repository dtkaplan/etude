#' Inserting exercises into a document.
#'
#' Inserts an etude into a mother document
#'
#' @param fname The path name for the etude file. Include the filetype extension (e.g. `.Rmd`).
#' @param title Character string containing the immediate header
#' for the etude, e.g., `"Problem 17.3:"` or `"### Exer A\n\n"`
#' @param package The name of the package (if etude file is provided by a package).
#' This avoids having to call `system.file()`
#'
#' @export
include_etude <- function(fname, title, package = NULL) {
  tooltip <- glue::glue(
    "<span title='{fname} in package {ifelse(!is.null(package), package, '')}'>...</span>")
  if (!is.null(package)) {
    fname <- system.file(fname, package=package)
  }

  if (missing(title)) {
    warning("Using etude file name as title. See title= argument to include_etude()")
    title = paste("### Source file:",fname,"\n\n")
  }
  assign(".the_title.",
         paste(title, tooltip),
         envir = etude:::title.env)

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
