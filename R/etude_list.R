#' Include multiple etude exercises
#'
#' @param spec: A data frame with one row for
#' each exercise to be inclued in the parent document. The variables
#' must be named `exercise`, `answers`, `documentation`, `package`, and `name`.
#' (Any additional variables will simply be ignored by `etude_list`.)
#' @param keep_ref A logical flag, `FALSE` by default. If `TRUE` then lines
#' defining pandoc references will be retained in the output document. If
#' the parent document is being compiled using bookdown, this is what you want, since
#' the references will be replaced appropriately by pandoc. But if not using bookdown,
#' the references will appear verbatim in the output, which is probably not what
#' you want.
#'
#' @examples
#' \dontrun{
#' etude_list(
#'  tibble(exercise="dog-drink-ring",
#'       answers=FALSE, documentation=FALSE, name="Problem 1: "
#'       )
#' )
#' }
#' @export
etude_list <- function(spec, keep_ref = FALSE) {
  Res <- ""
  for (k in 1:nrow(spec)) {

    this <- spec[k, ]
    if (is.null(this$package) || is.na(this$package)) this$package <- ""
    ## ADD MORE OF THESE FOR THE OTHER SPECIFICATIONS

    Lines <-
      etude_xform(this$exercise, show_answer=this$answers,
                  verbose = this$documentation, prob_name = this$name,
                  package = this$package, keep_ref = FALSE)
    writeLines(Lines, con = "~/Downloads/foo.Rmd")
    contents <- knitr::knit_child("~/Downloads/foo.Rmd")

    Res <- paste(Res, "\n", contents)
  }
  return(I(Res))

}
