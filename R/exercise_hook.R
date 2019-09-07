#' Exercise interface to knitr
#'
#' A hook to include exercises in a document
#'
#' In the startup to the document, set
#' `knitr::knit_hooks$set(exercise = exercise_hook)`
#'
#' You don't call the hook directly. Instead, it is called by knitr
#' whenever the chunk option `exercise = <file_hash>` is given.
#' You should communicate using the chunk options, not the ar
#' The chunk relevant options are:
#' - `exercise=`: hash for exercise file
#' - `name=`: the identifier for the exercise to print
#' - `answers=`: TRUE or FALSE: show the answers?
#' - `package=`: the package the exercise comes from.
#'
#'
#' @param before As with all knitr hooks, indicates whether we are before or after the chunk
#' is evaluated.
#' @param options As with all knitr hooks, the options given to the chunk
#' @param envir As with all knitr hooks, the environment in which the chunk will
#' be evaluated.
#'
#' @examples
#' \dontrun{
#' # Chunk options should be like this
#' exercise = "dog-flies-home", name="Exercise 36", answers = FALSE,
#' }
#' @export
exercise_hook = function(before, options, envir) { # boilerplate for knitr
  prob_name <- "**Exercise ....**"
  cat("In exercise hook...\n")
  if (! is.null(options$name))
    prob_name <- options$name

  if (is.null(options$answers))
    options$answers <- FALSE


  if (is.null(options$documentation))
    options$documentation = FALSE


  if (is.null(options$package))
    options$package <- "etude"

  if (before) {
    ## code to be run before a chunk
    cat(paste("Processing exercise file",
              options$exercise,
              ifelse(options$answers, "and", "but not"),
              "showing the answers.\n"))
    Lines <-
      etude_xform(id = options$exercise, show_answer=options$answers,
                   verbose = options$documentation, prob_name = prob_name)
    writeLines(Lines, con = "~/Downloads/foo.Rmd")
    contents <- suppressMessages(
      knitr::knit_child("~/Downloads/foo.Rmd"))
    contents <- paste(options$before, contents, options$after)
    return(I(contents))

  } else {
    ## code to be run after a chunk
  }
}

