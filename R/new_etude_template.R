#' Make a new etude exercise
#'
#' Creates the boilerplate for an etude exercise.
#' Typically, this will be called by the RStudio addin, but
#' you can also run it directly when `save=TRUE`
#'
#' @param save Logical indicating whether to save the template to a new file
#' and return the name of that file
#' @param exercise_id Root name for the exercise file (.Rmd will be added).
#' By default, a random, unique root name will be created using the `etude` naming
#' convention, e.g., bear-walk-dish
#' @param learnr Logical flag (default: FALSE) whether to use
#' a learnr compatible document for the exercise.
#'
#' @export
new_etude_template <- function(save = TRUE,
                               exercise_id = NULL, learnr = FALSE) {
  if (is.null(exercise_id))  exercise_id <- make_random_id()
  count = 0
  while (count < 10 && save && file.exists(paste0(exercise_id, ".Rmd"))) {
    exercise_id <- make_random_id()
    count = count + 1
  }
  if (count >= 10) stop("Can't find a random name for the new etude. Use exercise_id argument to make one of your own.")

  template_file <-
    if (learnr) "learnr_doc_template.Rmd"
    else "markdown_template.Rmd"
  contents <- readLines(system.file(template_file,
                                    package = "etude"))
  contents <- gsub("date:", paste("date:", Sys.Date()), contents)
  contents <- gsub("XXAXX", exercise_id, contents)
  contents <- gsub("XXAUTHORXX", whoami::fullname(), contents)
  res <- paste(contents,  collapse = "\n")
  filename <- paste0(exercise_id, ".Rmd")
  attributes(res) <- list(file_name = filename)


  if (save) {
    writeLines(res,  con = filename)
    message("Created  new  etude file named ", filename)
    return(filename)
  } else {
    return(res)
  }
}
