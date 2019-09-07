#' Addin  to make a new etude exercise
#'
new_etude <- function(directory = "Exercises") {
  # is there an Exercises directory?
  tmp <- list.dirs(path = directory)
  if (length(tmp) == 0)
    stop("No directory <", directory, "> in which to create the file.")

  while (TRUE) {
    doc_contents <- new_etude_template(save = FALSE)
    # will be saved later from editor
    new_file_name <- paste(directory, attr(doc_contents, "file_name"), sep = "/")
    tmp <- list.files(path = new_file_name)
    if (length(tmp) == 0) { # clear to create the file
      writeLines(doc_contents, con = new_file_name)
      if (!rstudioapi::isAvailable())
        return()
      if (!rstudioapi::hasFun("navigateToFile"))
        return()
      rstudioapi::navigateToFile(new_file_name)
      break;
    }

  }
}


