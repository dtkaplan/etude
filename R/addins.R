#' Addin  to make a new etude exercise
#'
new_etude_learnr <- function(directory = "Exercises") {
  new_etude(directory = directory, learnr = TRUE)
}
new_etude <- function(directory = "Exercises",
                      learnr = FALSE) {
  # is there an Exercises directory?
  tmp <- list.dirs(path = directory)
  if (length(tmp) == 0)
    stop("No directory <", directory, "> in which to create the file.")

  while (TRUE) {
    doc_contents <-
      new_etude_template(save = FALSE,
                         learnr = learnr)
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

# Addin to insert a question
#' @export
insertQ <- function() {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents
  # figure out the document ID
  id <- "unknown_document_id"
  id_line_number <- which(grepl("^id:", contents))
  if (length(id_line_number) == 1) {
    id <- gsub(" +", "",
               gsub("^[id:| )]+(.*)$", "\\1", contents[id_line_number])
    )
  } else {
    stop("Multiple id: lines in YAML header or document.")
  }
  # Get the next question number
  existing_questions <- 0 # if none
  line_nums <- grep(paste0("^```\\{r +",id, "-Q[0-9]+,"), contents)
  nums <- regmatches(contents[line_nums],
                     gregexpr("\\-Q([0-9]+)",
                              contents[line_nums])
  )
  nums <- unlist(nums)
  nums <- as.numeric(gsub("[^0-9]", "", nums))
  new_num <- max(c(existing_questions, nums))+1
  new_question <- readLines(system.file("learnr-question-template.R", package = "etude"))
  new_question <- gsub("BLOCK_NAME",
                       paste0(id, "-Q", new_num),
                       new_question)
  rstudioapi::insertText(
    paste(new_question, collapse="\n"),
    id = this_doc$id)
}


