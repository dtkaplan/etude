#' Addin  to make a new etude exercise
#'
#' @export
#' @rdname new_etude_template
#' @param directory Path to the directory where the files go
new_etude_learnr <- function(directory = ".") {
  new_etude(directory = directory, learnr = TRUE)
}
#' @export
#' @rdname new_etude_template
new_etude <- function(directory = ".",
                      learnr = FALSE) {
  # Does the directory exist
  if (!(grepl("/$", directory) || directory == "." || directory == ".."))
    stop("Directory name must terminate in a forward slash /.")
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
insertQ <- function(type = "-Q") {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents
  # figure out the document ID
  id <- get_doc_ID(contents)
  cat("Doc ID is", id, "\n")
  # Get the next question number
  chunk_id <- new_chunk_id(contents, id, type)
  cat("Chunk ID is", chunk_id, "\n")
  template_file <-
    system.file(glue::glue("template{type}.Rmd"),
                package = "etude")
  new_chunk <- readLines(template_file)
  new_chunk <- gsub("BLOCK_NAME",
                       chunk_id,
                       new_chunk, fixed = TRUE)
  rstudioapi::insertText(
    paste(new_chunk, collapse="\n"),
    id = this_doc$id)
}

#' Get the id of the document
get_doc_ID <- function(contents) {
  id <- paste0("document_", as.hexmode(floor(stats::runif(1, 1, 16^7))))
  id_line_number <- which(grepl("^id:", contents))
  if (length(id_line_number) > 0) {
    id <- gsub(" +", "",
               gsub("^(id:| )+(.*)$", "\\2", contents[id_line_number[1]])
    )
  }
  id
}

clean_acroscore <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  where <- rstudioapi::primary_selection(context)
  rstudioapi::insertText(where$range,
                         do_clean_acroscore(where$text),
                         context$id)
}

new_chunk_id <- function(contents, doc_id, type = "-Q") {

  line_nums <-
    grep(
      paste0("^```\\{r +",doc_id,
             glue::glue("{type}[0-9]+[, \\}]")),
      contents)
  if (length(line_nums) > 0) {
  nums <- regmatches(contents[line_nums],
                     gregexpr(glue::glue("\\{type}([0-9]+)"),
                              contents[line_nums]))
  nums <- unlist(nums)
  nums <- as.numeric(gsub("[^0-9]", "", nums))
  new_num <- max(nums)+1
  } else {
    new_num <- 1
  }

  # form the new chunk ID and return
  paste0(doc_id, type, new_num)
}
#' @export
etudeE <- function() etude::insertQ("-E")
#' @export
etudeS <- function() etude::insertQ("-sandbox")
#' @export
etudeQ <- function() etude::insertQ("-Q")
#' @export
etudeC <- function() etude::insertQ("-C") # Chunk
#' @export
etudeQA <- function() etude::insertQ("-QA")
#' @export
etudeTF <- function() etude::insertQ("-TF")
#' @export
etudeEssay <- function() etude::insertQ("-Essay")
#' @export
etudeQinline <- function() etude::insertQ("-Qinline")



