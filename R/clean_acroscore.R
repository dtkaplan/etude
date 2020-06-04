#' Translate an Acroscore-style question to an etude question
#'
#' This is needed only transiently, until I've worked through all
#' the old-style questions
#' @export
do_clean_acroscore <- function(text) {

  text <- unlist(strsplit(text, "\n"))

  # remove empty lines
  text <- text[!grepl("^\\s{0,100}$", text)]

  lines <- gsub("^[^ ]{1,10}\\s", "", text)
  # remove the bulleting, if any
  lines <- gsub("^\\*|\\*$", "+", lines)
  # lines <- clean_names(lines)
  # quote each name
  lines <- paste0('"', lines, '"')

  blanks <- paste(paste("  ", lines, "=", rep("''", length(lines))), collapse=",\n")

  paste(" ", glue::glue("list(\n{blanks}\n  )"))
}



