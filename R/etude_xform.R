#' Insert an exercise from a file
#'
#' Each exercise is contained in its own file, with a unique ID. This function
#' reads the file for that ID, knits the contents, and returns the knitted contents.
#' Typically this will be used to insert the exercise into an Rmd file.
#' If not specified, argument `show_answer` will be read from `options("show_answer")`.
#'
#' @param id the unique ID of the exercise file. Don't include the file type suffix ".Rmd" in the id.
#' @param verbose Include the YAML information
#' @param package character string naming the package from which the problem
#' is to be taken
#' @param keep_ref Logical flag indicating whether to keep the bookdown text reference (that is, `(ref:...)`). Default: FALSE
#' @param prob_name Character string naming the problem, e.g. `"Prob 1.3"`
#' @param show_answer if TRUE, include the answer comments from the file.
#'
#' @details
#' When output is in html, use the `answer-fragment` class in CSS to format
#' the answers.
#'
#' @examples
#' the_testing_dir <- system.file("Test_exercises", package = "SDSdata")
#' include_exercise("beech-run-mug", directory = the_testing_dir, format = "latex")
#'

#' @export
etude_xform <- function(id, show_answer = getOption("show_exercise", TRUE),
                         verbose = TRUE,
                         keep_ref = FALSE,
                         prob_name = "Problem XX",
                         package = "etude",
                         format = ifelse(knitr::is_latex_output(), "latex", "html")) {

  if (file.exists(id)) {
    fname <- id
  } else {
    if (package == "etude")
      warning("You are implicitly referring to an etude file from a package, but you haven't used the `package=` chunk option to say which package.")
    fname <- system.file(paste0("Exercises/", id, ".Rmd"), package = package)
  }
  content <- readLines(fname)

  yaml_stuff <- get_yaml_header(content)

  # replace the chunk labels with a (probably) unique label
  # in case a problem is included more than once in the document.
  if (is.null(yaml_stuff$id)) {
    chunk_id <- gsub(".Rmd$", "", id) #remove filetype extension if any
  } else {
    chunk_id <- yaml_stuff$id
  }
  new_chunk_id <- paste0(chunk_id, round(stats::runif(1, 100, 100000)))
  content <- gsub(chunk_id, new_chunk_id, content)


  content <- kill_yaml_header(content)
  if (! keep_ref) content <- kill_ref(content)
  if ( ! show_answer) { # delete the answer comments}
    content <- gsub("-A-.+$", "", content, perl = TRUE)
    content <- kill_answer_block(content)
  }
  # replace the answer markup with the appropriate latex/html constructs.
  inline_pattern <- "-A-([[:space:]]*)(.*)"
  answer_start_pattern <- "<\\!\\-\\-(answer\\-start|begin\\-answer)\\-\\->"
  answer_end_pattern <- "<\\!\\-\\-(answer\\-end|end\\-answer)\\-\\->"
  # Fill in the problem name.
  content <- gsub("(\\(ref:.*\\)) Exercise in file .*$",
                  paste("\\1", prob_name),
                  content, perl = TRUE)

  # prob_markup <- glue::glue("**{prob_name}** ")
  # using prob_name instead of prob_markup
  content <-
    paste(
      gsub("TITLE GOES HERE:?", prob_name, content),
      collapse = "\n")

  # content <- knitr::knit(text = content, rmarkdown::md_document())



  if (format == "latex") {
    # Can't use HTML markup because it will be deleted
    content <- gsub(inline_pattern,
                    "Ans: [\\2] ",
                    #"INLINE-ANSWER-START\\2INLINE-ANSWER-END",
                    content, perl = TRUE)
    content <- gsub(answer_start_pattern,
                    #"\\\\begin{quotation}\\\\em ",
                    "BLOCK-ANSWER-START",
                    content)
    content <- gsub(answer_end_pattern,
                    #"\\\\end{quotation}",
                    "BLOCK-ANSWER-END",
                    content)
  }  else if (format == "html") {
    content <- gsub("-A-([[:space:]]*)(.*)[$|\\n]",
                    "<span class = 'answer-fragment'> \\2 </span>\n",
                    content, perl = TRUE)
    content <- gsub(answer_start_pattern,
                    "<div class='answer-fragment'>",
                    content)

    content <- gsub(answer_end_pattern, "</div>", content)
  } else {
    stop("Unknown output format for problem file.")
  }



  # if (format == "html") {
  #   res <- knitr::knit(text = content, rmarkdown::md_document())
  #   writeLines(res, con = "testing.md")
  # } else if (format == "latex") {
  #   writeLines(content, con = "testing.Rmd")
  #   res <- knitr::knit("testing.Rmd", rmarkdown::latex_fragment(keep_tex = TRUE))
  # }

  if (verbose) {
    yaml_stuff <- gsub("\\[|\\]", "", yaml_stuff)
    keepers <- grep("id|topics|chapter|version|depends", yaml_stuff)
    yaml_addon <- paste0(paste0("* ", yaml_stuff[keepers]), collapse = "\n")

    content <- paste0(content, "\n\n", yaml_addon)
  }


  return(paste(content, "\n\n"))
}

kill_answer_block <- function(str) {
  starts <- grep("<\\!\\-\\- *(answer\\-start|begin\\-answer) *\\-\\->", str)
  if (length(starts) == 0) return(str)
  ends <- grep("<\\!\\-\\- *(answer\\-end|end\\-answer) *\\-\\->", str)
  if (length(starts) != length(ends) || any(starts >= ends))
    stop("Unmatched answer-block delimiter.")
  line_numbers <- integer(0)
  for (k in 1:length(starts)) {
    line_numbers <- c(line_numbers, starts[k]:ends[k])
  }
  str[-line_numbers]
}

get_yaml_header <- function(txt) {
  dashes <- which(grepl("^---$", txt))
  if (length(dashes) == 2 && dashes[1] == 1)
    return(yaml::yaml.load(txt[2:(dashes[2]-1)]))
  else if (length(dashes) == 0) return(NULL)
  else stop("Yaml must be at the top of the file, delimited by a pair of lines consisting only of '---' on line 1 and after the yaml content.")
}
kill_yaml_header <- function(txt) {
  dashes <- which(grepl("^---$", txt))
  if (length(dashes) == 2 && dashes[1] == 1)
    return(txt[(dashes[2]+1):length(txt)])
  else if (length(dashes) == 0) return(txt)
  else stop("Yaml must be at the top of the file, delimited by a pair of lines consisting only of '---' on line 1 and after the yaml content.")
}

kill_ref <- function(txt) {
  ref_lines <- grepl("^\\(ref:.*\\)",  txt)
  txt[ ! ref_lines]
}
