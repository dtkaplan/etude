#' Show solutions in an etude document
#'
#' Calling `show_answers(TRUE)` causes the etude document
#' to display answers. Answers can be marked in either of two
#' ways:
#'
#' 1. a chunk with engine `etude`
#' 2. an inline chunk calling `ans(your_text)`
#'
#' @param flag logical. If `TRUE` (default) show the chunks
#' marked with the `etude` engine as specially formatted solutions
#' @param format Whether the document is intended for translation
#' to latex or to html. Determined automatically by default.
#' @export
show_answers <- function(
  flag=TRUE,
  format = ifelse(knitr::is_latex_output(), "latex", "html")) {
  if (flag) {
    strings <- get_format_strings(format)
    knitr::knit_engines$set(etude = function(options) {
      if (!"show_answers" %in% names(options)) {
        paste(strings[1],
              knitr:::one_string(options$code),
              strings[2])
      }
    })
  }
}


get_format_strings <- function(format) {
  if (format == "latex") {
    c("ANSWER: ",
      ".....................")
  } else {
    c('<div style="color:Crimson; background-color: SeaShell;">ANSWER:',
      "</div>")
  }
}

get_inline_format_strings <- function(format) {
  if (format == "latex") {
    c("\\bf{Ans: ", "}")
  } else {
    c(
      '<span style="color:Crimson; background-color: SeaShell;">Ans: ',
      '</span>')
  }
}

#' @export
ans <- function(x, format = knitr::is_latex_output() ) {
  strings <- get_inline_format_strings(format)
  if (!is.null(knitr::knit_engines$get("etude"))) {
    I(paste0(strings[1], as.character(substitute(x)), strings[2]))
  } else {
    "answer should be here."
  }
}

