#' Show solutions in an etude document
#'
#' Calling `show_answers(TRUE)` causes the etude document
#' to display answers. Answers can be marked in either of two
#' ways:
#'
#' 1. a chunk with engine `etude`
#' 2. an inline chunk calling `ans(your_text)`
#'
#' When `show_answers(TRUE)` has been called, the multiple choice questions
#' created by `true_and_false()` or `choose_one()` will, **if they are
#' being rendered as static HTML, will show the answers along with each multiple-choice
#' item. Note that if the document is compiled to `learnr`, those static
#' answers won't be shown. Instead, it's the user's dynamic interaction
#' with the `learnr` document that sets whether the user sees answers.
#'
#' @param flag logical. If `TRUE` (default) show the chunks
#' marked with the `etude` engine as specially formatted solutions
#' @param format Whether the document is intended for translation
#' to latex or to html. Determined automatically by default.
#' @export
show_answers <- function(
  flag=TRUE,
  format = ifelse(knitr::is_latex_output(), "latex", "html")) {

  assign("answers_on", flag, show_answers_env)
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

# System for determining globally whether the display of
# answers is on.
show_answers_env <- new.env()
show_answers_env$answers_on <- FALSE
are_answers_on <- function() show_answers_env$answers_on

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

