#' Shortcuts for fixed-choice questions
#'
#' @param prompt Character string prompt for the question
#' @param choices A character vector of the choices. Correct choices
#' should be indicated by putting `+` before and after the text
#' to be displayed to the student. (Remember that mixing numbers
#' and character strings in a vector coerces all entries to character
#' string, which is convenient here.)
#' @param inline Logical flag. If true, lay out the choices horizontally.
#' @param points Integer number of 'grade points' to associate
#' with the question. Optional.
#' @param allow_retry Logical flag to allow multiple submissions
#' @param random_answer_order Logical flag to shuffle the order
#' of choices when question is displayed.
#' @param is_learnr Is the document being compiled for `learnr`. This is
#' set automatically. Override it to `FALSE` if you want to see the
#' question in ordinary markdown format.
#'
#' @details
#' The functions `true_or_false()` and `choose_one()` are intermediaries
#' to the `learnr::question()` and `learnr::answer()` functions. They
#' provide a somewhat more concise way of describing the choices and appropriate
#' feedback and they allow questions to be rendered in an ordinary
#' Rmarkdown document without requiring the "shiny" elements of a learnr
#' document.
#'
#' When a `etude` file intended to be used with `learnr` is compiled on its
#' own, it is setup for the `learnr` runtime and questions will render
#' in learnr format. (Unless you manually override this with the `is_learnr = FALSE` option.)
#' But when you include an etude file as a child of a mother document, using
#' the `include_etude()` function typically, the questions will inherit
#' the settings of that document, which might be an ordinary Rmd document
#' directed to HTML or PDF or Word, etc.
#'
#' Whether answers are displayed in the non-learnr static renderings
#' depends on the argument `show_answers`. By default, this uses `etude`'s system
#' for turning on and off answers with the `show_answers(TRUE)` or `show_answers(FALSE)` command
#' earlier in the document.
#'
#' @rdname  true_or_false
#' @export
true_or_false <-
  function(prompt, right_answer,
           message_right = "You got that right!",
           message_wrong = "Sorry, you missed.",
           points = NA,
           allow_retry = TRUE,
           show_answers = are_answers_on(),
           is_learnr = "learnr" %in% loadedNamespaces()) {
    printfun <- if (is_learnr) question_for_learnr else question_for_markup

    choices <- list(
      ifelse(right_answer, message_right, message_wrong),
      ifelse(right_answer, message_wrong, message_right))

    if (right_answer) names(choices) = c("+True+", "False")
    else  names(choices) = c("True", "+False+")

    printfun(prompt = prompt,
             choices,
             inline = TRUE,
             points = points,
             random_answer_order = FALSE,
             show_answers = show_answers)
  }
#' @aliases choose_one
#' @rdname  true_or_false
#' @export
choose_one <- function(prompt = "First consonant?",
                       choices = list(a = "No, it's not", "+b+" = "Right as rain"),
                       inline= TRUE,
                       points = NA,
                       show_answers = are_answers_on(),
                       ...,
                       random_answer_order = TRUE,
                       allow_retry = TRUE,
                       is_learnr = "learnr" %in% loadedNamespaces()) {

  if ((is.character(choices) || is.numeric(choices)) &&
      !is.null(names(choices))) {
    message("Please give the <choices> as a list() if you are assigning messages to the choices.")
    choices <- as.list(choices)
  }

  if (n_correct(choices) == 0) stop("Must provide at least one correct choice.")

  printfun <- if (is_learnr) question_for_learnr else question_for_markup

  printfun(prompt = prompt,
           choices = choices,
           inline= inline,
           points = points,
           show_answers = show_answers,
           random_answer_order = random_answer_order,
           allow_retry = allow_retry)
}
#'
#'

question_for_markup <- function(prompt = "What?",
                          choices = list(a = "No", "+b+" = "Right"),
                          inline= TRUE,
                          points = NA,
                          show_answers = TRUE,
                          chunk_name = knitr::opts_current$get()$label,
                          ...) {
  texts <- names(choices)
  messages <- unlist(choices)
  correct_ones <- which_are_correct(texts)
  texts <- clean_names(texts)

  res <-
    sprintf("%s <span title = '%s'>...</span>",
            prompt, chunk_name)

  if (!is.na(points)) res <- paste0(res, "(", points, "points)")
  res <- paste(res, "     \n")
  if (show_answers) {
    prelims <- ifelse(correct_ones, "✔", "✗")
    messages <- paste0("...*", messages, "*")
  } else {
    prelims <- "⚀"
    messages <- ""
  }

  if (inline) {
    answers <- paste(paste(prelims, texts, messages), collapse = "...")
  } else {
    answers <- paste("-", prelims, texts, messages, "     ")
  }

  # Final result
  ret <- paste(c(res, answers), collapse = "\n")

  knitr::asis_output(ret)
}

question_for_learnr <- function(prompt = "What?",
                                choices = list(a = "No, it's not", "+b+" = "Right as rain"),
                                inline= TRUE,
                                points = NA,
                                show_answers = TRUE,
                                random_answer_order = TRUE,
                                allow_retry = TRUE,
                                chunk_name = knitr::opts_current$get()$label
) {


  if (is.character(choices)) {
    # Convert character vector to a list with empty messages.
    tmp <- as.list(rep("", length(choices)))
    names(tmp) <- as.character(choices)
    choices <- tmp
  } else if (is.numeric(choices)) {
    stop("choices argument must be a named list or a character vector.")
  }

  if (is.null(names(choices))) {
    # add labels A B C, ... and all are correct
    warning("choices argument must be a **named** list.")
    names(choices) <- paste0("+", LETTERS[1:length(choices)], "+")
  }

  correct_ones <- which_are_correct(choices)
  names(choices) <- clean_names(choices)

  answers <- list()
  for (k in 1:length(choices)) {
    answers[[k]] <- learnr::answer(text = names(choices[k]), message = choices[[k]],
                   correct = correct_ones[k])
  }

  if (!is.na(points)) prompt <- paste(prompt, " ...",points, "points")

  qtype <- if (sum(correct_ones) > 1) {
    if (inline) "learnr_checkbox_inline"
    else "learnr_checkbox"
  } else {
    if (inline) "learnr_radio_inline"
    else "learnr_radio"
  }

  arguments <- list(
    text <-
      sprintf("%s <span title = '%s'>...</span>",
              prompt, chunk_name),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    type = qtype
  )

  do.call(learnr::question, #etude::etude_question, #
          c(arguments, answers))
}

n_correct <- function(choices) {
  sum(which_are_correct(choices))
}

which_are_correct <- function(choices, yes_delim = "\\+") {
  if (is.list(choices)) choices <- names(choices)

  grepl(glue::glue("^{yes_delim}.*{yes_delim}$"),
            choices)
}

clean_names <- function(choices, yes_delim = "\\+") {
  if (is.list(choices)) choices <- names(choices)

  gsub(glue::glue("^{yes_delim}|{yes_delim}$"), "", choices)
}


