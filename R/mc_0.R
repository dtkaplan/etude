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
#' @param yes_delim Delimiter for a correct answer. Default `"+"` on either side
#' of answer, but if there is a plus sign in in the answer, use `"Y"`.
#' @param is_learnr Is the document being compiled for `learnr`. This is
#' set automatically. Override it to `FALSE` if you want to see the
#' question in ordinary markdown format.
#'
#' @export
true_or_false <-
  function(prompt, right_answer,
           message_right = "You got that right!",
           message_wrong = "Sorry, you missed.",
           points = NA,
           allow_retry = TRUE,
           show_answers = are_answers_on(),
           is_learnr = "learnr" %in% loadedNamespaces()) {
    printfun <- if (is_learnr) question_for_learnr else question_for_html

    choices <- list(
      ifelse(right_answer, message_right, message_wrong),
      ifelse(right_answer, message_wrong, message_right))

    if (right_answer) names(choices) = c("+Right+", "Wrong")
    else  names(choices) = c("Right", "+Wrong+")

    printfun(prompt = prompt,
             choices,
             inline = TRUE,
             points = points,
             random_answer_order = FALSE,
             show_answers = show_answers)
  }

#' @export
choose_one <- function(prompt = "First consonant?",
                       choices = list(a = "No, it's not", "+b+" = "Right as rain"),
                       inline= TRUE,
                       points = NA,
                       show_answers = are_answers_on(),
                       yes_delim = c("\\+", "Y"), ...,
                       random_answer_order = TRUE,
                       allow_retry = TRUE,
                       is_learnr = "learnr" %in% loadedNamespaces()) {
  printfun <- if (is_learnr) question_for_learnr else question_for_html

  printfun(prompt = prompt,
           choices = choices,
           inline= inline,
           points = points,
           show_answers = show_answers,
           yes_delim = yes_delim,
           random_answer_order = random_answer_order,
           allow_retry = allow_retry)
}
#'
#'
#' @export
question_for_html <- function(prompt = "What?",
                          choices = list(a = "No", "+b+" = "Right"),
                          inline= TRUE,
                          points = NA,
                          show_answers = TRUE,
                          yes_delim = c("\\+", "Y"), ...) {
  yes_delim <- match.arg(yes_delim)
  texts <- names(choices)
  messages <- unlist(choices)
  correct_ones <-
    grepl(glue::glue("^{yes_delim}.*{yes_delim}$"), texts)
  texts <- gsub(glue::glue("^{yes_delim}|{yes_delim}$"), "", texts)

  res <- paste0("**", prompt, "**     ")
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
    answers <- paste(paste(prelims, texts, messages), collapse = "   ")
  } else {
    answers <- paste("-", prelims, texts, messages, "     ")
  }

  # Final result
  ret <- paste(c(res, answers), collapse = "\n")

  knitr::asis_output(ret)
}

#' @export
question_for_learnr <- function(prompt = "What?",
                                choices = list(a = "No, it's not", "+b+" = "Right as rain"),
                                inline= TRUE,
                                points = NA,
                                show_answers = TRUE,
                                yes_delim = c("\\+", "Y"), ...,
                                random_answer_order = TRUE,
                                allow_retry = TRUE) {
  yes_delim <- match.arg(yes_delim)
  correct_ones <-
    grepl(glue::glue("^{yes_delim}.*{yes_delim}$"), names(choices))
  names(choices) <- gsub(glue::glue("^{yes_delim}|{yes_delim}$"), "", names(choices))

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
    text <- prompt,
    random_answer_order = TRUE,
    allow_retry = TRUE,
    type = qtype
  )

  do.call(learnr::question,
          c(arguments, answers))
}
