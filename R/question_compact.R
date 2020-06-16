#' Custom version of `learnr::question()` for compact exercises.
#'
#' @import shiny
#' @export
etude_question <- function(text,
                     ...,
                     type = c("auto", "single", "multiple", "learnr_radio", "learnr_checkbox", "learnr_text", "learnr_radio_inline",  "learnr_checkbox_inline"),
                     correct = "Correct!",
                     incorrect = "Incorrect",
                     try_again = incorrect,
                     message = NULL,
                     post_message = NULL,
                     loading = c("**Loading:** ", text, "<br/><br/><br/>"),
                     submit_button = "Submit Answer",
                     try_again_button = "Try Again",
                     allow_retry = FALSE,
                     random_answer_order = FALSE,
                     options = list()
) {


  # one time tutor initialization
  initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  lapply(answers, function(answer) {
    checkmate::assert_class(answer, "tutorial_question_answer")
  })

  # verify chunk label if necessary
  learnr:::verify_tutorial_chunk_label()

  total_correct <- sum(vapply(answers, function(ans) { ans$correct }, logical(1)))
  if (total_correct == 0) {
    stop("At least one correct answer must be supplied")
  }

  ## no partial matching for s3 methods
  if (missing(type)) { # can not use match.arg(type) because of comment above
    type <- "auto"
  }
  if (isTRUE(all.equal(type, "auto"))) {
    if (total_correct > 1) {
      type <- "learnr_checkbox"
    } else {
      type <- "learnr_radio"
    }
  }
  if (length(type) == 1) {
    type <- switch(type,
                   "radio" = ,
                   "single" = "learnr_radio",
                   "checkbox" = ,
                   "multiple" = "learnr_checkbox",
                   # allows for s3 methods
                   type
    )
  }

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- label %||% learnr:::random_question_id()

  ret <- list(
    type = type,
    label = label,
    question = etude_quiz_text(text, label),
    answers = answers,
    button_labels = list(
      submit = etude_quiz_text(submit_button),
      try_again = etude_quiz_text(try_again_button)
    ),
    messages = list(
      correct = etude_quiz_text(correct),
      try_again = etude_quiz_text(try_again),
      incorrect = etude_quiz_text(incorrect),
      message = etude_quiz_text(message),
      post_message = etude_quiz_text(post_message)
    ),
    ids = list(
      answer = NS(q_id)("answer"),
      question = q_id
    ),
    loading = etude_quiz_text(loading),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    # Set a seed for local testing, even though it is overwritten for each shiny session
    seed = learnr:::random_seed(),
    options = options
  )
  class(ret) <- c(type, "tutorial_question")
  ret
}



# etude version of learnr's `quiz_text()`
# puts the submit and feedback info inline with the prompt.
etude_quiz_text <- function(text, label = "blank_label") {
  if (inherits(text, "html")) {
    return(text)
  }
  if (learnr:::is_tags(text)) {
    return(text)
  }
  if (!is.null(text)) {
    # convert markdown
    md <- markdown::markdownToHTML(
      text = text,
      options = c("use_xhtml", "fragment_only", "mathjax"),
      extensions = markdown::markdownExtensions(),
      fragment.only = TRUE,
      encoding = "UTF-8"
    )
    # remove leading and trailing paragraph
    md <- sub("^<p>", "", md)
    md <- sub("</p>\n?$", "", md)
    md <- HTML(md)
    # add a question button
    ns <- NS(label)
    submit_markup <-
      uiOutput(ns("action_button_container"), inline=TRUE)
    feedback_markup <-
      uiOutput(ns("message_container"), inline=TRUE)
    span(md, submit_markup, feedback_markup)

  }
  else {
    NULL
  }
}
