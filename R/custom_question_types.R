#' Custom learnr question types
#'
#'
#' Essay box
#' @export
essay_response <- function(
  prompt = "Your answer here ..."
) {
  question(text = prompt,
           answer("", correct=TRUE),
           allow_retry = TRUE,
           incorrect = "Message received!",
           type = "text_block",
           try_again_button = "Do you want to re-edit?")
}


#' Inline radio buttons
#' @export
question_ui_initialize.learnr_radio_inline <-
  function(question, value, ...) {
    choice_names <- learnr:::answer_labels(question)
    choice_values <- learnr:::answer_values(question)

    radioButtons(
      question$ids$answer,
      label = question$question,
      inline = TRUE,
      choiceNames = choice_names,
      choiceValues = choice_values,
      selected = value %||% FALSE # setting to NULL, selects the first item
    )
  }
#' Inline checkbox
#' @export
question_ui_initialize.learnr_checkbox_inline <-
  function(question, value, ...) {
    choice_names <- learnr:::answer_labels(question)
    choice_values <- learnr:::answer_values(question)

    shiny::checkboxGroupInput(
      question$ids$answer,
      label = question$question,
      inline = TRUE,
      choiceNames = choice_names,
      choiceValues = choice_values,
      selected = value %||% FALSE # setting to NULL, selects the first item
    )
  }
#' @export
question_is_correct.learnr_checkbox_inline <-
  learnr:::question_is_correct.learnr_checkbox
#' @export
question_is_correct.learnr_radio_inline <-
  learnr:::question_is_correct.learnr_radio

#' @export
question_is_correct.learnr_radio_inline <-
  learnr:::question_is_correct.learnr_radio

#' @export
question_ui_initialize.text_block <-
  function(question, value, ...) {
    textAreaInput(
      question$ids$answer,
      label = question$question,
      placeholder = question$options$placeholder,
      value = value
    )
  }
#' @export
question_is_correct.text_block <- function(question, value, ...) {
  return(learnr::mark_as(FALSE, message = NULL))
}

#' @export
question_ui_try_again.text_block <-
function(question, value, ...) {
  disable_all_tags(
    question_ui_initialize.text_block(question, value, ...)
  )
}

"%||%" <- function(x, y) if (is.null(x)) y else x
