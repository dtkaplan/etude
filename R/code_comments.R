#' Adds comments to the output of code in a learnr block.
#'
#' Often the scaffolding put in a `{learnr}` exercise chunk consists of
#' more than one command that produces output. By default, `{learnr}` will
#' print the outputs one after the other. This can make it hard for the reader
#' to determine which line of code produced which output. `announce()` can insert comments
#' into the output.
#'
#' @details Use `announce()` in either of two ways:
#' 1. As a simple function call, e.g. `announce("my message")` which will display
#' "my message" as a comment at the corresponding point in the exercise chunk output.
#' 2. As the receiver of a piped-in command, e.g. `sqrt(2 + 3) %>% announce("my message")`. This
#' will print out the code on  the LHS of the pipe, along with the message as a comment.
#'
#' @param code the code that you want to display
#' @param comment A character string to print as a comment
#'
#' @export
announce <- function(code, comment) {
  sc <- sys.calls()
  if (is.character(code) && length(code) == 1 && missing(comment)) {
    cat(paste("#", code, "\n"))
    invisible("comment")
  } else {
  the_expression <- as.character(sc[[1]])[2]
  cat(paste(">", the_expression, "#", comment, "\n"))
  return(code)
  }
}

