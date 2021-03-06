% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/etude_list.R
\name{etude_list}
\alias{etude_list}
\title{Include multiple etude exercises}
\usage{
etude_list(spec, keep_ref = FALSE)
}
\arguments{
\item{spec}{A data frame with one row for
each exercise to be inclued in the parent document. The variables
must be named \code{exercise}, \code{answers}, \code{documentation}, \code{package}, and \code{name}.
(Any additional variables will simply be ignored by \code{etude_list}.)}

\item{keep_ref}{A logical flag, \code{FALSE} by default. If \code{TRUE} then lines
defining pandoc references will be retained in the output document. If
the parent document is being compiled using bookdown, this is what you want, since
the references will be replaced appropriately by pandoc. But if not using bookdown,
the references will appear verbatim in the output, which is probably not what
you want.}
}
\description{
Include multiple etude exercises
}
\examples{
\dontrun{
etude_list(
 tibble(exercise="dog-drink-ring",
      answers=FALSE, documentation=FALSE, name="Problem 1: "
      )
)
}
}
