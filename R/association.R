#' Support for a statement
#'
#' @description TODO
#'
#' @template .data
#' @param ... \code{fuzzy statement}\cr
#'  Statements to evaluate.
#'
#' @return A single numeric value between 0 and 1, the larger the more support
#' for the conjunction of all statements there is in the data.
#'
#' @examples
#' # Can we say that people are young and grade-1 obese?
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   support(age %is% young, bmi %is% grade1_obese)
#'
#' @importFrom rlang enquos
#' @export
support <- function(.data, ...) {
  dots <- enquos(...)
  mask <- fuzzy_mask(.data)

  values <- lapply(dots, function(dot) {
    eval_tidy(dot, data = mask)
  })

  do.call(compute_fraction, values)
}

#' Appropriateness of a statement
#'
#' @description TODO
#'
#' @template data
#' @param qualifier \code{fuzzy statement}\cr
#'  Grouping of records towards which to compute appropriateness.
#' @param statement \code{fuzzy statement}\cr
#'  A statement to evaluate.
#'
#' @return A single numeric value between -1 and 1, the larger the more
#' appropriate the statement is.
#'
#' @examples
#' # Are obese people younger?
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   appropriateness(bmi %is% obese, age %is% young)
#'
#' @export
appropriateness <- function(.data, qualifier, statement) {
  # TODO: implement without fuzzy_filter(), because it's extremely costly
  pro <- support(
    fuzzy_filter(.data, {{ qualifier }}),
    {{ statement }}
  )
  against <- support(
    fuzzy_filter(.data, fuzzy_not({{ qualifier }})),
    {{ statement }}
  )
  # Ranges [-1; 1]
  pro - against
}

# TODO: implement confidence() with one property and suggest in docs using
#  fuzzy_filter() + support() when more than one property is needed
