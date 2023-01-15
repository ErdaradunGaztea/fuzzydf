#' Evaluate truth of a statement
#'
#' @description TODO
#'
#' @template data
#' @param proposition \code{fuzzy quantifier}\cr
#'  Statement about data to evaluate.
#'
#' @return A single numeric value between 0 and 1, the higher the more true the
#' statement is.
#'
#' @examples
#' # Is it true that most Star Wars characters are old?
#' starwars %>%
#'   define_quality(birth_year = qual_age()) %>%
#'   truth_of(q_most(age %is% old))
#'
#' # It's possible to evaluate complex statements:
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   truth_of(q_most(age %is% young | bmi %is% obese))
#'
#' @importFrom rlang enquo eval_tidy
#' @export
truth_of <- function(.data, proposition) {
  proposition <- enquo(proposition)
  eval_tidy(proposition, data = fuzzy_mask(.data))
}

#' @importFrom rlang enexpr eval_tidy syms
#' @export
propose_values <- function(.data, quality, quantifier) {
  quality <- enexpr(quality)
  quantifier <- enexpr(quantifier)
  fuzzy_vals <- retrieve_fuzzy_columns(.data)

  values <- names(eval_tidy(quality, fuzzy_vals))

  scores <- vapply(values, function(value) {
    truth_of(.data, (!!quantifier)(!!quality %is% !!sym(value)))
  }, numeric(1), USE.NAMES = FALSE)

  pos_indices <- Filter(function(index) {
    scores[[index]] > 0
  }, order(scores, decreasing = TRUE))

  mapply(
    function(values, score) {
      list(
        value = values,
        score = score
      )
    },
    values[pos_indices], scores[pos_indices],
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
}

#' @importFrom rlang enquo eval_tidy
#' @export
`%is%` <- function(e1, e2) {
  eval_tidy(enquo(e2), data = e1)
}

#' @export
fuzzy_not <- function(x) {
  1 - x
}

#' @export
fuzzy_and <- function(...) {
  pmin(...)
}

#' @export
fuzzy_or <- function(...) {
  pmax(...)
}

#' @export
`%then%` <- function(e1, e2) {
  UseMethod("%then%")
}

#' @export
`%then%.fuzzy_value` <- function(e1, e2) {
  # Lukasiewicz implication
  pmin(1, 1 - e1 + e2)
}
