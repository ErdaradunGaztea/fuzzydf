#' Apply a selector to multiple statements
#'
#' @description TODO
#'
#' @param ... \code{fuzzy statement}\cr
#'  Statements to apply a selector to. Each statement must have the same length.
#'
#' @return Fuzzy values of length equal to the length of any parameter passed to
#' `...`.
#'
#' @examples
#' # Is it true that few people are neither young nor obese?
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   truth_of(q_few(sel_none_of(age %is% young, bmi %is% obese)))
#'
#' @name fuzzy-selector

#' @rdname fuzzy-selector
#' @export
sel_all_of <- function(...) {
  fuzzy_and(...)
}

# sel_most_of
# sel_some_of
# sel_few_of

#' @rdname fuzzy-selector
#' @export
sel_any_of <- function(...) {
  fuzzy_or(...)
}

#' @rdname fuzzy-selector
#' @importFrom rlang list2
#' @export
sel_none_of <- function(...) {
  dots <- list2(...)
  do.call(fuzzy_and, lapply(dots, `!`))
}

# sel_none_of(...) = 1 - sel_any_of(...)
