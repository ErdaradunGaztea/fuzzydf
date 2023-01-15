#' Apply a modifier to a quality
#'
#' @description TODO
#'
#' @param .col \code{fuzzy_quality(1)}\cr
#'  Fuzzy quality to modify.
#'
#' @return Fuzzy values of a column, but appropriately modified.
#'
#' @examples
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   truth_of(q_few(bmi %is% mod_very(obese)))
#'
#' @name fuzzy-modifier
NULL

#' @rdname fuzzy-modifier
#' @export
mod_very <- function(.col) {
  .col^2
}

#' @rdname fuzzy-modifier
#' @export
mod_somewhat <- function(.col) {
  sqrt(.col)
}
