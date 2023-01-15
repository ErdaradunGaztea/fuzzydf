#' Apply a fuzzy filtering
#'
#' @description TODO
#'
#' @template data
#' @param property \code{fuzzy statement}\cr
#'  A statement, value of which determines belonging of a record to the data.
#'
#' @return A `fuzzy_df` object with the same rows as `data`, but with possibly
#' modified `belonging` attribute.
#'
#' @examples
#' # Filter data to later perform analyses only on people who are "not young"
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   fuzzy_filter(!age %is% young)
#'
#' @export
fuzzy_filter <- function(.data, property) {
  # TODO: shouldn't there be multiple properties available?
  new_belonging <- vec_cast(truth_of(.data, {{ property }}), double())
  update_belonging(.data, fuzzy_and(belonging(.data), new_belonging))
}
