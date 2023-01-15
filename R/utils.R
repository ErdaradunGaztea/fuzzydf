#' Retrieve fuzzy belonging of records
#'
#' @description TODO
#'
#' @param x \code{fuzzy_df}\cr
#'  A table, whose rows' belonging is retrieved.
#'
#' @return A vector of numeric values between 0 and 1 of length equal to the
#' number of rows in `x`. If \code{\link{fuzzy_filter}()} was never applied to
#' `x`, the result is a vector of 1.
#'
#' @examples
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' PimaIndiansDiabetes2 %>%
#'   define_quality(age = qual_age(), mass = qual_bmi()) %>%
#'   fuzzy_filter(age %is% young) %>%
#'   belonging()
#'
#' @export
belonging <- function(x) {
  structure(
    attr(x, "belonging", exact = TRUE),
    class = c("fuzzy_belonging", "numeric")
  )
}

#' @export
print.fuzzy_belonging <- function(x, ...) {
  cat(sprintf(
    "belonging <%s>\nAvg. belonging of %0.3f",
    vec_size(x),
    mean(x)
  ))
}

#' @importFrom rlang as_data_pronoun env new_data_mask new_environment
fuzzy_mask <- function(.data) {
  fuzzy_vals <- retrieve_fuzzy_columns(.data)
  col_names <- new_environment(.data)
  role_names <- env(col_names, !!!fuzzy_vals)

  mask <- new_data_mask(role_names, top = col_names)
  mask$.col <- as_data_pronoun(col_names)
  mask$.role <- as_data_pronoun(role_names)
  mask$.data <- as_data_pronoun(mask)

  mask
}

#' @importFrom dplyr select
#' @importFrom purrr map_chr
#' @importFrom tidyselect where
retrieve_fuzzy_columns <- function(.data) {
  fuzzy_cols <- select(.data, where(is_fuzzy_quality))
  names(fuzzy_cols) <- map_chr(fuzzy_cols, attr, "label", exact = TRUE)
  lapply(fuzzy_cols, attr, "values", exact = TRUE)
}
