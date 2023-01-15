#' Define meanings of columns
#'
#' @description TODO
#'
#' @param .data \code{data.frame}\cr
#'  Table (possibly fuzzy) to define column meanings for.
#' @param ... \code{fuzzy_quality(1)}\cr
#'  Fuzzy qualities to define, each for one column. Must be named after target
#'  columns.
#'
#' @return A `fuzzy_df` with qualities defined for specified columns (possibly
#' more, if `.data` was a `fuzzy_df` with some already defined qualities).
#'
#' @examples
#' # The key building block of fuzzydf package
#' infert %>%
#'   define_quality(age = qual_age())
#'
#' @importFrom purrr modify_in
#' @importFrom rlang enquos eval_tidy
#' @export
define_quality <- function(.data, ...) {
  # TODO: what if quality was defined by wrapping target column in qual_xxx()?
  dots <- enquos(...)
  .data <- as_fuzzy_df(.data)

  # TODO: use Reduce() instead of for
  for (col in names(dots)) {
    .data <- modify_in(.data, col, ~ {
      eval_tidy(dots[[col]], data = .data)(.x, belonging(.data))
    })
  }
  .data
}

# TODO: perhaps fuzzy_tbl() function that does the same as define_quality()?

qual <- function(name, ..., .domain = "people", .classes = NULL) {
  # Domain should be written in plural
  function(.col, belonging) {
    new_fuzzy_quality(
      .col,
      label = name,
      summarizers = list(...),
      belonging = belonging
    )
  }
}
