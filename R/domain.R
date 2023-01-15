#' @export
define_domain <- function(.data, domain) {
  .data <- as_fuzzy_df(.data)
  attr(.data, "domain") <- domain
  .data
}
