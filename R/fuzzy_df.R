#' @importFrom purrr modify_if
#' @importFrom tibble new_tibble
new_fuzzy_df <- function(x, belonging = rep(1, NROW(x)), ..., class = character()) {
  out <- new_tibble(
    x, ..., nrow = NROW(x), class = c(class, "fuzzy_df")
  )
  update_belonging(out, belonging)
}

update_belonging <- function(x, belonging) {
  UseMethod("update_belonging", x)
}

#' @export
update_belonging.default <- function(x, belonging) x

#' @export
update_belonging.fuzzy_df <- function(x, belonging) {
  attr(x, "belonging") <- belonging
  modify_if(x, is_fuzzy_quality, update_belonging, belonging = belonging)
}

#' @export
update_belonging.fuzzy_quality <- function(x, belonging) {
  attr(x, "belonging") <- belonging
  attr(x, "values") <- lapply(
    attr(x, "values", exact = TRUE), update_belonging, belonging = belonging
  )
  x
}

#' @export
update_belonging.fuzzy_value <- function(x, belonging) {
  attr(x, "belonging") <- belonging
  x
}

as_fuzzy_df <- function(.data) {
  UseMethod("as_fuzzy_df")
}

as_fuzzy_df.data.frame <- function(.data) {
  new_fuzzy_df(.data)
}

as_fuzzy_df.fuzzy_df <- function(.data) .data

# TODO: ?dplyr_extending for the idea for fuzzy_df class

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.fuzzy_df <- function(data, i, ...) {
  # This function should subset `belonging` attribute
  out_belonging <- vec_slice(belonging(data), i)
  out <- vec_slice(data, i)
  # TODO: do it so that you wouldn't lose new classes
  new_fuzzy_df(out, out_belonging)
}
