#' @importFrom purrr when
#' @export
quantify <- function(.data, property) {
  belonging <- truth_of(.data, compute_fraction({{ property }}))
  when(
    belonging,
    . <= .01 ~ q_none,
    . <= .35 ~ q_few,
    . <= .65 ~ q_some,
    . <= .99 ~ q_most,
    ~ q_all
  )
}

# RELATIVE QUANTIFIERS --------------------------------------------------------

#' @export
q_all <- structure(
  function(...) {
    fraction <- compute_fraction(...)
    summarizer_over(.97, 1)(fraction)
  },
  class = c("rel_quantif", "fuzzy_quantif", "function"),
  quantity = "all"
)

#' @export
q_most <- structure(
  function(...) {
    fraction <- compute_fraction(...)
    summarizer_trapezoid(.4, .8, .95, 1.2)(fraction)
  },
  class = c("rel_quantif", "fuzzy_quantif", "function"),
  quantity = "most"
)

#' @export
q_some <- structure(
  function(...) {
    fraction <- compute_fraction(...)
    summarizer_trapezoid(.2, .35, .65, .8)(fraction)
  },
  class = c("rel_quantif", "fuzzy_quantif", "function"),
  quantity = "some"
)

#' @export
q_few <- structure(
  function(...) {
    fraction <- compute_fraction(...)
    summarizer_trapezoid(-.2, .05, .2, .6)(fraction)
  },
  class = c("rel_quantif", "fuzzy_quantif", "function"),
  quantity = "few"
)

#' @export
q_none <- structure(
  function(...) {
    fraction <- compute_fraction(...)
    summarizer_below(0, .03)(fraction)
  },
  class = c("rel_quantif", "fuzzy_quantif", "function"),
  quantity = "none"
)

# ABSOLUTE QUANTIFIERS --------------------------------------------------------

# q_about(value, width = 5%)  # relative error
# q_about(value, by = .03 * 10^(num_digits(value)))  # absolute error

# UTILITY FUNCTIONS -----------------------------------------------------------

#' @importFrom stats weighted.mean
compute_fraction <- function(...) {
  values <- fuzzy_and(...)
  weights <- vec_data(belonging(values))
  weights[is.na(weights)] <- 0
  weighted.mean(vec_data(values), weights, na.rm = TRUE)
}

# DISPLAY ---------------------------------------------------------------------

#' @export
print.fuzzy_quantif <- function(x, ...) {
  cat(sprintf(
    "fuzzy quantifier <%s>",
    attr(x, "quantity", exact = TRUE)
  ))
}
