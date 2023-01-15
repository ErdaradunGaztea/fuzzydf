#' @importFrom purrr partial
#' @export
qual_rnorm <- function(label, mean = 0, sd = 1, domain = "people") {
  qual(
    label,
    low = partial(summarizer_rnorm_low, mean = mean, sd = sd),
    medium = partial(summarizer_rnorm_medium, mean = mean, sd = sd),
    high = partial(summarizer_rnorm_high, mean = mean, sd = sd),
    .domain = domain, .classes = "fuzzy_rnorm"
  )
}

summarizer_rnorm_low <- function(val, mean, sd) {
  summarizer_below(mean - 2 * sd, mean - sd)(val)
}

summarizer_rnorm_medium <- function(val, mean, sd) {
  summarizer_trapezoid(mean - 2 * sd, mean - sd, mean + sd, mean + 2 * sd)(val)
}

summarizer_rnorm_high <- function(val, mean, sd) {
  summarizer_over(mean + sd, mean + 2 * sd)(val)
}

#' @importFrom purrr partial
#' @export
qual_runif <- function(label, min = 0, max = 1, domain = "people") {
  qual(
    label,
    low = partial(summarizer_runif_low, min = min, max = max),
    medium = partial(summarizer_runif_medium, min = min, max = max),
    high = partial(summarizer_runif_high, min = min, max = max),
    .domain = domain, .classes = "fuzzy_runif"
  )
}

summarizer_runif_low <- function(val, min, max) {
  span <- max - min
  summarizer_below(min + .2 * span, min + .35 * span)(val)
}

summarizer_runif_medium <- function(val, min, max) {
  span <- max - min
  summarizer_trapezoid(
    min + .2 * span, min + .35 * span, max - .2 * span, max - .35 * span
  )(val)
}

summarizer_runif_high <- function(val, min, max) {
  span <- max - min
  summarizer_over(max - .2 * span, max - .35 * span)(val)
}
