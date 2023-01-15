#' @export
qual_age <- function(domain = "people") {
  qual(
    "age",
    young = summarizer_age_young,
    middle_aged = summarizer_age_middleaged,
    old = summarizer_age_old,
    .domain = domain, .classes = "fuzzy_age"
  )
}

summarizer_age_young <- function(age) {
  summarizer_below(20, 45)(age)
}

summarizer_age_middleaged <- function(age) {
  summarizer_trapezoid(28, 35, 50, 60)(age)
}

summarizer_age_old <- function(age) {
  summarizer_over(45, 70)(age)
}
