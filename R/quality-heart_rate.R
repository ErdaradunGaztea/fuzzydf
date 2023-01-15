#' @export
qual_heart_rate <- function(domain = "people") {
  qual(
    "heart_rate",
    bradycardia = summarizer_heart_rate_bradycardia,
    normal = summarizer_heart_rate_normal,
    tachycardia = summarizer_heart_rate_tachycardia,
    .domain = domain, .classes = "fuzzy_heart_rate"
  )
}

summarizer_heart_rate_bradycardia <- function(heart_rate) {
  summarizer_below(38, 54)(heart_rate)
}

summarizer_heart_rate_normal <- function(heart_rate) {
  summarizer_trapezoid(38, 54, 88, 100)(heart_rate)
}

summarizer_heart_rate_tachycardia <- function(heart_rate) {
  summarizer_over(88, 100)(heart_rate)
}
