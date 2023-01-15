#' @export
qual_bmi <- function(label = "bmi", domain = "people") {
  qual(
    label,
    underweight = summarizer_bmi_underweight,
    severe_underweight = summarizer_bmi_severe_underweight,
    moderate_underweight = summarizer_bmi_moderate_underweight,
    mild_underweight = summarizer_bmi_mild_underweight,
    normal = summarizer_bmi_normal,
    overweight = summarizer_bmi_overweight,
    obese = summarizer_bmi_obese,
    grade1_obese = summarizer_bmi_grade1_obese,
    grade2_obese = summarizer_bmi_grade2_obese,
    grade3_obese = summarizer_bmi_grade3_obese,
    .domain = domain, .classes = "fuzzy_bmi"
  )
}

summarizer_bmi_underweight <- function(bmi) {
  summarizer_below(16.65, 20.35)(bmi)
}

summarizer_bmi_severe_underweight <- function(bmi) {
  summarizer_below(14.4, 17.6)(bmi)
}

summarizer_bmi_moderate_underweight <- function(bmi) {
  summarizer_trapezoid(14.4, 17.6, 15.3, 18.7)(bmi)
}

summarizer_bmi_mild_underweight <- function(bmi) {
  summarizer_trapezoid(15.3, 18.7, 16.65, 20.35)(bmi)
}

summarizer_bmi_normal <- function(bmi) {
  summarizer_trapezoid(16.65, 20.35, 22.5, 27.5)(bmi)
}

summarizer_bmi_overweight <- function(bmi) {
  summarizer_trapezoid(22.5, 27.5, 27, 33)(bmi)
}

summarizer_bmi_obese <- function(bmi) {
  summarizer_over(27, 33)(bmi)
}

summarizer_bmi_grade1_obese <- function(bmi) {
  summarizer_trapezoid(27, 33, 31.5, 38.5)(bmi)
}

summarizer_bmi_grade2_obese <- function(bmi) {
  summarizer_trapezoid(31.5, 38.5, 36, 44)(bmi)
}

summarizer_bmi_grade3_obese <- function(bmi) {
  summarizer_over(36, 44)(bmi)
}
