#' @export
qual_blood_pressure <- function(type = c("diastolic", "systolic"),
                                domain = "people") {
  type <- match.arg(type)
  switch(
    type,
    diastolic = qual_diastolic_blood_pressure(domain = domain),
    systolic = qual_systolic_blood_pressure(domain = domain)
  )
}

# DIASTOLIC BLOOD PRESSURE ----------------------------------------------------
qual_diastolic_blood_pressure <- function(domain = "people") {
  qual(
    "diastolic_BP",
    hypotension = summarizer_dia_blood_press_hypotension,
    optimal = summarizer_dia_blood_press_optimal,
    normal = summarizer_dia_blood_press_normal,
    high_normal = summarizer_dia_blood_press_high_normal,
    hypertension = summarizer_dia_blood_press_hypertension,
    grade1_hypertension = summarizer_dia_blood_press_grade1_hypertension,
    grade2_hypertension = summarizer_dia_blood_press_grade2_hypertension,
    grade3_hypertension = summarizer_dia_blood_press_grade3_hypertension,
    .domain = domain, .classes = c("fuzzy_dia_blood_press", "fuzzy_blood_press")
  )
}

summarizer_dia_blood_press_hypotension <- function(bp) {
  summarizer_below(52, 66)(bp)
}

summarizer_dia_blood_press_optimal <- function(bp) {
  summarizer_trapezoid(52, 66, 74, 86)(bp)
}

summarizer_dia_blood_press_normal <- function(bp) {
  summarizer_trapezoid(74, 86, 84, 96)(bp)
}

summarizer_dia_blood_press_high_normal <- function(bp) {
  summarizer_trapezoid(79, 91, 84, 96)(bp)
}

summarizer_dia_blood_press_hypertension <- function(bp) {
  summarizer_over(84, 96)(bp)
}

summarizer_dia_blood_press_grade1_hypertension <- function(bp) {
  summarizer_trapezoid(84, 96, 94, 106)(bp)
}

summarizer_dia_blood_press_grade2_hypertension <- function(bp) {
  summarizer_trapezoid(94, 106, 104, 116)(bp)
}

summarizer_dia_blood_press_grade3_hypertension <- function(bp) {
  summarizer_over(104, 116)(bp)
}

# SYSTOLIC BLOOD PRESSURE -----------------------------------------------------
qual_systolic_blood_pressure <- function(domain = "people") {
  qual(
    "systolic_BP",
    hypotension = summarizer_sys_blood_press_hypotension,
    optimal = summarizer_sys_blood_press_optimal,
    normal = summarizer_sys_blood_press_normal,
    high_normal = summarizer_sys_blood_press_high_normal,
    hypertension = summarizer_sys_blood_press_hypertension,
    grade1_hypertension = summarizer_sys_blood_press_grade1_hypertension,
    grade2_hypertension = summarizer_sys_blood_press_grade2_hypertension,
    grade3_hypertension = summarizer_sys_blood_press_grade3_hypertension,
    .domain = domain, .classes = c("fuzzy_sys_blood_press", "fuzzy_blood_press")
  )
}

summarizer_sys_blood_press_hypotension <- function(bp) {
  summarizer_below(82, 98)(bp)
}

summarizer_sys_blood_press_optimal <- function(bp) {
  summarizer_trapezoid(82, 98, 112, 128)(bp)
}

summarizer_sys_blood_press_normal <- function(bp) {
  summarizer_trapezoid(112, 128, 132, 148)(bp)
}

summarizer_sys_blood_press_high_normal <- function(bp) {
  summarizer_trapezoid(122, 138, 132, 148)(bp)
}

summarizer_sys_blood_press_hypertension <- function(bp) {
  summarizer_over(132, 148)(bp)
}

summarizer_sys_blood_press_grade1_hypertension <- function(bp) {
  summarizer_trapezoid(132, 148, 152, 168)(bp)
}

summarizer_sys_blood_press_grade2_hypertension <- function(bp) {
  summarizer_trapezoid(152, 168, 172, 188)(bp)
}

summarizer_sys_blood_press_grade3_hypertension <- function(bp) {
  summarizer_over(172, 188)(bp)
}
