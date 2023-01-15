#' @importFrom purrr when
general_statement <- function(quality, values, support, cutoff = .7) {
  when(
    support,
    q_all(.) >= cutoff ~ general_statement_all(quality, values),
    q_most(.) >= cutoff ~ general_statement_most(quality, values),
    q_none(.) >= cutoff ~ general_statement_none(quality, values),
    ~ NULL
  )
}

general_statement_all <- function(quality, values) {
  structure(
    values,
    class = c("genr_statm_all", "genr_statm", "fuzzy_statm"),
    quality = quality,
    values = values
  )
}

general_statement_most <- function(quality, values) {
  structure(
    values,
    class = c("genr_statm_most", "genr_statm", "fuzzy_statm"),
    quality = quality,
    values = values
  )
}

general_statement_none <- function(quality, values) {
  structure(
    values,
    class = c("genr_statm_none", "genr_statm", "fuzzy_statm"),
    quality = quality,
    values = values
  )
}

conditional_statement <- function(qualif_quality, qualif_value, statm_quality,
                                  statm_value, appropriateness, cutoff = .15) {
  if (abs(appropriateness) >= cutoff) {
    structure(
      statm_value,
      class = c("cond_statm", "fuzzy_statm"),
      qualif_quality = qualif_quality,
      qualif_value = qualif_value,
      statm_quality = statm_quality,
      statm_value = statm_value,
      more_often = appropriateness > 0
    )
  }
}

# METHODS ---------------------------------------------------------------------
#' @export
print.genr_statm <- function(x, ...) {
  cat(sprintf(
    "<general statement[%s]>",
    attr(x, "quality", exact = TRUE)
  ))
}

#' @export
print.cond_statm <- function(x, ...) {
  cat(sprintf(
    "<conditional statement[%s -> %s]>",
    attr(x, "qualif_quality", exact = TRUE),
    attr(x, "statm_quality", exact = TRUE)
  ))
}

#' @importFrom glue glue_collapse
#' @export
as.character.genr_statm_all <- function(x, ..., .domain = "people") {
  sprintf(
    "All %s are of %s %s.",
    .domain,
    glue_collapse(attr(x, "values", exact = TRUE), sep = ", ", last = " or "),
    attr(x, "quality", exact = TRUE)
  )
}

#' @importFrom glue glue_collapse
#' @export
as.character.genr_statm_most <- function(x, ..., .domain = "people") {
  sprintf(
    "Most %s are of %s %s.",
    .domain,
    glue_collapse(attr(x, "values", exact = TRUE), sep = ", ", last = " or "),
    attr(x, "quality", exact = TRUE)
  )
}

#' @importFrom glue glue_collapse
#' @export
as.character.genr_statm_none <- function(x, ..., .domain = "people") {
  sprintf(
    "No %s are of %s %s.",
    .domain,
    glue_collapse(attr(x, "values", exact = TRUE), sep = ", ", last = " or "),
    attr(x, "quality", exact = TRUE)
  )
}

#' @importFrom stringi stri_trans_totitle
#' @export
as.character.cond_statm <- function(x, ..., .domain = "people") {
  sprintf(
    "%s that are of %s %s, are significantly %s often of %s %s.",
    stri_trans_totitle(.domain),
    attr(x, "qualif_value", exact = TRUE),
    attr(x, "qualif_quality", exact = TRUE),
    if (attr(x, "more_often", exact = TRUE)) "more" else "less",
    attr(x, "statm_value", exact = TRUE),
    attr(x, "statm_quality", exact = TRUE)
  )
}
