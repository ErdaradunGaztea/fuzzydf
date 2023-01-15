summarizer_below <- function(a, b) {
  function(val) {
    case_when(
      is.na(val) ~ NA_real_,
      val <= a ~ 1,
      val <= b ~ (b - val) / (b - a),
      TRUE ~ 0
    )
  }
}

summarizer_over <- function(a, b) {
  function(val) {
    case_when(
      is.na(val) ~ NA_real_,
      val <= a ~ 0,
      val <= b ~ (val - a) / (b - a),
      TRUE ~ 1
    )
  }
}

summarizer_trapezoid <- function(a, b, c, d) {
  if (b < c) {
    # The usual trapezoid
    function(val) {
      case_when(
        is.na(val) ~ NA_real_,
        val <= a ~ 0,
        val <= b ~ (val - a) / (b - a),
        val <= c ~ 1,
        val <= d ~ (d - val) / (d - c),
        TRUE ~ 0
      )
    }
  } else {
    middle_point <- (b * d - a * c) / (d + b - c - a)
    # A triangle with height(A) <= 1
    function(val) {
      case_when(
        is.na(val) ~ NA_real_,
        val <= a ~ 0,
        val <= middle_point ~ (val - a) / (b - a),
        val <= d ~ (d - val) / (d - c),
        TRUE ~ 0
      )
    }
  }
}
