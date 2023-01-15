#' @importFrom rlang env
new_fuzzy_value <- function(x = double(), belonging = vec_rep(1, vec_size(x))) {
  x <- vec_cast(x, double())
  new_vctr(x, class = "fuzzy_value", belonging = belonging)
}

# is_fuzzy_value <- function(x) {
#   inherits(x, "fuzzy_value")
# }

#' @export
format.fuzzy_value <- function(x, ...) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(vec_data(x), digits = 2, format = "f")
  )
}

#' @export
vec_ptype_abbr.fuzzy_value <- function(x, ...) {
  "fuzzy_val"
}

# CASTING ---------------------------------------------------------------------

#' @export
vec_ptype2.fuzzy_value.double <- function(x, y, ...) x
#' @export
vec_ptype2.double.fuzzy_value <- function(x, y, ...) y
#' @export
vec_ptype2.fuzzy_value.logical <- function(x, y, ...) x
#' @export
vec_ptype2.logical.fuzzy_value <- function(x, y, ...) y
#' @export
vec_cast.double.fuzzy_value <- function(x, to, ...)
  vec_data(x)
#' @export
vec_cast.fuzzy_value.double <- function(x, to, ...)
  new_fuzzy_value(x, belonging(to))
#' @export
vec_cast.fuzzy_value.logical <- function(x, to, ...)
  new_fuzzy_value(x, belonging(to))

# ARITHMETIC ------------------------------------------------------------------

#' @export
#' @method vec_arith fuzzy_value
vec_arith.fuzzy_value <- function(op, x, y, ...) {
  UseMethod("vec_arith.fuzzy_value", y)
}

#' @export
#' @method vec_arith.fuzzy_value fuzzy_value
vec_arith.fuzzy_value.fuzzy_value <- function(op, x, y, ...) {
  switch(
    op,
    "&" = do.call(fuzzy_and, vec_cast_common(x, y)),
    "|" = do.call(fuzzy_or, vec_cast_common(x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.fuzzy_value numeric
vec_arith.fuzzy_value.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "&" = ,
    "|" = stop_incompatible_op(op, x, y),
    vec_cast(vec_arith_base(op, x, y), vec_ptype(x))
  )
}
#' @export
#' @method vec_arith.numeric fuzzy_value
vec_arith.numeric.fuzzy_value <- function(op, x, y, ...) {
  switch(
    op,
    "&" = ,
    "|" = stop_incompatible_op(op, x, y),
    vec_cast(vec_arith_base(op, x, y), vec_ptype(y))
  )
}

#' @export
#' @method vec_arith.fuzzy_value logical
vec_arith.fuzzy_value.logical <- function(op, x, y, ...) {
  switch(
    op,
    "&" = do.call(fuzzy_and, vec_cast_common(x, y)),
    "|" = do.call(fuzzy_or, vec_cast_common(x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.logical fuzzy_value
vec_arith.logical.fuzzy_value <- function(op, x, y, ...) {
  switch(
    op,
    "&" = do.call(fuzzy_and, vec_cast_common(x, y)),
    "|" = do.call(fuzzy_or, vec_cast_common(x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.fuzzy_value MISSING
vec_arith.fuzzy_value.MISSING <- function(op, x, y, ...) {
  switch (
    op,
    "!" = fuzzy_not(x),
    stop_incompatible_op(op, x, y)
  )
}
