#' @importFrom rlang env
new_fuzzy_quality <- function(x = double(), label = "", summarizers = list(),
                              belonging = vec_rep(1, vec_size(x))) {
  vec_assert(label, ptype = character(), size = 1)
  new_vctr(
    x,
    label = label,
    summarizers = summarizers,
    belonging = belonging,
    values = lapply(summarizers, function(s) new_fuzzy_value(s(x), belonging)),
    class = "fuzzy_quality"
  )
}

is_fuzzy_quality <- function(x) {
  inherits(x, "fuzzy_quality")
}

#' @export
vec_restore.fuzzy_quality <- function(x, to, ...) {
  new_fuzzy_quality(x, attr(to, "label"), attr(to, "summarizers"), attr(to, "params"))
}

# CASTING ---------------------------------------------------------------------

# TODO: what if quality is of different type than double?
#' @export
vec_ptype2.fuzzy_quality.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.fuzzy_quality <- function(x, y, ...) double()
#' @export
vec_cast.double.fuzzy_quality <- function(x, to, ...) vec_data(x)
