is_subset <- function(x, y) {
  # TODO: make S3 generic
  out <- x <= y
  all(out[!is.na(out)])
}

any_subset <- function(x) {
  if (length(x) == 1) {
    return(FALSE)
  }

  any(vapply(combn(x, 2, simplify = FALSE), function(pair) {
    is_subset(pair[[1]], pair[[2]]) || is_subset(pair[[2]], pair[[1]])
  }, logical(1)))
}
