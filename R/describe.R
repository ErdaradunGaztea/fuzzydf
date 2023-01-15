#' @importFrom purrr compact flatten imap keep
#' @importFrom utils combn
#' @export
describe <- function(.data, ...,
                     truth_threshold = .7,
                     min_support = .15,
                     min_meaningful = .05,
                     max_set_size = 2) {
  fuzzy_cols <- retrieve_fuzzy_columns(.data)

  general_statements <- imap(fuzzy_cols, function(quality, qual_name) {
    set_sizes <- seq_len(min(max_set_size, length(quality) - 1))

    lapply(set_sizes, function(set_size) {
      combn(names(quality), set_size, FUN = function(values) {
        if (!any_subset(quality[values])) {
          support <- do.call(fuzzy_or, quality[values])
          general_statement(qual_name, values, support, truth_threshold)
        }
      }, simplify = FALSE) %>%
        compact()
    }) %>%
      flatten()
  }) %>%
    flatten() %>%
    unname()

  cond_statements <- imap(fuzzy_cols, function(quality, qualname) {
    qual_index <- which(names(fuzzy_cols) == qualname)
    quality <- keep(quality, ~ compute_fraction(.x) >= min_support)

    imap(quality, function(val, valname) {
      fuzzy_cols <- fuzzy_cols[-qual_index]

      imap(fuzzy_cols, function(quality_2, qualname_2) {
        imap(quality_2, function(val_2, valname_2) {
          qualifier <- expr(!!sym(qualname) %is% !!sym(valname))
          statement <- expr(!!sym(qualname_2) %is% !!sym(valname_2))
          appr <- appropriateness(.data, !!qualifier, !!statement)

          conditional_statement(
            qualname, valname, qualname_2, valname_2, appr, min_support
          )
        }) %>%
          compact()
      }) %>%
        flatten()
    }) %>%
      flatten()
  }) %>%
    flatten() %>%
    unname()

  domain <- attr(.data, "domain", exact = TRUE)
  structure(
    c(general_statements, cond_statements),
    class = "fuzzy_desc",
    domain = if (!is.null(domain)) domain else "people"
  )
}

#' @export
as.character.fuzzy_desc <- function(x, ..., .domain = NULL) {
  domain <- if (is.null(.domain)) attr(x, "domain", exact = TRUE) else .domain
  vapply(x, as.character, character(1), ..., .domain = domain)
}
