# HELPER VALUES ---------------------------------------------------------------
n <- 10
x <- runif(n)
fv <- new_fuzzy_value(x)
x2 <- runif(n)
fv2 <- new_fuzzy_value(x2)
y <- rep(c(TRUE, FALSE), length.out = n)

# STRUCTURE -------------------------------------------------------------------

test_that("fuzzy_value is a double vector", {
  expect_type(new_fuzzy_value(), "double")
})

test_that("fuzzy_value has belonging attribute", {
  expect_type(
    attr(new_fuzzy_value(), "belonging", exact = TRUE),
    "double"
  )
})

test_that("x may be specified", {
  expect_equal(
    vec_data(new_fuzzy_value(x)),
    x
  )
})

test_that("belonging attribute should be equal length to x", {
  expect_length(
    attr(fv, "belonging", exact = TRUE),
    n
  )
})

test_that("by default belonging attribute is equal to 1", {
  expect_equal(
    attr(fv, "belonging", exact = TRUE),
    vec_rep(1, n)
  )
})

# ARITHMETIC ------------------------------------------------------------------

test_that("AND and OR can be applied to two same-length fuzzy values", {
  expect_equal(
    vec_data(fv & fv2),
    fuzzy_and(x, x2)
  )
  expect_equal(
    vec_data(fv | fv2),
    fuzzy_or(x, x2)
  )
})

test_that("NOT can be applied to a single fuzzy value", {
  expect_equal(
    vec_data(!fv),
    fuzzy_not(x)
  )
})

test_that("operators with fuzzy values return a fuzzy value", {
  expect_vector(
    fv & fv2,
    ptype = vec_ptype_common(fv, fv2),
    size = vec_size_common(fv, fv2)
  )
  expect_vector(
    fv | fv2,
    ptype = vec_ptype_common(fv, fv2),
    size = vec_size_common(fv, fv2)
  )
  expect_vector(
    !fv,
    ptype = vec_ptype(fv),
    size = vec_size(fv)
  )
})

test_that("logical vectors are treated as potential fuzzy values", {
  expect_equal(
    vec_data(fv & y),
    fuzzy_and(x, y)
  )
  expect_equal(
    vec_data(fv | y),
    fuzzy_or(x, y)
  )
  expect_equal(
    vec_data(y & fv),
    fuzzy_and(y, x)
  )
  expect_equal(
    vec_data(y | fv),
    fuzzy_or(y, x)
  )
})

test_that("operators with logical vectors return a fuzzy value", {
  expect_vector(
    fv & y,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, y)
  )
  expect_vector(
    fv | y,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, y)
  )
  expect_vector(
    y & fv,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, y)
  )
})

test_that("all non-logical operators with numeric values are allowed", {
  expect_equal(
    vec_data(fv + x2),
    x + x2
  )
  expect_equal(
    vec_data(fv - x2),
    x - x2
  )
  expect_equal(
    vec_data(fv * x2),
    x * x2
  )
  expect_equal(
    vec_data(fv ^ x2),
    x ^ x2
  )
  expect_equal(
    vec_data(fv %/% x2),
    x %/% x2
  )
})

test_that("operators with numeric values return a fuzzy value", {
  expect_vector(
    fv + x2,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, x2)
  )
  expect_vector(
    fv - x2,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, x2)
  )
  expect_vector(
    fv %/% x2,
    ptype = vec_ptype(fv),
    size = vec_size_common(fv, x2)
  )
})

# TODO: test failing cases (unsupported operations, incorrect input, etc.)
