
data(mtcars)
mod <- lm(mpg ~ wt + hp, data = mtcars)
mod1 <- lm(mpg ~ wt + hp + 0, data = mtcars)
mod3 <- lm(petal_length ~ sepal_length , data = data.io::read("iris", package = "datasets"))

test_that("output is a list contains all expected named elements", {
  # Verifies that the returned list
  expect_type(.extract_infos_lm(mod), "list")
})

test_that("output contains all expected named elements of the list", {
  # Verifies that the returned list includes all expected components
  res <- .extract_infos_lm(mod, type = "tidy")
  expect_true(all(c("df", "title", "cols", "equa", "terms", "psignif", "footer") %in% names(res)))
})

# df ----------------------------------------------------------------------

test_that("invalid type throws an error", {
  # Ensures an error is thrown for an unsupported type
  expect_error(.extract_infos_lm(mod, type = "unknown"))
})

test_that("type = 'coef' returns expected columns in df", {
  # Checks that the 'coef' type returns a data frame with 'term' and 'estimate'
  res <- .extract_infos_lm(mod, type = "coef")
  expect_equal(colnames(res$df), c("term", "estimate"))
})

test_that("type = 'glance' returns a one-row data frame", {
  # Checks that the 'glance' type returns a summary with one row
  res <- .extract_infos_lm(mod, type = "glance")
  expect_true("df" %in% names(res))
  expect_equal(nrow(res$df), 1)
})

test_that("type = 'tidy' returns expected columns with conf.int = TRUE", {
  # Checks that confidence intervals are included when conf.int = TRUE
  res <- .extract_infos_lm(mod, type = "tidy", conf.int = TRUE)
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "std.error",
                    "statistic", "p.value") %in% colnames(res$df)))
})

test_that("type = 'tidy' without conf.int omits CI columns", {
  # Ensures confidence interval columns are not present when conf.int = FALSE
  res <- .extract_infos_lm(mod, type = "tidy", conf.int = FALSE)
  expect_false("conf.low" %in% colnames(res$df))
})

test_that("signif.stars = TRUE adds 'signif' column", {
  # Checks that significance stars are added when enabled
  res <- .extract_infos_lm(mod, type = "tidy", show.signif.stars = TRUE)
  expect_true("signif" %in% colnames(res$df))
})

test_that("signif.stars = FALSE omits 'signif' column in df", {
  # Ensures 'signif' column is not added when disabled
  res <- .extract_infos_lm(mod, type = "tidy", show.signif.stars = FALSE)
  expect_false("signif" %in% colnames(res$df))
})


# title -------------------------------------------------------------------

test_that("x$title is NULL when title = FALSE/TRUE/chr", {
  res <- .extract_infos_lm(mod, type = "tidy", title = FALSE)
  expect_null(res$title)

  res <- .extract_infos_lm(mod, type = "tidy", title = TRUE)
  expect_equal(res$title, "Linear model")
  #res <- .extract_infos_lm(mod, type = "tidy", title = TRUE, lang = "fr")
  #expect_equal(res$title, "Modèle linéaire")

  res <- .extract_infos_lm(mod, type = "tidy", title = "blablabla")
  expect_type(res$title, "character")
  expect_equal(res$title, "blablabla")
})


# cols --------------------------------------------------------------------

test_that("x$cols is a named character vector", {
  res <- .extract_infos_lm(mod, type = "tidy")

  # Check that 'cols' exists and is a character vector
  expect_true("cols" %in% names(res))
  expect_type(res$cols, "character")

  # Check that it has names
  expect_true(any(nzchar(names(res$cols))))  # names are not empty
})

# equation ----------------------------------------------------------------

test_that("x$equa is an equation ", {
  res <- .extract_infos_lm(mod, type = "coef")

  # Check that 'equa' exists and is a character vector
  expect_true("equa" %in% names(res))
  expect_true(all(class(res$equa) == c("equation", "character")))

  res <- .extract_infos_lm(mod, type = "coef", equation = "tada")
  expect_type(res$equa, "character")
})


# terms -------------------------------------------------------------------
test_that("res$terms is a character vector with or without names depending on
          equation argument", {
  res <- .extract_infos_lm(mod, equation = TRUE)
  expect_type(res$terms, "character")
  # .params_equation returns an unnamed character vector
  expect_null(names(res$terms))

  # .params_equation returns a vector of the same length as the number of rows in df
  expect_true(length(res$terms) == nrow(res$df))

  res <- .extract_infos_lm(mod, equation = FALSE)
  expect_type(res$terms, "character")
  expect_true(any(nzchar(names(res$terms))))

  res <- .extract_infos_lm(mod1, equation = FALSE)
  expect_null(res$terms)

  res <- .extract_infos_lm(mod3, equation = FALSE)
  expect_true(length(res$terms) == nrow(res$df))
  expect_true(any(nzchar(names(res$terms))))
})

# psignif -----------------------------------------------------------------

test_that("x$signif is an equation ", {
  res <- .extract_infos_lm(mod, show.signif.stars = FALSE)
  expect_null(res$psignif)

  res <- .extract_infos_lm(mod, show.signif.stars = TRUE)
  expect_type(res$psignif, "character")
})

# footer ------------------------------------------------------------------

test_that("x$footer is NULL when footer = FALSE", {
  # Checks that footer is NULL when not requested
  res <- .extract_infos_lm(mod, type = "tidy", footer = FALSE)
  expect_null(res$footer)

   # footer is generated when footer = TRUE and a summary.lm object
   res <- .extract_infos_lm(summary(mod), type = "tidy", footer = TRUE)
   expect_true(!is.null(res$footer))

   # footer is not generated when footer = TRUE and a lm object
   expect_error(extract_infos_lm(mod, type = "tidy", footer = TRUE))
 })


