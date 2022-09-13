#' Transform an lm or glm model into a function
#'
#' @description Transform an lm or glm model that has only two variables into a
#'   function (useful for plotting, see examples).
#'
#' @param x An lm or glm model
#' @param ... Further arguments to the method (not used for now)
#'
#' @return A function with argument `x` that returns the values predicted by the
#'   model for these values of `x`.
#' @export
#'
#' @examples
#' data("trees", package = "datasets")
#' trees_lm1 <- lm(Volume ~ Girth, data = trees)
#' trees_lm2 <- lm(Volume ~ Girth + I(Girth^2), data = trees)
#'
#' # Compare these two models on a chart
#' library(chart)
#' chart(trees, Volume ~ Girth) +
#'   geom_point() +
#'   stat_function(fun = as.function(trees_lm1), col = "red") +
#'   stat_function(fun = as.function(trees_lm2), col = "blue")
#'
#' # The created function can also be used for easy predictions
#' trees_fn1 <- as.function(trees_lm1)
#' trees_fn1(10:20) # Volume for Girth 10:20
as.function.lm <- function(x, ...) {
  lm_model <- x
  terms <- attr(get_formula_x_y(x$terms), "terms")
  # Currently works only with 2 variables
  if (length(terms) != 2)
    stop("Can only transform a model with 2 variables for now.")
  # ... and they must be both numeric (or nmatrix.x for poly())
  classes7 <- substring(attr(x$terms, "dataClasses"), 1L, 7L)
  if (!all(classes7 %in% c("numeric", "nmatrix")))
    stop("Can only make of function from a model involving numeric variables for now.")
  # X name is the second item
  name_x <- terms[2]
  function(x) predict(lm_model, newdata = structure(list(x), names = name_x))
}
