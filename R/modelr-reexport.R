#' Reexport of modelr functions
#'
#' @description [add_predictions()] and [add_residuals()] are pipe-friendly
#'   functions to add predictions or residuals to a data frame.
#'   [geom_ref_line()] adds a vertical of horizontal reference line. [rmse()]
#'   (the root-mean-squared-error), [mae[]] (the mean absolute error), [qae()]
#'   (the quantiles of absolute error) and [rsquare()] (the variance of the
#'   predictions divided by the variance of the response) are useful model
#'   metrics.
#'
#' @param data A data frame
#' @param model A model that has a `predict()` method.
#' @param var A string with the name of the predictions or residuals variable
#'   (by default, it is `"pred"` and `"resid"` respectively)
#' @param type If the model's `predict()` method has a `type=` argument, you can
#'   give it here.
#' @param h Position of the horizontal reference line
#' @param v Position of the vertical reference line
#' @param size The width of the reference line
#' @param color The color of the reference line
#' @param colour Same as above (use the one you prefer)
#' @param probs A numeric vector of probabilities
#'
#' @return A function with argument `x` that returns the values predicted by the
#'   model for these values of `x`.
#' @export
#' @name modelr-reexport
#'
#' @examples
#' data(trees, package = "datasets")
#' trees_lm <- lm(Volume ~ Girth + I(Girth^2), data = trees)
#' rmse(trees_lm, trees)
#' rsquare(trees_lm, trees)
#' mae(trees_lm, trees)
#' qae(trees_lm, trees, probs = c(0, 0.25, 0.5, 0.75, 1)) # Resids five numbers
#'
#' add_predictions(trees, trees_lm)
#' add_residuals(trees, trees_lm)
#'
#' library(chart)
#' chart(trees_lm) +
#'   geom_ref_line(h = 0) # Not particularly useful here, just an example
add_predictions <- modelr::add_predictions

#' @export
#' @rdname modelr-reexport
add_residuals <- modelr::add_residuals

#' @export
#' @rdname modelr-reexport
geom_ref_line <- function(h, v, color = "red", colour = color, size = 1) {
  if (missing(h) && missing(v))
    stop("You must give either 'h=' or 'v='.", call = FALSE)
  if (!missing(h)) {
    modelr::geom_ref_line(h = h, size = size, colour = colour)
  } else {
    modelr::geom_ref_line(v = v, size = size, colour = colour)
  }
}

#' @export
#' @rdname modelr-reexport
rmse <- modelr::rmse

#' @export
#' @rdname modelr-reexport
mae <- modelr::mae

#' @export
#' @rdname modelr-reexport
qae <- modelr::qae

#' @export
#' @rdname modelr-reexport
rsquare <- modelr::rsquare
