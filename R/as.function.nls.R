#' Transform an nls model into a function
#'
#' @description Transforming an nls model into a function could be useful to
#'   plot or otherwise manipulate it, see examples.
#'
#' @param x An nls model
#' @param ... Further arguments to the method (not used for now)
#'
#' @return A function with argument `x` that returns the values predicted by the
#'   model for these values of `x`.
#' @export
#'
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#'
#' # Show this on a ggplot
#' library(ggplot2)
#' p <- ggplot(chick1, aes(x = Time, y = weight)) +
#'   geom_point() +
#'   stat_function(fun = as.function(chick1_logis), col = "red")
#' p
#'
#' # Visually compare with another model (four-parameter logistic):
#' chick1_fpl <- nls(weight ~ SSfpl(Time, A, B, xmid, scal), data = chick1)
#'
#' p + stat_function(fun = as.function(chick1_fpl), col = "blue")
as.function.nls <- function(x, ...) {
  nls_model <- x
  name_x <- names(nls_model$dataClasses)
  stopifnot(length(name_x) == 1)
  function(x) predict(nls_model, newdata = structure(list(x), names = name_x))
}
