#' Fit a {parsnip} model and manipulate it as a base R model like lm
#'
#' @description [fit_model()] takes a **model_spec** object from {parsnip} and
#' it fits is. Then, usual methods like [summary()], or [coef()] can be applied
#' directly on it, while it can still be used as the {tidymodels} recommends it.
#'
#' @param data A data frame (or a **model_fit** object for [chart()])
#' @param formula A formula specifying a model
#' @param ... Further arguments passed to the method
#' @param type The type of model fitting, specified by a **model_spec** object
#'   or the name of such an object in a string
#' @param env The environment where to evaluate `type`. It is `parent.frame()`
#'   by default and you probably have no reasons to change it, unless you really
#'   know what you are doing!
#' @param object A **model_fit** object
#' @param model Idem
#' @param x Idem
#' @param y Not used here
#' @param parm Specification of parameters for the confidence intervals (vector of numbers or of names). If missing, all parameters are considered.
#' @param level Confidence level required.
#' @param k The penalty per parameter to be used in the AIC (by default, `k = 2`).
#'
#' @return A **model_fit** object.
#' @export
#'
#' @examples
#' library(parsnip)
#' data(trees, package = "datasets")
#'
#' # Take the habit to prefix your regression model specs by `reg_`
#' reg_lm <- linear_reg(mod = "regression", engine = "lm")
#' trees_fit <- fit_model$reg_lm(data = trees, Volume ~ Girth)
#'
#' # You can use summary(), AIC(), anova(), tidy(), glance(), etc. directly
#' summary(trees_fit)
#' anova(trees_fit)
#' AIC(trees_fit)
#' coef(trees_fit)
#' library(chart)
#' chart(trees_fit)
#' # etc.
fit_model <- structure(function(data, formula, ..., type = NULL, env = parent.frame()) {
  if (is.character(type))
    type <- get0(type,envir = env)
  if (is.null(type) || !inherits(type, "model_spec"))
    stop("The type= argument must provide a model_spec object or its name in a character string.")
  fit(type, data = data, formula = formula, ...)
}, class = c("function", "subsettable_type"))

# Already in {parsnip}: glance, tidy, augment, autoplot, predict, print

#' @export
#' @rdname fit_model
summary.model_fit <- function(object, ...)
  summary(object$fit, ...)

#' @export
#' @rdname fit_model
anova.model_fit <- function(object, ...)
  anova(object$fit, ...)

#' @export
#' @rdname fit_model
plot.model_fit <- function(x, y, ...)
  plot(x$fit, ...)

#' @export
#' @rdname fit_model
chart.model_fit <- function(data, ..., type = "model", env = parent.frame())
  chart(data$fit, ..., name = deparse(substitute(data)), type = type, env = env)

#' @export
#' @rdname fit_model
as.function.model_fit <- function(x, ...)
  as.function(x$fit, ...)

#' @export
#' @rdname fit_model
coef.model_fit <- function(object, ...)
  coef(object$fit, ...)

#' @export
#' @rdname fit_model
vcov.model_fit <- function(object, ...)
  vcov(object$fit, ...)

#' @export
#' @rdname fit_model
confint.model_fit <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) {
    confint(object$fit, level = level, ...)
  } else {
    confint(object$fit, parm = parm, level = level, ...)
  }
}

#' @export
#' @rdname fit_model
fitted.model_fit <- function(object, ...)
  fitted(object$fit, ...)

#' @export
#' @rdname fit_model
residuals.model_fit <- function(object, ...)
  residuals(object$fit, ...)

#' @export
#' @rdname fit_model
rstandard.model_fit <- function(model, ...)
  rstandard(model$fit, ...)

#' @export
#' @rdname fit_model
cooks.distance.model_fit <- function(model, ...)
  cooks.distance(model$fit, ...)

#' @export
#' @rdname fit_model
hatvalues.model_fit <- function(model, ...)
  hatvalues(model$fit)

#' @export
#' @rdname fit_model
deviance.model_fit <- function(object, ...)
  deviance(object$fit, ...)

#' @export
#' @rdname fit_model
AIC.model_fit <- function(object,..., k = 2)
  AIC(object$fit, ..., k = k)

#' @export
#' @rdname fit_model
BIC.model_fit <- function(object,...)
  BIC(object$fit, ...)

#' @export
#' @rdname fit_model
family.model_fit <- function(object, ...)
  family(object$fit, ...)

#' @export
#' @rdname fit_model
nobs.model_fit <- function(object, ...)
  nobs(object$fit, ...)

#' @export
#' @rdname fit_model
formula.model_fit <- function(x, ...)
  formula(x$fit, ...)

#' @export
#' @rdname fit_model
variable.names.model_fit <- function(object, ...)
  variable.names(object$fit, ...)

#' @export
#' @rdname fit_model
labels.model_fit <- function(object, ...)
  labels(object$fit, ...)
