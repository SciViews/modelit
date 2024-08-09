#' Fitting Linear Models by SciViews
#'
#' @description
#'The lm_() function is used like the lm() function from the {stats} package. It allows adding additional elements such as labels and units.
#'
#' @param data A data frame
#' @param formula An object of class formula
#' @param ... All the arguments of the lm() function
#'
#' @return an lm_ object if attribute additions have been made. Otherwise, the object will be of class lm
#' @export
#'
#' @examples
#' data(iris)
#' res <- lm_(iris, formula = Petal.Length ~ Sepal.Length + Species)
#' res
#' class(res)
#'
lm_ <- function(data, formula, ...) {
  res <- stats::lm(data = data, formula = formula,...)

  # Extract labels ----
  labs_auto <- tabularise:::.labels(data)
  vars <- rownames(attr(res$terms, "factors"))
  labs_auto <- labs_auto[names(labs_auto) %in% vars]
  attr(res, "labs") <- labs_auto

  # Adding a new class if attribute additions have been made
  if (!is.null(attr(res, "labs"))) {
    class(res) <- c("lm_", class(res))
  }

  res
}

#' Summarizing Linear Model Fits by SciViews
#'
#' @description
#' summary method for class lm_
#'
#' @param object an object of class lm_
#' @param ... additional argument to stats:::summary.lm()
#'
#' @return an object of class summary.lm_ object, similar to summary.lm
#' @export
#'
#' @examples
#' #TODO
summary.lm_ <- function(object, ...) {
  res <- stats::summary.lm(object = object, ...)

  if(!is.null(attr(object, "labs"))) {
    attr(res, "labs") <- attr(object, "labs")
  }

  # Adding a new class if attribute additions have been made
  if (is.null(attr(res, "labs"))) {
    class(res) <- c("summary.lm_", class(res), "lm")
  }
  res
}


