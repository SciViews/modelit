#' Fitting Linear Models with Enhanced Output (Experimental)
#'
#' @description
#' `lm_()` is an **experimental** wrapper around the base [stats::lm()] function.
#' It behaves similarly to `lm()` but enriches the returned object with additional
#' metadata.
#'
#' @param data A `data.frame` containing the variables in the model.
#' @param formula An object of class `formula`: a symbolic description of the model to be fitted.
#' @param ... Additional arguments passed to [stats::lm()].
#'
#' @return An object of class `lm_`, which inherits from `lm`, and includes additional
#' components such as `labels`. If no additional attributes are added, a standard `lm` object is returned.
#'
#' @export
#'
#' @examples
#' data(iris)
#'
#' # Add labels to variables
#' attr(iris$Sepal.Length, "label") <- "Sepal Length (cm)"
#' attr(iris$Petal.Length, "label") <- "Petal Length (cm)"
#'
#' # Fit the model using lm_()
#' res <- lm_(iris, formula = Petal.Length ~ Sepal.Length + Species)
#'
#' res
#' class(res)
#' summary(res)
#'
#' # Access labels
#' res$labels
#'
lm_ <- function(data, formula, ...) {
  res <- stats::lm(data = data, formula = formula, ...)

  # Extract labels
  if (!is.null(res$model)) {
    labs <- .labels3(res)
  } else {
    labs <- .labels3(res, origdata = data)
  }

  res$labels <- labs

  # Add custom class if labels were successfully added
  if (!is.null(res$labels)) {
    class(res) <- c("lm_", class(res))
  }

  res
}


#' Summarizing Linear Model Fits with Enhanced Output
#'
#' @description
#' `summary.lm_()` is a method for objects of class `lm_`, extending the standard
#' [summary.lm()] functionality.
#' It returns a summary object similar to `summary.lm`, but includes additional
#' metadata such as variable labels (if available), making the output more
#' informative for reporting and interpretation.
#'
#' This method is part of the experimental `lm_()` modeling framework.
#'
#' @param object An object of class `lm_`, typically returned by [lm_()].
#' @param ... Additional arguments passed to [stats::summary.lm()].
#'
#' @return An object of class `summary.lm_`, which inherits from `summary.lm` and
#' includes an optional `labels` component if available in the original model.
#'
#' @export
#'
#' @examples
#' data(iris)
#'
#' # Add labels to variables
#' attr(iris$Sepal.Length, "label") <- "Sepal Length (cm)"
#' attr(iris$Petal.Length, "label") <- "Petal Length (cm)"
#' attr(iris$Species, "label") <- "Iris Species"
#'
#' # Fit model using lm_()
#' model <- lm_(iris, formula = Petal.Length ~ Sepal.Length + Species)
#'
#' # Get summary with labels
#' summary_model <- summary(model)
#' summary_model
#'
#' # Access labels
#' summary_model$labels
#'
summary.lm_ <- function(object, ...) {
  res <- stats::summary.lm(object = object, ...)

  # Add labels if available
  if (!is.null(object$labels)) {
    res$labels <- object$labels
  }

  # Add custom class if labels were added
  if (!is.null(res$labels)) {
    class(res) <- c("summary.lm_", class(res))
  }

  res
}

#' ANOVA Tables for Enhanced Linear Models (`lm_`)
#'
#' @description
#' `anova.lm_()` is a method for objects of class `lm_`, extending the standard
#' [stats::anova()] functionality.
#' It returns an ANOVA table similar to the base version but includes additional
#' metadata such as variable labels (if available), making the output more
#' informative for interpretation and reporting.
#'
#' This method is part of the experimental `lm_()` modeling framework.
#'
#' @param object An object of class `lm_`, typically returned by [lm_()].
#' @param ... Additional arguments passed to [stats::anova()].
#'
#' @return An object of class `anova_`, which inherits from `anova` and may include
#' a `labels` component if available in the original model.
#'
#' @export
#'
#' @examples
#' data(iris)
#'
#' # Add labels to variables
#' attr(iris$Sepal.Length, "label") <- "Sepal Length (cm)"
#' attr(iris$Petal.Length, "label") <- "Petal Length (cm)"
#'
#' # Fit model using lm_()
#' model <- lm_(iris, Petal.Length ~ Sepal.Length + Species)
#'
#' # Get ANOVA table with labels
#' anova_model <- anova(model)
#' anova_model
#'
#' # Access labels
#' anova_model$labels
#'
anova.lm_ <- function(object, ...) {
  res <- .anova.lm(object = object, ...)

  # Add labels if available
  if (!is.null(object$labels)) {
    res$labels <- object$labels
  }

  # Add custom class if labels were added
  if (!is.null(res$labels)) {
    class(res) <- c("anova_", class(res))
  }

  res
}

.anova.lm <- getS3method("anova", "lm")
