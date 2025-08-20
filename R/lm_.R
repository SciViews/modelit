#' Fitting Linear Models with Enhanced Output (Experimental)
#'
#' @description
#' `lm_()` is an **experimental** wrapper around the base [stats::lm()] function.
#' It behaves similarly to `lm()`, but enriches the returned object with additional metadata.
#' The order of the arguments differs from `lm()`, and the function uses evaluation
#' through [svMisc::eval_data_dot()] to support flexible data referencing.
#'
#' @param data A `data.frame` containing the variables in the model.
#' @param formula An object of class `formula`: a symbolic description of the model to be fitted.
#' @param ... Additional arguments passed to [stats::lm()].
#' @param .data an alias for the `data` argument
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
lm_ <- function(data = (.), formula, ..., .data = data) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(data) || !is.data.frame(data))
    return(svMisc::eval_data_dot(sys.call(), arg = 'data', abort_msg =
                           gettext("`data` must be a `data.frame`.")))

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
    attr(res, "labels") <- object$labels
  }

  # Add custom class if labels were added
  if (!is.null(res$labels)) {
    class(res) <- c("anova_", class(res))
  }
  res
}

.anova.lm <- getS3method("anova", "lm")

#' Fitting Generalized Linear Models with Enhanced Output (Experimental)
#'
#' @description
#' `glm_()` is an **experimental** wrapper around the base [stats::glm()]
#' function. It behaves similarly to `glm()`, but enriches the returned object
#' with additional metadata. The order of the arguments differs from `glm()`,
#' and the function uses evaluation through [svMisc::eval_data_dot()] to support
#' flexible data referencing.
#'
#' @param data A `data.frame` containing the variables in the model.
#' @param formula An object of class `formula`: a symbolic description of the
#'                model to be fitted.
#' @param ... Additional arguments passed to [stats::glm()].
#' @param .data an alias for the `data` argument
#'
#' @return An object of class `glm_`, which inherits from `glm`, and includes
#' additional components such as `labels`. If no additional attributes are
#' added, a standard `glm` object is returned.
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
#' res <- glm_(iris, formula = Petal.Length ~ Sepal.Length + Species)
#'
#' res
#' class(res)
#' summary(res)
#'
#' # Access labels
#' res$labels
#'
glm_ <- function(data = (.), formula, ..., .data = data) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(data) || !is.data.frame(data))
    return(svMisc::eval_data_dot(sys.call(), arg = 'data', abort_msg =
                                   gettext("`data` must be a `data.frame`.")))

  res <- stats::glm(data = data, formula = formula, ...)

  # Extract labels
  if (!is.null(res$model)) {
    labs <- .labels3(res)
  } else {
    labs <- .labels3(res, origdata = data)
  }

  res$labels <- labs

  # Add custom class if labels were successfully added
  if (!is.null(res$labels)) {
    class(res) <- c("glm_", class(res))
  }

  res
}


#' Fitting Non Linear Models with Enhanced Output (Experimental)
#'
#' @description
#' `nls_()` is an **experimental** wrapper around the base [stats::nls()]
#' function. It behaves similarly to `glm()`, but enriches the returned object
#' with additional metadata. The order of the arguments differs from `glm()`,
#' and the function uses evaluation through [svMisc::eval_data_dot()] to support
#' flexible data referencing.
#'
#' @param data A `data.frame` containing the variables in the model.
#' @param formula An object of class `formula`: a symbolic description of the
#'                model to be fitted.
#' @param model logical. If true, the model frame is returned as part of the
#'              object. Default is FALSE.
#' @param ... Additional arguments passed to [stats::nls()].
#' @param .data an alias for the `data` argument
#'
#' @return An object of class `nls_`, which inherits from `nls`, and includes
#' additional components such as `labels`. If no additional attributes are
#' added, a standard `nls` object is returned.
#'
#' @export
#'
#' @examples
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Add labels to variables
#' attr(chick1$weight, "label") <- "Body weight [gm]"
#' attr(chick1$Time, "label") <- "Number of days"
#'
#' chick1_nls <- nls_(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#'
#'
#' chick1_nls
#' class(chick1_nls)
#' summary(chick1_nls)
#'
#' # Access labels
#' chick1_nls
#'
nls_ <- function(data = (.), formula, model = TRUE, ..., .data = data) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(data) || !is.data.frame(data))
    return(svMisc::eval_data_dot(sys.call(), arg = 'data', abort_msg =
                                   gettext("`data` must be a `data.frame`.")))

  res <- stats::nls(data = data, formula = formula, model = model, ...)

  # Extract labels
  if (!is.null(res$model)) {
    labs <- .labels3(res)
  } else {
    labs <- .labels3(res, origdata = data)
  }

  res$labels <- labs

  # Add custom class if labels were successfully added
  if (!is.null(res$labels)) {
    class(res) <- c("nls_", class(res))
  }

  res
}

#' Object summaries with the original object as an attribute
#'
#' @description
#' This function attempts to summarize an object using the standard [base::summary()]
#' function. The original object is attached as an attribute to the result for
#' reference.
#'
#' @param object 	An object to be summarized.
#' @param ...  Additional arguments passed to the `summary()` function.
#'
#' @return A summary object with an additional `"object"` attribute containing
#'  the original input.
#' @export
#'
#' @examples
#'
#' is_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' summary(is_lm)
#'
#' summary_(is_lm)
#' attr(summary_(is_lm), "object")
#'
summary_ <- function(object, ...) {
  res <- try(summary(object, ...), silent = TRUE)
  if (inherits(res, "try-error")) {
    warning("Unable to summarize the object.")
    return(NULL)
  }
  attr(res, "object") <- object
  res
}

#' ANOVA tables with the original object as an attribute
#'
#' @description
#' This function attempts to compute anova or deviance tables using the
#' standard [stats::anova()] function. The original object is attached as an
#' attribute to the result for reference.
#'
#' @param object An object for which anova or deviance tables should be computed.
#' @param ... Additional arguments passed to the `anova()` function.
#'
#' @return An anova object with an additional `"object"` attribute containing
#'   the original input.
#' @export
#'
#' @examples
#' is_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' anova(is_lm)
#'
#' anova_(is_lm)
#' attr(anova_(is_lm), "object")

anova_ <- function(object, ...) {
  res <- try(anova(object, ...), silent = TRUE)
  if (inherits(res, "try-error")) {
    warning("Unable to compute ANOVA for the object.")
    return(NULL)
  }
  attr(res, "object") <- object
  res
}



