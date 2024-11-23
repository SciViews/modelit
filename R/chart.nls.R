#' Chart an nls model or diagnose its residuals visually
#'
#' @description The methods `autoplot()` or `chart()` for **nls** objects. If
#' `type = model` (by default for `chart()`), a scatterplot with the model
#' superimposed is produced,. The other types allow to analyze the residuals of
#' the model.
#'
#' @param data A **nls** model.
#' @param object Idem
#' @param type The type of plot: `"model"`, `"resfitted"`, `"qqplot"`,
#'   `"scalelocation"`, `"reshist"` or `"resautocor"`. For `chart()`, can also
#'   be provided as `chart$type(....)`. `chart()` also uses `"residuals"` that
#'   constructs a combined figure with resfitted, qqplot, scalelocation and
#'   resautocor.
#' @param name The name of the model. If not provided, it is the name of the
#'   model object by default.
#' @param title A title for the plot. If not provided, a default title is
#'   computed.
#' @param xlab A label for the X axis. A default label is proposed if it is not
#'   provided.
#' @param ylab A label for the Y axis (with default if not provided).
#' @param labels A vector of four character strings, one for each plot done with
#' `chart$residuals()`.
#' @param ... Additional arguments passed to the chart.
#' @param lang The language to use for titles and labels, currently only `"en"`
#'   or `"fr"`.
#' @param env The environment to evaluate code. It is `parent.frame()` by
#' default, and there is no reasons to change it, unless you really know what
#' you are doing!
#'
#' @return The ggplot object produced.
#' @export
#'
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)

#' library(chart)
#' chart(chick1_logis)
#' # Residuals analysis
#' chart$resfitted(chick1_logis)
#' chart$qqplot(chick1_logis)
#' chart$scalelocation(chick1_logis)
#' chart$reshist(chick1_logis, bins = 15)
#' chart$resautocor(chick1_logis)
#'
#' # The four most important residual analysis plots in one figure
#' chart$residuals(chick1_logis)
chart.nls <- function(data, type = "model", ..., title, labels = "AUTO",
name = deparse(substitute(data)), lang = getOption("data.io_lang", "en"),
env = parent.frame()) {
  if (type == "residuals") {
    a <- autoplot.nls(data, ..., name = name, type = "resfitted",
      title = "", lang = lang, env = env)
    b <- autoplot.nls(data, ..., name = name, type = "qqplot",
      title = "", lang = lang, env = env)
    c <- autoplot.nls(data, ..., name = name, type = "scalelocation",
      title = "", lang = lang, env = env)
    d <- autoplot.nls(data, ..., name = name, type = "resautocor",
      title = "", lang = lang, env = env)
    combine_charts(list(a, b, c, d), ncol = 2L, nrow = 2L, labels = labels)
  } else {
    if (missing(title)) {
      autoplot.nls(data, ..., name = name, type = type, lang = lang, env = env)
    } else {
      autoplot.nls(data, ..., name = name, title = title, type = type,
        lang = lang, env = env)
    }
  }
}
class(chart.nls) <- c("function", "subsettable_type")

#' @export
#' @rdname chart.nls
#' @importFrom rlang .data
autoplot.nls <- function(object,
type = c("model", "resfitted", "qqplot", "scalelocation", "reshist",
"resautocor"), title, xlab, ylab, ..., name = deparse(substitute(object)),
  lang = getOption("data.io_lang", "en"), env = parent.frame()) {
  # TODO: chart style if invoked via chart(), otherwise default ggplot theme

  # Needed to avoid spurious R CMD check errors
  . <- NULL
  .data <- NULL
  .std.resid <- NULL

  lang <- tolower(lang)
  if (lang != "fr") lang <- "en" # Only en or fr for now

  type = match.arg(type)

  if (lang == "fr") {
    cur_type_title <- type_title_fr[type]
    labs <- labs_fr
    labs_sqrtresid <- expression(bold(sqrt(abs("R\u00e9sidus standardis\u00e9s"))))
  } else {
    cur_type_title <- type_title_en[type]
    labs <- labs_en
    labs_sqrtresid <- expression(bold(sqrt(abs("Standardized residuals"))))
  }

  if (missing(title))
    title <- paste(cur_type_title, name, sep = " - ")

  res <- switch(type,
    model = object %>.% # scatterplot + model (TODO: + confidence)
      (function(nls) {
        X <- names(nls$dataClasses)
        if (length(X) > 1)
          stop("Only one independent variable is supported for now")
        adata <- augment(nls)
        ndata <- names(adata)
        if (ndata[1] == X) {
          Y <- ndata[2]
        } else {
          Y <- ndata[1]
        }
        chart(data = adata, aes(x = .data[[X]], y = .data[[Y]])) + # Was aes(x = {{ X }}, y = {{ Y }})) + # Was: aes_string(x = X, y = Y)) +
          geom_point() +
          stat_function(fun = as.function(nls), col = "skyblue3", size = 1) +
          labs(x = X, y = Y)
      })(.),

    resfitted = object %>.% #plot(lm., which = 1)
      augment(.) %>.%
      chart(., .resid ~ .fitted) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
        labs(x = labs["fitted"], y = labs["resid"]),

    qqplot = object %>.% #plot(lm., which = 2)
      augment2_nls(.) %>.%
      chart(., aes(sample = .std.resid)) +
      geom_qq() +
      geom_qq_line(colour = "darkgray") +
      labs(x = labs["theoquant"], y = labs["stdresid"]),

    scalelocation = object %>.% #plot(lm., which = 3)
      # Note: in lindia::gg_scalelocation(), X and Y axes are transposed!
      augment2_nls(.) %>.%
      chart(., sqrt(abs(.std.resid)) ~ .fitted) +
        geom_point() +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
        labs(x = labs["fitted"], y = labs_sqrtresid),

    reshist = object %>.% # not in plot() but lindia::gg_reshist()
      augment(.) %>.%
      chart(., ~.resid) +
        geom_histogram(...) +
        labs(x = labs["resid"], y = labs["count"]),

    resautocor = object %>.% # Autocorrelation residuals (i+1th against ith)
      augment(.) %>.%
      add_lag_resid(.) %>.%
      chart(., .lag.resid ~.resid) +
        geom_point(na.rm = TRUE) +
        geom_abline(intercept = 0, slope = 1, col = "gray") +
      labs(x = paste(labs["resid"], "(x)"), y = paste(labs["resid"], "(x+1)")),

    stop("Unrecognized type, must be 'model', 'resfitted', 'qqplot',
      'scalelocation' or 'reshist'")
  )

  # Override default title and axes labels
  res <- res + ggtitle(title)
  if (!missing(xlab))
    res <- res + xlab(xlab)
  if (!missing(ylab))
    res <- res + ylab(ylab)
  res
}

# There is no rstandard() method for nls, but nlstools calculate it this way:
rstandard_nls <- function(x, ...) {
  sigma <- summary(x)$sigma
  resid <- residuals(x)
  (resid - mean(resid)) / sigma
}

augment2_nls <- function(model) {
 data <- augment(model)
 # Add standardized residuals too
  data$.std.resid <- rstandard_nls(model)
  data
}

add_lag_resid <- function(data) {
  data$.lag.resid <- c(data$.resid[-1], NA)
  data
}
