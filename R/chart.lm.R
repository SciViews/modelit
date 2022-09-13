#' Chart an lm or glm model or diagnose its residuals visually
#'
#' @description The methods `autoplot()` or `chart()` for **lm** or **glm**
#' objects. If `type = model` (by default for `chart()`), a scatterplot with the
#' model superimposed is produced, providing the model has only two numeric
#' variables (or a combination of these). The other types allow to analyze the
#' residuals of the model.
#'
#' @param data A **lm** or **glm** model.
#' @param object Idem
#' @param type The type of plot: `"model"`, `"resfitted"`, `"qqplot"`,
#'   `"scalelocation"`, `"cooksd"`, `"resleverage"`, `"cookleverage"`,
#'   `"reshist"` or `"resautocor"`. For `chart()`, can also be provided as
#'   `chart$type(....)`.  `chart()` also uses `"residuals"` that constructs a
#'   combined figure with resfitted, qqplot, scalelocation and
#'   resleverage.
#' @param origdata The original dataset this model was fitted to. Only required
#'   for `type = model` and in case untransformed X variable is not in the model.
#' @param name The name of the model. If not provided, it is the name of the
#'   model object by default.
#' @param title A title for the plot. If not provided, a default title is
#'   computed.
#' @param xlab A label for the X axis. A default label is proposed if it is not
#'   provided.
#' @param ylab A label for the Y axis (with default if not provided).
#' @param labels A vector of four character strings, one for each plot done with
#' `chart$residuals()`.
#' @param ... Additional arguments passed to the chart.``
#' @param lang The language to use for titles and labels, currently only `"en"`
#'   or `"fr"`.`
#' @param env The environment to evaluate code. It is `parent.frame()` by
#' default, and there is no reasons to change it, unless you really know what
#' you are doing!
#'
#' @return The ggplot object produced.
#' @export
#'
#' @examples
#' library(chart)
#' data(trees, package = "datasets")
#' trees_lm <- lm(Volume ~ Girth, data = trees)
#' chart(trees_lm) # origdata not needed because untransformed variables
#' # Residuals analysis
#' chart$resfitted(trees_lm)
#' chart$qqplot(trees_lm)
#' chart$scalelocation(trees_lm)
#' chart$cooksd(trees_lm)
#' chart$resleverage(trees_lm)
#' chart$cookleverage(trees_lm)
#' chart$reshist(trees_lm, bins = 15)
#' chart$resautocor(trees_lm)
#'
#' # The four most important residual analysis plots in one figure
#' chart$residuals(trees_lm)
#'
#' trees_lm2 <-  lm(Volume ~ log(Girth), data = trees)
#' chart(trees_lm2, origdata = trees) # origdata needed, cf. transformed Girth
#' trees_lm3 <- lm(Volume ~ Girth + Height, data = trees)
#' # chart(trees_lm3) # Error because more than 2 variables!
#' # Polynomial regressions work too
#' trees_lm4 <- lm(Volume ~ I(Girth^2) + Girth, data = trees)
#' chart(trees_lm4)
#' # or using poly()
#' trees_lm5 <- lm(Volume ~ poly(Girth, 3), data = trees)
#' chart(trees_lm5, origdata = trees) # origdata required here!
chart.lm <- function(data, type = "model", ..., origdata = NULL, title,
labels = "AUTO", name = deparse(substitute(data)),
lang = getOption("data.io_lang", "en"), env = parent.frame()) {
  if (type == "residuals") {
    a <- autoplot.lm(data, ..., name = name, type = "resfitted",
      title = "", lang = lang, env = env)
    b <- autoplot.lm(data, ..., name = name, type = "qqplot",
      title = "", lang = lang, env = env)
    c <- autoplot.lm(data, ..., name = name, type = "scalelocation",
      title = "", lang = lang, env = env)
    d <- autoplot.lm(data, ..., name = name, type = "resleverage",
      title = "", lang = lang, env = env) +
      theme(legend.position = "none")
    combine_charts(list(a, b, c, d), ncol = 2L, nrow = 2L, labels = labels)
  } else {
    if (missing(title)) {
      autoplot.lm(data, ..., origdata = origdata, name = name,
        type = type, lang = lang, env = env)
    } else {
      autoplot.lm(data, ..., origdata = origdata, name = name,
        title = title, type = type, lang = lang, env = env)
    }
  }
}
class(chart.lm) <- c("function", "subsettable_type")

#' @export
#' @rdname chart.lm
autoplot.lm <- function(object, origdata = NULL,
type = c("model", "resfitted", "qqplot", "scalelocation", "cooksd",
"resleverage", "cookleverage", "reshist", "resautocor"), title, xlab, ylab,
..., name = deparse(substitute(object)),
lang = getOption("data.io_lang", "en"), env = parent.frame()) {
  # TODO: chart style if invoked via chart(), otherwise default ggplot theme
  # TODO: lindia::resx() but several plots for more complex formulas and one
  # could question the validity of this plot as the distrubition of residuals
  # is linked to Y (and fitted Y), not to X!!!
  # TODO: lindia::boxcox() but requires a dependency to MASS::boxcox() or to
  # lindia itself!

  # Needed to avoir spurious R CMD check errors
  . <- NULL
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
    model = object %>.% # scatterplot + model + confidence
      (function(lm) {
        model <- lm$model
        if (!is.null(origdata)) {
          # In case Y is transformed, we need this too (1st var in model)
          if (names(model)[1] %in% names(origdata)) {
            model <- origdata
          } else {# Also keep the transformed Y
            model <- cbind(origdata, model[, 1])
          }
        }
        variables = names(model)
        terms <- lm$terms
        if (length(terms) != 3)
          stop("You cannot use one-sided formula, like ~x.")
        formula <- get_formula_x_y(terms)
        vars_x_y <- attr(formula, "terms") # Y first, then X
        # No because stat_smooth() cannot transform Y, so use the already
        # transformed version instead
        #Y <- vars_x_y[1]
        Y <- names(lm$model)[1]
        X <- vars_x_y[2]
        if (length(vars_x_y) != 2)
          stop("Impossible to plot in 2D a model with more than 2 variables")
        formula <- replace_formula_x_y(terms, x = X, y = Y)
        # Only works for a model involving numeric variables (no ANOVA yet)
        # But poly() creates nmatrix.2, nmatrix.3, ... objects, so:
        classes7 <- substring(attr(terms, "dataClasses"), 1L, 7L)
        if (!all(classes7 %in% c("numeric", "nmatrix")))
          stop("Can only plot a model involving numeric variables for now.")
        # The X variable must appear untransformed somewhere in the formula
        # or origdata is required
        if (!X %in% variables)
          stop("Independent variable '", X, "' must appear untransformed in the model's formula, or you must supply the original dataset in origdata=.")
        # It seems stat_smooth() does not support transforming Y, so, change for
        # Y being its transformed version in the plot
        formula[[2]] <- as.name("y")
        attributes(formula) <- NULL
        chart(data = model, aes_string(x = X, y = Y)) +
          geom_point() +
          stat_smooth(method = "lm", formula = formula)
        })(.),

    resfitted = object %>.% #plot(lm., which = 1)
      augment(.) %>.%
      chart(., .resid ~ .fitted) +
      geom_point() +
      geom_hline(yintercept = 0) +
      geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
      labs(x = labs["fitted"], y = labs["resid"]),

    qqplot = object %>.% #plot(lm., which = 2)
      augment(.) %>.%
      chart(., aes(sample = .std.resid)) +
      geom_qq() +
      geom_qq_line(colour = "darkgray") +
      labs(x = labs["theoquant"], y = labs["stdresid"]),

    scalelocation = object %>.% #plot(lm., which = 3)
      # Note: in lindia::gg_scalelocation(), X and Y axes are transposed!
      augment(.) %>.%
      chart(., sqrt(abs(.std.resid)) ~ .fitted) +
      geom_point() +
      geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
      labs(x = labs["fitted"], y = labs_sqrtresid),

    cooksd = object %>.% #plot(lm., which = 4)
      augment(.) %>.%
      chart(., .cooksd ~ seq_along(.cooksd)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = seq(0, 0.1, by = 0.05), colour = "darkgray") +
      labs(x = labs["obsnbr"], y = labs["cooksd"]),

    resleverage = object %>.% #plot(lm., which = 5)
      augment(.) %>.%
      chart(., .std.resid ~ .hat %size=% .cooksd) +
      geom_point() +
      geom_smooth(se = FALSE, size = 0.5, method = "loess", formula = y ~ x) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(x = labs["lev"], y = labs["stdresid"]),

    cookleverage = object %>.% #plot(lm., which = 6)
      augment(.) %>.%
      chart(., .cooksd ~ .hat %size=% .cooksd) +
      geom_point() +
      geom_vline(xintercept = 0, colour = NA) +
      geom_abline(slope = seq(0, 3, by = 0.5), colour = "darkgray") +
      geom_smooth(se = FALSE, size = 0.5, method = "loess", formula = y ~ x) +
      labs(x = labs["lev"], y = labs["cooksd"]),

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

    stop("Unrecognized type, must be 'model', 'resfitted', 'qqplot', 'scalelocation', 'cooksd', 'resleverage', 'cookleverage', 'reshist', or 'resautocor'")
  )

  # Override default title and axes labels
  if (!missing(title))
    res <- res + ggtitle(title)
  if (!missing(xlab))
    res <- res + xlab(xlab)
  if (!missing(ylab))
    res <- res + ylab(ylab)
  res
}

# Get the name of x and y variables in a formula
get_formula_x_y <- function(f) {
  terms <- character(0)
  for (i in 2:length(f)) {# Note f[[1]] is always a function
    if (is.call(f[[i]])) {
      fi <- get_formula_x_y(f[[i]])
      terms <- c(terms, attr(fi, "terms"))
      attr(fi, "terms") <- NULL
      f[[i]] <- fi
    }
    if (is.name(f[[i]]))
      terms <- c(terms, as.character(f[[i]]))
  }
  attr(f, "terms") <- unique(terms)
  f
}

# Replace x and y recursively in a formula
replace_formula_x_y <- function(f, x, y) {
  X <- as.name("x")
  Y <- as.name("y")
  for (i in 2:length(f)) {# Note f[[1]] is always a function
    if (is.call(f[[i]]))
      f[[i]] <- replace_formula_x_y(f[[i]], x, y)
    if (f[[i]] == x) f[[i]] <- X
    if (f[[i]] == y) f[[i]] <- Y
  }
  f
}

type_title_en <- c(
  model         = "Model",
  resfitted     = "Residuals distribution",
  qqplot        = "Residuals quantite-quantile plot",
  scalelocation = "Residuals scale location",
  cooksd        = "Cook's distance of the residuals",
  resleverage   = "Residuals leverage",
  cookleverage  = "Leverage vs Cook's distance",
  reshist       = "Residuals histogram",
  resautocor    = "Residuals autocorrelation")
type_title_fr <- c(
  model         = "Mod\u00e8le",
  resfitted     = "Distribution des r\u00e9sidus",
  qqplot        = "Graphique quantile-quantile des r\u00e9sidus",
  scalelocation = "Position et \u00e9chelle des r\u00e9sidus",
  cooksd        = "Distance de Cook des r\u00e9sidus",
  resleverage   = "Effet levier des r\u00e9sidus",
  cookleverage  = "Effet levier vs distance de Cook",
  reshist       = "Histogramme des r\u00e9sidus",
  resautocor    = "Autocorr\u00e9lation des r\u00e9sidus")

labs_en <- c(
  fitted    = "Fitted values",
  resid     = "Residuals",
  stdresid  = "Standardized residuals",
  theoquant = "Theoretical quantiles",
  count     = "Count",
  obsnbr    = "Observation number",
  cooksd    = "Cook's distance",
  lev       = "Leverage")
labs_fr <- c(
  fitted    = "Valeurs pr\u00e9dites",
  resid     = "R\u00e9sidus",
  stdresid  = "R\u00e9sidus standardis\u00e9s",
  theoquant = "Quantiles th\u00e9oriques",
  count     = "Effectif",
  obsnbr    = "Num\u00e9ro des observation",
  cooksd    = "Distance de Cook",
  lev       = "Effet levier")


