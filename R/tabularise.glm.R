# when we calculate a glm with the glm function, we obtain an object of
# type `glm` and `lm`. So `tabularise()` using the `lm` method
# The equatiomatic package is not capable of extracting an equation from an
# object of type summary.glm

#' @title Create a rich-formatted table using the coefficients of a glm object
#'
#' @description
#' Extract and format the table of coefficients from a **glm** object, similar
#' to [stats::coef()], but in a rich-formatted **flextable** object.
#'
#' @param data A **glm** object
#' @param header Logical. If `TRUE` (default), a header is added to the table.
#'   The header includes both the title and the equation (if applicable).
#'   If set to `FALSE`, neither the title nor the equation will be displayed in
#'   the table header, even if the `title` or `equation` parameters are provided.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE`: The equation is generated and added to the table header. Its
#'              parameters are also used in the "Term" column.
#'   - `FALSE` (by default): No equation is generated or displayed, and its
#'              parameters are not used in the "Term" column.
#'   - `NA`: The equation is generated but not displayed in the table header.
#'              Its parameters are used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'              the table header.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param footer If `TRUE` (`FALSE` by default), add a footer to the table.
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange it with the
#'   \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @method tabularise_coef glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$coef(iris_glm)
#'
#' # If the 'iris' dataset has labels and units, they can be used to enhance
#' # the output table
#' iris <- data.io::labelise(iris, self = FALSE, label = list(
#'     Sepal.Length = "Length of the sepals",
#'     Petal.Length = "Length of the petals",
#'     Species = "Species"), units = c(rep("cm", 4), NA))
#'
#' iris_glm1 <- glm(data = iris, Petal.Length ~ Sepal.Length + Species)
#' tabularise::tabularise$coef(iris_glm1)
#'
tabularise_coef.glm <- function(data, header = FALSE, title = header,
equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL, footer = FALSE,
lang = getOption("data.io_lang", default = Sys.getenv("LANGUAGE",unset = "en")),
..., kind = "ft", env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "coef", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = FALSE, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = footer, ...)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table from a glm object
#'
#' @param data A **glm** object
#' @param header Logical. If `TRUE` (`FALSE` by default), a header is added to the table.
#'   The header includes both the title and the equation (if applicable).
#'   If set to `FALSE`, neither the title nor the equation will be displayed in
#'   the table header, even if the `title` or `equation` parameters are provided.
#' @param title If `TRUE` (`FALSE` by default), add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE`: The equation is generated and added to the table header. Its
#'              parameters are also used in the "Term" column.
#'   - `FALSE` (by default): No equation is generated or displayed, and its
#'              parameters are not used in the "Term" column.
#'   - `NA`: The equation is generated but not displayed in the table header.
#'              Its parameters are used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'              the table header.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param footer If `TRUE` (`FALSE` by default), add a footer to the table
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return  A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange it with the
#'   \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise(iris_glm)
#' tabularise::tabularise(iris_glm, header = TRUE, footer = TRUE)
#' tabularise::tabularise(iris_glm, header = TRUE, footer = FALSE)
#' tabularise::tabularise(iris_glm, header = TRUE, equation = NA,footer = TRUE)
tabularise_default.glm <- function(data, header = FALSE, title = header,
      equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
      footer = FALSE, lang = getOption("data.io_lang",
        default = Sys.getenv("LANGUAGE",unset = "en")), ..., kind = "ft", env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "coef", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = FALSE, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = footer, ..., env = env)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

#' Create a tidy version of the glm object as a rich-formatted table
#'
#' @description
#' Turn the tidy of **glm** object into a rich-formatted table with
#' \{flextable\}. The table can be printed in different formats (HTML, LaTeX,
#' Word, PowerPoint), or rearranged later on.
#'
#' @param data A **glm** object
#' @param header Logical. If `TRUE` (`TRUE` by default), a header is added to the table.
#'   The header includes both the title and the equation (if applicable).
#'   If set to `FALSE`, neither the title nor the equation will be displayed in
#'   the table header, even if the `title` or `equation` parameters are provided.
#' @param title If `TRUE` (`TRUE` by default), add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE` (default) : The equation is generated and added to the table
#'            header. Its parameters are also used in the "Term" column.
#'   - `FALSE`: No equation is generated or displayed, and its parameters are
#'            not used in the "Term" column.
#'   - `NA`: The equation is generated but not displayed in the table header.
#'            Its parameters are used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'            the table header.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param conf.int If `TRUE`, add the confidence interval. The default is
#'   `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if
#'   `conf.int = TRUE`. The default is 0.95.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return  A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint), or rearrange it with the
#'   \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy glm
#' @examples
#' #' # If the 'iris' dataset has labels and units, they can be used to enhance
#' # the output table
#' iris <- data.io::labelise(iris, self = FALSE, label = list(
#'     Sepal.Length = "Length of the sepals",
#'     Petal.Length = "Length of the petals",
#'     Species = "Species"), units = c(rep("cm", 4), NA))
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#'
#' tabularise::tabularise$tidy(iris_glm)
#' tabularise::tabularise$tidy(iris_glm, conf.int = TRUE)
#' tabularise::tabularise$tidy(iris_glm, conf.int = TRUE, equation = NA)
tabularise_tidy.glm <- function(data, header = TRUE, title = NULL,
    equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
    conf.int = FALSE, conf.level = 0.95,
  lang = getOption("data.io_lang", default = Sys.getenv("LANGUAGE",unset = "en")),
  show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft",
  env = parent.frame()) {
  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "tidy", conf.int = conf.int, conf.level = 0.95,
    show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = FALSE, ..., env = env)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

#' Create a glance version of the glm object as a rich-formatted table
#'
#' @description
#' Turn the glance of **glm** object into a rich-formatted table with
#' \{flextable\}. The table can be printed in different formats (HTML, LaTeX,
#' Word, PowerPoint), or rearranged later on.
#'
#' @param data A **glm** object
#' @param header Logical. If `TRUE` (`TRUE` by default), a header is added to the table.
#'   The header includes both the title and the equation (if applicable).
#'   If set to `FALSE`, neither the title nor the equation will be displayed in
#'   the table header, even if the `title` or `equation` parameters are provided.
#' @param title If `TRUE` (`TRUE` by default), add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE` (default) : The equation is generated and added to the table
#'            header. Its parameters are also used in the "Term" column.
#'   - `FALSE`: No equation is generated or displayed, and its parameters are
#'            not used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'            the table header.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object is produced that you can print in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange with the \{flextable\}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$glance(iris_glm)
#' tabularise::tabularise$glance(iris_glm, equation = FALSE)
#' tabularise::tabularise$glance(iris_glm, equation = "my personal equation")
#'
tabularise_glance.glm <- function(data, header = TRUE, title = header,
    equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
    lang = getOption("data.io_lang", "en"), ..., kind = "ft",
    env = parent.frame()) {
  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "glance", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = FALSE, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = FALSE, ..., env = env)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table using the table of coefficients of the summary.glm object
#'
#' @description
#' Create a rich-formatted \{flextable\} object with the table of coefficients
#' from the [summary()] of a **glm** object.
#'
#' @param data A **summary.glm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), try to add a equation to the table
#'   header. The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return  A **flextable** object that you can print in different formats
#'   (HTML, LaTeX, Word, PowerPoint) or rearrange with the \{flextable\}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_glm_sum <- summary(iris_glm)
#' tabularise::tabularise_coef(iris_glm_sum)
tabularise_coef.summary.glm <- function(data, header = TRUE, title = NULL,
  equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
  lang = getOption("data.io_lang", default = Sys.getenv("LANGUAGE",unset = "en")),
  show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft",
  env = parent.frame()) {
  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "coef", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = FALSE, ..., env = env)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table from a summary.glm object
#'
#' @description
#' Create a rich-formatted table version of the [summary()] of a **glm** object.
#'
#' @param data A **summary.glm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), try to add a equation to the table
#'   header. The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param footer If `TRUE` (by default), add a footer to the table
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object that you can print in different form or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.glm
#'
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_glm_sum <- summary(iris_glm)
#' tabularise::tabularise(iris_glm_sum)
tabularise_default.summary.glm <- function(data, header = TRUE, title = NULL,
  equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
  lang = getOption("data.io_lang", default = Sys.getenv("LANGUAGE",unset = "en")),
  show.signif.stars = getOption("show.signif.stars", TRUE), footer = TRUE,
   ..., kind = "ft", env = parent.frame()) {
  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_glm(
    data, type = "coef", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_glm, footer = footer, ..., env = env)

  # formatted table ----
  format_table(df_list, kind = kind, header = header)
}

# A list of internals functions ------

colnames_glm <- c(
  term = "Term",
  estimate = "Estimate",
  conf.low = "Lower bound (CI)",
  conf.high = "Upper bound (CI)",
  std.error = "Standard Error",
  t.value = "*t* value",
  sigma = "Sigma",# The misnomer “Residual standard error”
  r.squared = "R^2^",
  adj.r.squared = "Adj.R^2^",
  AIC = "AIC",
  BIC = "BIC",
  statistic = "*t* value",
  statistic2 = "*z* value",
  p.value = "*p* value",
  deviance = "Deviance",
  logLik = "Log-Likelihood",
  null.deviance = "Total deviance",
  df.null = "Total df",
  df = "Num. df",
  df.residual = "Residuals df",
  nobs = "N",
  signif = "",
  "(Intercept)" = "Intercept")

.trads <- gettext(term = "Term",
                  estimate = "Estimate",
                  conf.low = "Lower bound (CI)",
                  conf.high = "Upper bound (CI)",
                  std.error = "Standard Error",
                  t.value = "*t* value",
                  sigma = "Sigma",# The misnomer “Residual standard error”
                  r.squared = "R^2^",
                  adj.r.squared = "Adj.R^2^",
                  AIC = "AIC",
                  BIC = "BIC",
                  statistic = "*t* value",
                  statistic2 = "*z* value",
                  p.value = "*p* value",
                  deviance = "Deviance",
                  logLik = "Log-Likelihood",
                  null.deviance = "Total deviance",
                  df.null = "Total df",
                  df = "Num. df",
                  df.residual = "Residuals df",
                  nobs = "N",
                  "(Intercept)" = "Intercept",
                  "header" = "Generalized Linear Model",
                  lang = "fr")

# See utils.R for internal functions used by various .extract_infos_***

.extract_footer_glm <- function(data, lang = "en") {
  digits <- max(3L, getOption("digits") - 3L)
  domain <- "R-modelit"

  if (inherits(data, "summary.glm")) {
    footer_glm <- c(gaussian = "Gaussian family", binomial = "Binomial family",
        Gamma = "Gamma family", inverse.gaussian = "Inverse Gaussian family",
        poisson = "Poisson family",
        quasi = "Quasi-Gaussian family",
        quasibinomial = "Quasi-Binomial family",
        quasipoisson = "Quasi-Poisson family")
    family_glm <- gettext(footer_glm[data$family$family], lang = lang)

    res <- paste(
      gettextf("(Dispersion parameter for %s: %.*g)", family_glm, digits,
               data$dispersion, domain = domain, lang = lang),
      gettextf("Total deviance: %.*g on %.*g degrees of freedom",digits, data$null.deviance, digits, data$df.null, domain = domain, lang = lang),
      gettextf("Residual deviance: %.*g on %.*g degrees of freedom",digits, data$deviance, digits, max(data$df), domain = domain, lang = lang),
      gettextf("AIC: %.*g - Number of Fisher Scoring iterations: %.*g",digits, data$aic, digits, max(data$iter), domain = domain, lang = lang),
      sep = "\n")
    res
  }
  else {
    res <- paste(
      gettextf("Degrees of Freedom: %.*g Total (i.e. no model); %.*g Residual", digits, data$df.null, digits,
               data$df.residual, domain = domain, lang = lang),
      gettextf("Total deviance: %.*g", digits, data$null.deviance, domain = domain, lang = lang),
      gettextf("Residual deviance: %.*g AIC: %.*g",digits, data$deviance, digits, data$ai, domain = domain, lang = lang),
      sep = "\n")
    res
  }
}

.extract_infos_glm <- function(data, type = "coef", conf.int = FALSE,
    conf.level = 0.95, show.signif.stars = TRUE, lang = "en",
    colnames = colnames_glm, auto.labs = TRUE, origdata = NULL, labs = NULL,
    equation = TRUE, title = TRUE, footer = TRUE, env = parent.frame()) {

  if (!inherits(data, c("glm", "summary.glm")))
    stop(".extract_infos_glm() can apply only glm and summary.glm object.")

  type <- match.arg(type, choices = c("coef", "glance", "tidy"))

  if (inherits(data, "summary.glm") && type != "coef") {
    #TODO: Implement support for type = "glance" and type = "coef"
    message(".extract_infos_glm() can only apply type = 'coef' to a summary.glm
            object.")
    #type <- "tidy"
  }

  if(inherits(data, "summary.glm")) {
    s <- data$coefficients
    df <- data.frame(term = rownames(s), s)
    colnames(df) <- c("term", "estimate", "std.error", "statistic",
                       "p.value")

    if (any(colnames(s) %in% "z value"))
      colnames(df)[colnames(df) == "statistic"] <- "statistic2"

    rownames(df) <- df$term
  } else {
    df <- switch(type,
          coef = {df <- coef(data)
                  df <- data.frame(term = names(df), estimate = df)},
          glance = {df <- as.data.frame(broom::glance(x = data))
                 rownames(df) <- df$term
                 df},
          tidy = {df <- as.data.frame(broom::tidy(x = data, conf.int = conf.int,
                                                         conf.level = conf.level))
                 rownames(df) <- df$term

                 s <- colnames(coef(summary(data)))
                 if (any(s %in% "z value")) {
                   colnames(df)[colnames(df) == "statistic"] <- "statistic2"
                 }

                 if (isTRUE(conf.int)) {
                   df <- df[, c("term", "estimate", "conf.low", "conf.high",
                                "std.error", "statistic", "p.value")]
                 }

                 if (isTRUE(show.signif.stars)) {
                   df$signif <- .pvalue_format(df$p.value)
                 }
                 df
                 }
          )
  }

  if(isTRUE(show.signif.stars)) {
    psignif <- "0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"
  } else {
    psignif <- NULL
  }

  lang <- tolower(lang)
  cols <- .extract_colnames(df, labs = colnames_glm, lang = lang)

  data_obj <- attr(data, "object")

  if (is.null(data_obj)) {

    labels <- .extract_labels(df = df, data = data, auto.labs = auto.labs,
                              origdata = origdata, labs = labs)

    equa <- .extract_equation(data, equation = equation, labs = labels)

    } else {

    labels <- .extract_labels(df = df, data = data_obj, auto.labs = auto.labs,
                              origdata = origdata, labs = labs)

    equa <- .extract_equation(data_obj, equation = equation, labs = labels)
  }

  if ((isTRUE(equation) || is.na(equation)) && !is.null(equa))  {
    terms <- .params_equa(equa)
  } else {
    terms <- .extract_terms(df, labs = labels, lang = lang)
  }

  if (is.na(equation)) {
    equa <- NULL
  }

  title <- .extract_title(title, lang, default = "Generalized Linear Model")

  # footer
  if (isTRUE(footer)) {
    footer <- .extract_footer_glm(data, lang)
  } else {
    footer <- NULL
  }

  list(
    df = df,
    title = title,
    cols = cols,
    equa = equa,
    terms = terms,
    psignif = psignif,
    footer = footer
  )

}
