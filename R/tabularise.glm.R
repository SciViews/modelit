# when we calculate a glm with the glm function, we obtain an object of
# type `glm` and `lm`. So tabularise() using the `lm` method
# The equatiomatic package is not capable of extracting an equation from an
# object of type summary.glm

#' @title Create a rich-formatted table using the coefficients of a glm object
#'
#' @description
#' Extract and format the table of coefficients from a **glm** object, similar
#' to [stats::coef()], but in a rich-formatted **flextable** object.
#'
#' @param data A **glm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add an equation to the table header.
#'   The equation can also be passed in the form of a character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and labels of the original data set are not used.
#' @param labs Labels to change the names of elements in the `term` column of
#'    the table. By default, it is `NULL` and no change is performed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments (not used yet).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange it with the
#'   {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @method tabularise_coef glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$coef(iris_glm)
tabularise_coef.glm <- function(data, header = TRUE, title = NULL,
equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
lang = getOption("data.io_lang", "en"), ..., env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  info_lang <- .infos_lang.glm(lang = lang)

  # Extract coef
  co <- coef(data)
  co <- data.frame(term = names(co), estimate = co)
  # co <- as.data.frame(rbind(coef(data)))

  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Create the flextable object
  ft <- flextable(co) |>
    colformat_sci()

  ft <- .header_labels(ft, info_lang = info_lang)

  # Header and equation
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equation(data, swap_var_names =  labs, ...)
    } else {
      equa <- equation(data, ...)
    }

    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      title = title, equation = equa)
  } else {
    equa <- NULL
    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      title = title, equation = equation)
  }

  if (isTRUE(auto.labs) && any(co$term %in% "(Intercept)")) {
    ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
      value = as_paragraph(info_lang[["(Intercept)"]]))
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% co$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = "term",
        value = para_md(labs_red[i]), part = "body")
  }

  if (isTRUE(equation) & !is.null(equa)) {
    params <- .params_equa(equa,...)
    if (length(params) == length(co$term))
      ft <- mk_par(ft, j = "term", value = para_md(params), part = "body")
  }

  autofit(ft, part = c("header", "body"))
}

#' Create a rich-formatted table from a glm object
#'
#' @param data A **glm** object
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param footer If `TRUE` (by default), add a footer to the table
#' @param ... Additional arguments passed to [modelit::tabularise_coef.glm()]
#'
#' @return  A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange it with the
#'   {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise(iris_glm)
tabularise_default.glm <- function(data, lang = getOption("data.io_lang", "en"),
footer = TRUE, ...) {
  ft <- tabularise_coef.glm(data = data, ...)

  if (isTRUE(footer)) {
    info_lang <- .infos_lang.glm(lang = lang)

    digits <- max(3L, getOption("digits") - 3L)
    footer <- info_lang[["footer"]]
    vals <- c(
      paste(footer[["df"]], data$df.null, footer[["total"]], data$df.residual,
        footer[["residual"]]),
      paste(footer[["null.deviance"]],
        format(signif(data$null.deviance, digits))),
      paste(footer[["resid.deviance"]], format(signif(data$deviance,
        digits)), footer[["AIC"]], format(signif(data$aic, digits)))
    )
    ft <- add_footer_lines(ft, values = vals)
  }

  autofit(ft, part = c("header", "body"))
}

#' Create a tidy version of the glm object as a rich-formatted table
#'
#' @description
#' Turn the tidy of **glm** object into a rich-formatted table with
#' {flextable}. The table can be printed in different formats (HTML, LaTeX,
#' Word, PowerPoint), or rearranged later on.
#'
#' @param data A **glm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add an equation to the table header.
#'   The equation can also be passed in the form of a character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from `origdata=`. `
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and variables labels from this data set are not used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param conf.int If `TRUE`, add the confidence interval. The default is
#'   `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if
#'   `conf.int = TRUE`. The default is 0.95.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'  Its value is obtained from `getOption("show.signif.stars")`.
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return  A **flextable** object is returned. You can print it in different
#'   formats (HTML, LaTeX, Word, PowerPoint), or rearrange it with the
#'   {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$tidy(iris_glm)
tabularise_tidy.glm <- function(data, header = TRUE, title = NULL,
equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
conf.int = FALSE, conf.level = 0.95, lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE), ...,
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  info_lang <- .infos_lang.glm(lang = lang)

  # Extract labels of data or origdata
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(x = data, conf.int = conf.int,
    conf.level = conf.level))
  rownames(data_t) <- data_t$term

  if (isTRUE(conf.int)) {
    data_t <- data_t[, c("term", "estimate", "conf.low", "conf.high",
      "std.error", "statistic", "p.value")]
  }

  s <- colnames(coef(summary(data)))
  if (any(s %in% "z value"))
    colnames(data_t)[colnames(data_t) == "statistic"] <- "statistic2"

  if (isTRUE(show.signif.stars)) {
    ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
  } else {
    ft <- flextable(data_t)
  }
  ft <- colformat_sci(ft)
  ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- .header_labels(ft, info_lang = info_lang)

  # headers
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equation(data, swap_var_names =  labs, ...)
    } else {
      equa <- equation(data, ...)
    }

    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      title = title, equation = equa)
  } else {
    equa <- NULL
    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      title = title, equation = equation)
  }

  if (isTRUE(auto.labs) && any(data_t$term %in% "(Intercept)")) {
    ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
      value = as_paragraph(info_lang[["(Intercept)"]]))
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  if (isTRUE(equation) && !is.null(equa)) {
    params <- .params_equa(equa)
    if (length(params) == length(data_t$term))
      ft <- mk_par(ft, j = "term", value = para_md(params), part = "body")
  }

  # Add information on the p.value
  if (ncol_keys(ft) > ncol(data_t))
    ft <- .add_signif_stars(ft, j = "signif")

  ft <- autofit(ft, part = c("header", "body"))

  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  ft
}

#' Create a glance version of the glm object as a rich-formatted table
#'
#' @description
#' Turn the glance of **glm** object into a rich-formatted table with
#' {flextable}. The table can be printed in different formats (HTML, LaTeX,
#' Word, PowerPoint), or rearranged later on.
#'
#' @param data A **glm** object
#' @param header If `TRUE` (by default), add an header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add an equation to the table header.
#'   The equation can also be passed in the form of a character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and original labels are not used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object is produced that you can print in different
#'   formats (HTML, LaTeX, Word, PowerPoint) or rearrange with the {flextable}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$glance(iris_glm)
tabularise_glance.glm <- function(data, header = TRUE, title = NULL,
equation = TRUE, auto.labs = TRUE, origdata = NULL, labs = NULL,
lang = getOption("data.io_lang", "en"), ..., env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  info_lang <- .infos_lang.glm(lang = lang)

  # Extract labels off data or origdata
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::glance(x = data))
  rownames(data_t) <- data_t$term

  # Use flextable
  ft <- flextable(data_t)
  ft <- colformat_sci(ft)
  #ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- .header_labels(ft, info_lang = info_lang)

  # Headers
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equation(data, swap_var_names =  labs, ...)
    } else {
      equa <- equation(data, ...)
    }

    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      equation = equa)
  } else {
    equa <- NULL
    ft <- .add_header(ft, data = data, info_lang = info_lang, header = header,
      equation = equation)
  }

  if (isTRUE(auto.labs) && any(data_t$term %in% "(Intercept)")) {
    ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
      value = as_paragraph(info_lang[["(Intercept)"]]))
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  autofit(ft, part = c("header", "body"))
}

#' Create a rich-formatted table using the table of coefficients of the summary.glm object
#'
#' @description
#' Create a rich-formatted {flextable} object with the table of coefficients
#' from the [summary()] of a **glm** object.
#'
#' @param data A **summary.glm** object
#' @param ... Additional arguments passed to [modelit::tabularise_tidy.glm()]
#' @param env The environment where to evaluate the model.
#'
#' @return  A **flextable** object that you can print in different formats
#'   (HTML, LaTeX, Word, PowerPoint) or rearrange with the {flextable}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.glm
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_glm_sum <- summary(iris_glm)
#' tabularise::tabularise$coef(iris_glm_sum)
tabularise_coef.summary.glm <- function(data, ..., env = parent.frame()) {

  lm_original <- data$call
  data <- eval(lm_original, envir = env)

  tabularise_tidy.glm(data = data, ...)
}

#' Create a rich-formatted table from a summary.glm object
#'
#' @description
#' Create a rich-formatted table version of the [summary()] of a **glm** object.
#'
#' @param data A **summary.glm** object
#' @param ... Additional arguments passed to [modelit::tabularise_coef.summary.glm()]
#' @param env The environment where to evaluate the model.
#'
#' @return A **flextable** object that you can print in different form or
#'   rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.glm
#'
#' @examples
#' iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_glm_sum <- summary(iris_glm)
#' tabularise::tabularise(iris_glm_sum)
tabularise_default.summary.glm <- function(data, ..., env = parent.frame()) {

  tabularise_coef.summary.glm(data = data, ...)
}

# Choose the lang and the infos_lang
.infos_lang.glm <- function(lang) {
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.glm
  } else {
    info_lang <- infos_en.glm
  }

  info_lang
}

infos_en.glm <- list(
  labs = c(
    term = "Term",
    estimate = "Estimate",
    conf.low = "Lower bound (CI)",
    conf.high = "Upper bound (CI)",
    std.error = "Standard Error",
    t.value = "*t* value",
    sigma = "RSE",
    r.squared = "R^2^",
    adj.r.squared = "Adj.R^2^",
    AIC = "AIC",
    BIC = "BIC",
    statistic = "*t* value",
    statistic2 = "*z* value",
    p.value = "*p* value",
    deviance = "D\u00e9viance",
    logLik = "Log-Likelihood",
    null.deviance = "Null deviance",
    df.null = "Null df",
    df = "Num. df",
    df.residual = "Denum. df",
    nobs = "N",
    "(Intercept)" = "Intercept"),
  footer = c(
    "df" = "Degrees of freedom:",
    "total" = "Total (i.e. Null)",
    "residual" = "Residual",
    "null.deviance" = "Null deviance:",
    "resid.deviance" = "Residual Deviance:",
    AIC = "AIC:"),
  "(Intercept)" = "Intercept",
  "summary" = "Model summary",
  "header" = "Generalized Linear Model"
)

infos_fr.glm <- list(
  labs = c(
    term = "Terme",
    estimate = "Valeur estim\u00e9e",
    conf.low = "Limite basse (IC)",
    conf.high = "Limite haute (IC)",
    std.error = "Erreur standard",
    t.value = "Valeur de *t*",
    p.value = "Valeur de *p*",
    sigma = "RSE",
    r.squared = "R^2^",
    adj.r.squared = "R^2^ ajust\u00e9",
    deviance = "D\u00e9viance",
    logLik = "Log-vraisemblance",
    null.deviance = "D\u00e9viance nulle",
    df.null = "Ddl nulle",
    AIC = "AIC",
    BIC = "BIC",
    statistic = "Valeur de *t*",
    statistic2 = "Valeur de *z*",
    df = "Ddl num.",
    df.residual = "Ddl d\u00e9nom.",
    nobs = "N",
    "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine"
  ),
  footer = c(
    "df" = "Degr\u00e9s de libert\u00e9 :",
    "total" = "Totaux (i.e. Nulle)",
    "residual" = "R\u00e9sidus",
    "null.deviance" = "D\u00e9viance nulle :",
    "resid.deviance" = "D\u00e9viance r\u00e9siduelle :",
    AIC = "AIC :"),
  "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine",
  "summary" = "R\u00e9sum\u00e9 du mod\u00e8le",
  "header" = "Mod\u00e8le lin\u00e9aire g\u00e9n\u00e9ralis\u00e9"
)
