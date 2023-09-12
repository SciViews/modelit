#' Create a rich-formatted table using the coefficients of an lm object
#'
#' @description
#' This function extracts and formats the table of coefficients from an **lm**
#' object, similar to [stats::coef()], but in a rich-formatted table using
#' {flextable}.
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add a equation to the table header.
#'   The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different formats
#'   (HTML, LaTeX, Word, PowerPoint) or rearrange with the {flextable}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom knitr opts_current
#' @method tabularise_coef lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$coef(iris_lm)
tabularise_coef.lm <- function(data, header = TRUE, title = NULL,
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
  info_lang <- .infos_lang.lm(lang = lang)

  # Extract coefficients
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

#' Create a rich-formatted table from an lm object
#'
#' @description
#' The default [tabularise()] method for **lm** objects create a minimalist
#' table with result of the analysis in a rich-formatted tabular presentation.
#'
#' @param data An **lm** object
#' @param ... Additional arguments passed to [modelit::tabularise_coef.lm()]
#'
#' @return A **flextable** object that you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise(iris_lm)
tabularise_default.lm <- function(data, ...) {
  tabularise_coef.lm(data = data, ...)
}

#' Tidy version of the lm object into a flextable object
#'
#' @description
#' Create a rich-formatted table with the 'tidy' information from an **lm**
#' object.
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add an header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add an equation to the table header.
#'   The equation can also be passed in the form of a character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and no term is changed.
#' @param conf.int If `TRUE`, add the confidence interval. The default is
#'   `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if
#'   `conf.int = TRUE`. The default is 0.95.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$tidy(iris_lm)
tabularise_tidy.lm <- function(data, header = TRUE, title = NULL,
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
  info_lang <- .infos_lang.lm(lang = lang)

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

  # Use flextable
  if (isTRUE(show.signif.stars)) {
    ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
  } else {
    ft <- flextable(data_t)
  }
  ft <- colformat_sci(ft)
  ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

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

#' Glance version of the lm object into a flextable object
#'
#' @description
#' Create a rich-formatted table with the 'glance' information from an **lm**
#' object.
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add a equation to the table header.
#'   The equation can also be passed in the form of a character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   of `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different form or
#'   rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$glance(iris_lm)
tabularise_glance.lm <- function(data, header = TRUE, title = NULL,
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
  info_lang <- .infos_lang.lm(lang = lang)

  # Extract labels of data or origdata
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

  if (isTRUE(auto.labs) && any(data_t$term %in% "(Intercept)"))
    ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
      value = as_paragraph(info_lang[["(Intercept)"]]))

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  autofit(ft, part = c("header", "body"))
}

#' Create a rich-formatted table using the table of coefficients of the summary.lm object
#'
#' @param data A **summary.lm** object
#' @param ... Additional arguments passed to [tabularise_tidy.lm()]
#' @param env The environment where to evaluate the model.
#'
#' @return A **flextable** object you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.lm
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_lm_sum <- summary(iris_lm)
#' tabularise::tabularise$coef(iris_lm_sum)
tabularise_coef.summary.lm <- function(data, ..., env = parent.frame()) {

  lm_original <- data$call
  data <- eval(lm_original, envir = env)

  tabularise_tidy.lm(data = data, ...)
}

#' Create a rich-formatted table from an summary.lm object
#'
#' @param data A **summary.lm** object
#' @param ... Additional arguments passed to [tabularise_coef.summary.lm()]
#' @param env The environment where to evaluate the model.
#'
#' @return A **flextable** object you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.lm
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_lm_sum <- summary(iris_lm)
#' tabularise::tabularise(iris_lm_sum)
tabularise_default.summary.lm <- function(data, ..., env = parent.frame()) {
  tabularise_coef.summary.lm(data = data, ...)
}

# Choose the lang and the infos_lang
.infos_lang.lm <- function(lang) {
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.lm
  } else {
    info_lang <- infos_en.lm
  }

  info_lang
}

infos_en.lm <- list(
  labs = c(
    term = "Term",
    estimate = "Estimate",
    conf.low = "Lower bound (CI)",
    conf.high = "Upper bound (CI)",
    std.error = "Standard Error",
    t.value = "t value",
    sigma = "RSE",
    r.squared = "R^2^",
    adj.r.squared = "Adj.R^2^",
    AIC = "AIC",
    BIC = "BIC",
    deviance = "Deviance",
    logLik = "Log-likelihood",
    statistic = "*t* value",
    p.value = "*p* value",
    df = "Num. df",
    df.residual = "Denum. df",
    nobs = "N",
    "(Intercept)" = "Intercept"),
  "(Intercept)" = "Intercept",
  "summary" = "Model summary",
  "header" = "Linear model"
)

infos_fr.lm <- list(
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
    AIC = "AIC",
    BIC = "BIC",
    statistic = " Valeur de *t*",
    deviance = "D\u00e9viance",
    logLik = "Log-vraisemblance",
    df = "Ddl num.",
    df.residual = "Ddl d\u00e9nom.",
    nobs = "N",
    "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine"
  ),
  "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine",
  "summary" = "R\u00e9sum\u00e9 du mod\u00e8le",
  "header" = "Mod\u00e8le lin\u00e9aire"
)
