#' Create a rich-formatted table using the coefficients of an lm object
#'
#' @description
#' This function extracts and formats the table of coefficients from an **lm**
#' object, similar to [stats::coef()], but in a rich-formatted table using
#' \{flextable\}.
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation If `TRUE` (by default), add a equation to the table header.
#'   The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used.
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different formats
#'   (HTML, LaTeX, Word, PowerPoint) or rearrange with the \{flextable\}
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
  lang = getOption("data.io_lang", "en"), ..., kind = "ft",
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

  # Extract coefficients
  df <- coef(data)
  df <- data.frame(term = names(df), estimate = df)

  df <- extract_infos(df,
    show.signif.stars = FALSE, info_lang = info_lang,
    auto.labs = auto.labs, data = data, origdata = origdata, labs = labs,
    equation = equation, title = title)

  # formatted table ----
  formate_table(df, kind = kind, header = header)
}

#' Create a rich-formatted table from an lm object
#'
#' @description
#' The default [tabularise()] method for **lm** objects create a minimalist
#' table with result of the analysis in a rich-formatted tabular presentation.
#'
#' @param data An **lm** object
#' @param ... Additional arguments passed to [modelit::tabularise_coef.lm()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate the model.
#' @return A **flextable** object that you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise(iris_lm)
tabularise_default.lm <- function(data, ..., kind = "ft", env = parent.frame()) {
  # Note: there isn't much in print(lm_obj) than the table of coefficients
  # so, we produce the same table as tabularise$coef() here
  tabularise_coef.lm(data = data, ..., kind = kind, env = env)
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
#'   from data or `origdata=`.
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
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the \{flextable\} functions.
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
  show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft",
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

  # Extract coefficients with broom::tidy() ----
  df <- as.data.frame(broom::tidy(x = data, conf.int = conf.int,
    conf.level = conf.level))
  rownames(df) <- df$term

  if (isTRUE(conf.int)) {
    df <- df[, c("term", "estimate", "conf.low", "conf.high",
      "std.error", "statistic", "p.value")]
  }

  df <- extract_infos(df,
    show.signif.stars = show.signif.stars, info_lang = info_lang,
    auto.labs = auto.labs, data = data, origdata = origdata, labs = labs,
    equation = equation, title = title)

  # formatted table ----
  formate_table(df, kind = kind, header = header)
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
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [tabularise::equation()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different form or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$glance(iris_lm)
tabularise_glance.lm <- function(data, header = TRUE, title = NULL,
  equation = TRUE, auto.labs = TRUE, origdata = NULL, labs = NULL,
  lang = getOption("data.io_lang", "en"), ..., kind = "ft",
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

  # Extract coefficients with broom::tidy() ----
  df <- as.data.frame(broom::glance(x = data))
  rownames(df) <- df$term

  df <- extract_infos(df,
    show.signif.stars = FALSE, info_lang = info_lang,
    auto.labs = auto.labs, data = data, origdata = origdata, labs = labs,
    equation = equation, title = title)

  # formatted table ----
  formate_table(df, kind = kind, header = header)
}


#' Create a rich-formatted table using the table of coefficients of the summary.lm object
#'
#' @param data A **summary.lm** object
#' @param ... Additional arguments passed to [tabularise_tidy.lm()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate the model.
#'
#' @return A **flextable** object you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.lm
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_lm_sum <- summary(iris_lm)
#' tabularise::tabularise$coef(iris_lm_sum)
tabularise_coef.summary.lm <- function(data, ..., kind = "ft",
    env = parent.frame()) {

  lm_original <- data$call
  data <- eval(lm_original, envir = env)

  tabularise_tidy.lm(data = data, ..., kind = kind, env = env)
}

#' Create a rich-formatted table from an summary.lm object
#'
#' @param data A **summary.lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param footer If `TRUE` (by default), add a footer to the table
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [tabularise_coef.summary.lm()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate the model.
#'
#' @return A **flextable** object you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.lm
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' iris_lm_sum <- summary(iris_lm)
#' tabularise::tabularise(iris_lm_sum)
tabularise_default.summary.lm <- function(data, header = TRUE, footer = TRUE,
    lang = getOption("data.io_lang", "en"), ..., kind = "ft",
    env = parent.frame()) {

  df <- tabularise_coef.summary.lm(data = data, lang = lang, header = header ,
    kind = "df", ..., env = env)

  if (isTRUE(footer)) {
    info_lang <- .infos_lang.lm(lang = lang)
    df <- extract_footer(df, data = data, info_lang = info_lang)
  }

  formate_table(df, kind= kind, header = header)
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
    df = "Model df",
    df.residual = "Residuals df",
    nobs = "N",
    signif = "",
    "(Intercept)" = "Intercept"),
  "(Intercept)" = "Intercept",
  "summary" = "Model summary",
  "header" = "Linear model",
  footer = c(
    "resid.range" = "Residuals range:",
    "resid.std.err" = "Residuals standard error:",
    "on" = "on",
    "and" = "and",
    "df" = "df",
    "df2" = "degrees of freedom",
    "R2" = "Multiple *R*^2^:",
    "adj.R2" = "adjusted *R*^2^:",
    "f.stat" = "*F*-statistic:",
    "p" = "*p* value:"
  )
)

infos_fr.lm <- list(
  labs = c(
    term = "Terme",
    estimate = "Valeur estim\u00e9e",
    conf.low = "Limite basse (IC)",
    conf.high = "Limite haute (IC)",
    std.error = "Ecart type",
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
    df = "Ddl mod\u00e8le",
    df.residual = "Ddl r\u00e9sidus",
    nobs = "N",
    signif = "",
    "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine"
  ),
  "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine",
  "summary" = "R\u00e9sum\u00e9 du mod\u00e8le",
  "header" = "Mod\u00e8le lin\u00e9aire",
  footer = c(
    "resid.range" = "Etendue des r\u00e9sidus :",
    "resid.std.err" = "Ecart type des r\u00e9sidus :",
    "on" = "pour",
    "and" = "et",
    "df" = "ddl",
    "df2" = "degr\u00e9s de libert\u00e9",
    "R2" = "*R*^2^ multiple :",
    "adj.R2" = "*R*^2^ ajust\u00e9 :",
    "f.stat" = "Statistique *F* :",
    "p" = "valeur de *p* :"
  )
)


# A list of functions ------

extract_significance_stars <- function(x, show.signif.stars) {
  if (isTRUE(show.signif.stars)) {
    x[["signif"]] <- .pvalue_format(x[["p.value"]])
    attr(x, "signif.stars") <- "0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"
    attr(x, "show.signif.stars") <- show.signif.stars
  }
  return(x)
}

extract_column_names <- function(x, info_lang) {
  hlabs <- info_lang[["labs"]]
  attr(x, "col.names") <- hlabs[names(hlabs) %in% names(x)]
  return(x)
}

replace_intercept_term <- function(x, info_lang) {
  if (any(x$term %in% "(Intercept)")) {
    x$term[x$term == "(Intercept)"] <- info_lang[["(Intercept)"]]
  }
  return(x)
}

extract_labels <- function(x, auto.labs, data, origdata, labs) {
  if (isTRUE(auto.labs)) {
    labs <- tabularise:::.labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- tabularise:::.labels2(x = NULL, labs = labs)
  }
  attr(x, "labs") <- labs
  return(x)
}

extract_equation <- function(x, equation, data, labs, ...) {
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equation(data, swap_var_names =  labs, ...)
    } else {
      equa <- equation(data, auto.labs = FALSE, ...)
    }
    attr(x, "equation") <- equa
    attr(x, "equation_params") <- .params_equa(equa)
  }

  if (is.character(equation)) {
    attr(x, "equation") <- equation
  }

  return(x)
}

extract_title <- function(x, title, info_lang) {
  if (isTRUE(title)) {
    attr(x, "title") <-  info_lang[["header"]]
  }

  if (is.character(title)) {
    attr(x, "title") <- title
  }

  return(x)
}

extract_footer <- function(x, data, info_lang) {
  digits <- max(3L, getOption("digits") - 3L)
  footer <- info_lang[["footer"]]
  vals <- c(
    paste0(footer[["resid.range"]], " [",
      format(signif(min(data$residuals, na.rm = TRUE), digits)), ", ",
      format(signif(max(data$residuals, na.rm = TRUE), digits)), "] "),
    paste(footer[["resid.std.err"]],
      format(signif(data$sigma, digits)), footer[["on"]],
      max(data$df), footer[["df2"]]),
    paste(footer[["R2"]], format(signif(data$r.squared, digits)), "  -  ",
      footer[["adj.R2"]], format(signif(data$adj.r.squared, digits))),
    paste(footer[["f.stat"]], format(signif(data$fstatistic[1L], digits)),
      footer[["on"]], format(signif(data$fstatistic[2L], digits)),
      footer[["and"]], format(signif(data$fstatistic[3L], digits)),
      footer[["df"]], "  -  ", footer[["p"]],
      format.pval(pf(data$fstatistic[1L],  data$fstatistic[2L],
        data$fstatistic[3L], lower.tail = FALSE)))
    # TODO: nicely format this last p value!
  )
  attr(x, "footer") <- vals
  return(x)
}

extract_infos <- function(x, show.signif.stars, info_lang, auto.labs, data, origdata, labs, equation, title) {
  # stars ----
  x <- extract_significance_stars(x, show.signif.stars = show.signif.stars)

  # colnames -----
  x <- extract_column_names(x, info_lang = info_lang)

  # specific case : (Intercept) -> Intercept ----
  x <- replace_intercept_term(x, info_lang = info_lang)

  # labels ----
  x <- extract_labels(x = x, auto.labs = auto.labs,
    data = data, origdata = origdata, labs = labs)

  # equation ----
  x <- extract_equation(x, equation = equation, data = data,
    labs = attr(x, "labs"))

  # title ----
  x <- extract_title(x, title = title, info_lang = info_lang)

  return(x)
}

.add_signif <- function(x, signif) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_signif_stars()"))}

  ft <- x
  s <- signif

  ft <- add_footer_lines(ft,
    values = s)
  align(ft, i = 1, align = "right", part = "footer")
}

.extract_labels <- function(x, info_lang) {
  hlabs <- info_lang[["labs"]]
  hlabs[names(hlabs) %in% names(x)]
}

.add_header2 <- function(x, title, equation) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_header2()")) }

  ft <- x

  if (is.character(equation)) {
    ft <- add_header_lines(ft,
      values = as_paragraph(as_equation(equation)))
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  if (is.character(title)) {
    ft <- add_header_lines(ft,
      values = as_paragraph(title))
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  h_nrow <- nrow_part(ft, part = "header")

  if (h_nrow > 2) {
    ft |>
      border_inner_h(border = officer::fp_border(width = 0), part = "header") |>
      hline(i = nrow_part(ft, "header") - 1,
        border = officer::fp_border(width = 1.5, color = "#666666"),
        part = "header") ->
      ft
  }

  ft
}

.add_colnames <- function(x, labs) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_colnames()")) }

  ft <- x

  for (i in seq_along(labs))
    ft <- mk_par(ft, i = 1, j = names(labs)[i],
      value = para_md(labs[i]), part = "header")

  ft
}

.add_labs <- function(x, labs) {
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_colnames()")) }

  ft <- x

  labs_red <- labs[names(labs) %in%ft$body$dataset$term]

  for (i in seq_along(labs_red))
    ft <- mk_par(ft, i = names(labs_red)[i], j = "term",
      value = para_md(labs_red[i]), part = "body")

  ft
}

.add_params <- function(x, params) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_colnames()")) }

  ft <- x

  if (length(params) == length(ft$body$dataset$term))
    ft <- mk_par(ft, j = "term", value = para_md(params), part = "body")

  ft
}

create_flextable <- function(x, header = TRUE) {
  ft <- flextable(x) |>
    colformat_sci()

  if ("p.value" %in% colnames(x)) {
    ft <- ft |>
      colformat_sci(j = "p.value", lod = 2e-16)
  }

  if (!is.null(attr(x, "col.names"))) {
    ft <- .add_colnames(ft, attr(x, "col.names"))
  }

  if (!is.null(attr(x, "labs"))) {
    ft <- .add_labs(ft, attr(x, "labs"))
  }

  if (!is.null(attr(x,"equation")) & !is.null(attr(x, "equation_params"))) {
    ft <- .add_params(ft, attr(x, "equation_params"))
  }

  if (isTRUE(header)) {
    ft <- .add_header2(ft, title = attr(x, "title"), equation = attr(x, "equation"))
  }

  if (!is.null(attr(x,"signif.stars"))) {
    ft <- .add_signif(ft, attr(x, "signif.stars"))
  }

  if (!is.null(attr(x, "footer"))) {
    vals <- attr(x, "footer")
    ft <- add_footer_lines(ft, top = FALSE, values = para_md(vals))
    ft <- align(ft, i = seq_len(length(vals)) + 1 , align = "left",
      part = "footer")
  }

  ft <- autofit(ft, part = c("header", "body"))

  sss <- attr(x,"show.signif.stars")
  if (!is.null(sss) && isTRUE(sss)) {
    ft <- width(ft, j = "signif", width = 0.4)
  }

  return(ft)
}

formate_table <- function(df, kind, header) {
  switch(kind,
    df = {df},
    tt = {
      stop("Not implemented yet")
    },
    ft = {
      create_flextable(df, header = header)
    },
    gt = {
      stop("Not implemented yet")
    }
  )
}
