# Use the same logic as for nls and summary nls objects

#' Create a rich-formatted table using the coefficients of the lm object
#'
#' @description
#' This function extracts and formats the table of coefficients from an lm object, similar to [stats::coef()], but in flextable.
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE` (by default), add a title to the table header
#' @param equation If `TRUE` (by default), add a equation to the table header. The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically of the object.
#' @param origdata The original dataset this model was fitted to. By default is `NULL`
#' @param labs Labs to change the names of elements in the `term` column of the table. By default is `NULL`
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @method tabularise_coef lm
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' library(tabularise)
#' tabularise$coef(is.lm)
#'
tabularise_coef.lm <- function(data,
  header = TRUE,
  title = header,
  equation = header,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()) {

  # Choose de lang ----
  info_lang <- .infos_lang.lm(lang = lang)

  # Extract coef ----
  {
    co <- coef(data)
    co <- data.frame(term = names(co), estimate = co)
  }
  # co <- as.data.frame(rbind(coef(data)))
  #
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # create the flextable object
  ft <- flextable(co) |>
    colformat_sci()

  ft <- header_labels(ft, lang = lang)

  # Header and equation ----
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equatiomatic::extract_eq(data, swap_var_names =  labs, ...)
    } else {
      equa <- equatiomatic::extract_eq(data, ...)
    }

    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, title = title, equation = equa)
  } else {
    equa <- NULL
    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, title = title, equation = equation)
  }

  if (isTRUE(auto.labs)) {
    if (any(co$term %in% "(Intercept)")) {
      ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
        value = as_paragraph(info_lang[["(Intercept)"]]))
    }
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% co$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = "term",
        value = para_md(labs_red[i]), part = "body")
  }

  if (isTRUE(equation) & !is.null(equa)) {
    params <- .params_equa(equa,...)
    if(length(params) == length(co$term)) {
      ft <- mk_par(ft, j = "term",
        value = para_md(params), part = "body")
    }
  }

  autofit(ft, part = c("header", "body"))
}

#' Create a rich-formatted table from an lm object
#'
#' @param data An **lm** object
#' @param ... Additional arguments passed to [modelit::tabularise_coef.lm()]
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default lm
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' library(tabularise)
#' tabularise(is.lm)
#'
tabularise_default.lm <- function(data, ...) {
  tabularise_coef.lm(data = data, ...)
}

#' Tidy version of the lm object into a flextable object
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE` (by default), add a title to the table header
#' @param equation If `TRUE` (by default), add a equation to the table header. The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically of the object.
#' @param origdata The original dataset this model was fitted to. By default is `NULL`
#' @param labs Labs to change the names of elements in the `term` column of the table. By default is `NULL`
#' @param conf.int If `TRUE`, add the confidence interal.Default is `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Default is 0.95.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars to the table.
#' @param ... Additional arguments passed to [equatiomatic::extract_eq()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy lm
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' library(tabularise)
#' tabularise$tidy(is.lm)
tabularise_tidy.lm <- function(data,
  header = TRUE,
  title = TRUE,
  equation = FALSE,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  conf.int = FALSE,
  conf.level = 0.95,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "lm")
    )}

  # Choose de lang ----
  info_lang <- .infos_lang.lm(lang = lang)

  # Extract labels of data or origdata
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(x = data, conf.int = conf.int, conf.level = conf.level))
  rownames(data_t) <- data_t$term

  if (isTRUE(conf.int)) {
    data_t <- data_t[,
      c("term", "estimate", "conf.low",
        "conf.high", "std.error", "statistic", "p.value")]
  }

  # Use flextable ----
  if (isTRUE(show.signif.stars)) {
    ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
  } else {
    ft <- flextable(data_t)
  }

  # Use tabularise with colformat_sci() ----
  ft <- colformat_sci(ft)
  ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- header_labels(ft, lang = lang)

  # headers
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equatiomatic::extract_eq(data, swap_var_names =  labs, ...)
    } else {
      equa <- equatiomatic::extract_eq(data, ...)
    }

    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, equation = equa)
  } else {
    equa <- NULL
    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, equation = equation)
  }

  if (isTRUE(auto.labs)) {
    if (any(data_t$term %in% "(Intercept)")) {
      ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
        value = as_paragraph(info_lang[["(Intercept)"]]))
    }
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  if (isTRUE(equation) & !is.null(equa)) {
    #params <- .params_equa(equa, ...)
    params <- .params_equa(equa)
    if (length(params) == length(data_t$term)) {
      ft <- mk_par(ft, j = "term",
        value = para_md(params), part = "body")
    }
  }

  # Add information on the p.value (with internal function) ----
  if (ncol_keys(ft) > ncol(data_t)) {
    ft <- add_signif_stars(ft, j = "signif")
  }

  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  ft
}

#' Tidy version of the lm object into a flextable object
#'
#' @param data An **lm** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE` (by default), add a title to the table header
#' @param equation If `TRUE` (by default), add a equation to the table header. The equation can also be passed in the form of a character string.
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically of the object.
#' @param origdata The original dataset this model was fitted to. By default is `NULL`
#' @param labs Labs to change the names of elements in the `term` column of the table. By default is `NULL`
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [equatiomatic::extract_eq()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance lm
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' library(tabularise)
#' tabularise$glance(is.lm)
tabularise_glance.lm <- function(data,
  header = TRUE,
  title = TRUE,
  equation = FALSE,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "lm")
    )}

  # Choose de lang ----
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

  # Use flextable ----
  ft <- flextable(data_t)

  # Use tabularise with colformat_sci() ----
  ft <- colformat_sci(ft)
  #ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- header_labels(ft, lang = lang)

  # headers
  if (isTRUE(equation)) {
    if (!is.null(labs)) {
      equa <- equatiomatic::extract_eq(data, swap_var_names =  labs, ...)
    } else {
      equa <- equatiomatic::extract_eq(data, ...)
    }

    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, equation = equa)
  } else {
    equa <- NULL
    ft <- add_header_lm(ft, data = data,
      lang = lang, header = header, equation = equation)
  }

  if (isTRUE(auto.labs)) {
    if (any(data_t$term %in% "(Intercept)")) {
      ft <- mk_par(ft, i = "(Intercept)", j = 1, part = "body",
        value = as_paragraph(info_lang[["(Intercept)"]]))
    }
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  ft
}

#' Create a rich-formatted table using the table of coefficients of the summary.lm object
#'
#' @param data An **summary.lm** object
#' @param ... Additional arguments passed to [modelit::tabularise_tidy.lm()]
#' @param env The environment where to evaluate the model.
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.lm
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' is.sum <- summary(is.lm)
#' library(tabularise)
#' tabularise$coef(is.sum)
tabularise_coef.summary.lm <- function(data,
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "lm")
    )}

  lm_original <- data$call
  data <- eval(lm_original, envir = env)

  tabularise_tidy.lm(data = data, ...)
}

#' Create a rich-formatted table from an summary.lm object
#'
#' @param data An **summary.lm** object
#' @param ... Additional arguments passed to [modelit::tabularise_coef.summary.lm()]
#' @param env The environment where to evaluate the model.
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.lm
#'
#' @examples
#' is.lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' is.sum <- summary(is.lm)
#' library(tabularise)
#' tabularise(is.sum)
tabularise_default.summary.lm <- function(data,
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "lm")
    )}

  tabularise_coef.summary.lm(data = data, ...)
}


# Internal function ----
## Internal function to add header and equation ----
add_header_lm <- function(x,
  lang,
  header = TRUE,
  title = header,
  equation,
  ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "add_header_lm()")) }

  info_lang <- .infos_lang.lm(lang = lang)

  ft <- x

  if (isTRUE(header)) {
    if (is.character(equation)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(
          as_equation(equation)
        ))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

    if (isTRUE(title)) {
      ft <- add_header_lines(ft, values = info_lang[["header"]])
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

  }

  h_nrow <- nrow_part(ft, part = "header")

  if (h_nrow > 2) {
    ft |>
      border_inner_h(border = officer::fp_border(width = 0), part = "header") |>
      hline(i= nrow_part(ft, "header")-1,
        border = officer::fp_border(width = 1.5, color = "#666666"),
        part = "header") ->
      ft
  }

  ft
}

## Internal function change the labels of header ----
header_labels <- function(x, lang = lang, ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels()")) }

  # choose de lang ----
  info_lang <- .infos_lang.lm(lang = lang)

  ft <- x

  hlabs <- info_lang[["labs"]]
  hlabs_red <- hlabs[names(hlabs) %in% ft$header$col_keys]

  for (i in seq_along(hlabs_red))
    ft <- mk_par(ft, i = 1, j = names(hlabs_red)[i],
      value = para_md(hlabs_red[i]), part = "header")
  ft
}

# Internal function of flextable
pvalue_format <- function(x){
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels = c("***", " **", "  *", "  .", "   "))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

# Internal function to add pvalue signif
add_signif_stars <- function(x, i = NULL, j = NULL, part = "body", align = "right", ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels()"))}

  ft <- x

  ft <- mk_par(ft, i =i,  j = j, value =  as_paragraph(
    pvalue_format(.data$p.value)))
  ft <- add_footer_lines(ft,
    values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
  ft <- align(ft, i = 1, align = align, part = "footer")

  ft
}

## Internal function : Extract labels and units  ----
.labels <- function(x, units = TRUE, ...) {
  labels <- sapply(x, data.io::label, units = units)

  if (any(labels != "")) {
    # Use a \n before labels and the units
    if (isTRUE(units))
      labels <- sub(" +\\[([^]]+)\\]$", "\n[\\1]", labels)
    # set names if empty
    labels[labels == ""] <- names(x)[labels == ""]
    # Specific case for I() using in a formula
    labels[grepl("^I\\(.*\\)$", names(labels))] <- names(labels)[grepl("^I\\(.*\\)$", names(labels))]
  }

  if (all(labels == "")) {
    labels <- NULL
  }
  labels
}

## Intenal function
.labels2 <- function(x, origdata = NULL, labs = NULL) {

  labs_auto <- NULL
  labs_auto <- .labels(x$model)

  if (!is.null(origdata)) {
    labs_auto <- .labels(origdata)
  }

  if (!is.null(labs)) {
    if (!is.character(labs))
      stop("labs is not character vector")
    if (is.null(names(labs)))
      stop("labs must be named character vector")
    if (any(names(labs) %in% ""))
      stop("all element must be named")
    labs_res <- c(labs, labs_auto[!names(labs_auto) %in% names(labs)])
  } else {
    labs_res <- labs_auto
  }

  labs_res
}

## Internale function : Choose the lang and the infos_lang ----
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
    statistic = "F value",
    p.value = "P value",
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
    t.value = "Valeur de t",
    p.value = "Valeur de P",
    sigma = "RSE",
    r.squared = "R^2^",
    adj.r.squared = "R^2^ ajust\u00e9",
    AIC = "AIC",
    BIC = "BIC",
    statistic = " Valeur de F",
    df = "Ddl num.",
    df.residual = "Ddl d\u00e9nom.",
    nobs = "N",
    "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine"
  ),
  "(Intercept)" = "Ordonn\u00e9e \u00e0 l'origine",
  "summary" = "R\u00e9sum\u00e9 du mod\u00e8le",
  "header" = "Mod\u00e8le lin\u00e9aire"
)


.params_equa <- function(x, intercept = "alpha", greek = "beta") {
  vals <- NULL

  if (intercept != greek) {
    if (grepl(intercept, x)) {
      it <- paste0("\\\\", intercept)
      res <- regmatches(x,
        gregexpr(it, x))[[1]]

      vals <- paste0("$",res, "$")
      }
    }

  if (grepl(greek, x)) {
    g <- paste0("\\\\", greek,"_\\{\\d+\\}")
    res <- regmatches(x,
      gregexpr(g, x)
      )[[1]]
    res1 <- paste0("$",res, "$")
    vals <- c(vals, res1)
  }

  vals

}

