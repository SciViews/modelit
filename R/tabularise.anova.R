#' Create a rich-formatted table from an anova object
#'
#' @param data An **anova** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE` (by default), add a title to the table header
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically of the object
#' @param origdata The original dataset this model was fitted to. By default is `NULL`
#' @param labs Labs to change the names of elements in the `term` column of the table. By default is `NULL`
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars to the table.
#' @param ... Additional arguments (unused for now)
#' @param env The environment where to evaluate lazyeval expressions (unused for now)
#'
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default anova
#' @examples
#' is.lm <- anova(lm(data = iris, Petal.Length ~ Species))
#' library(tabularise)
#' tabularise(is.lm)
tabularise_default.anova <- function(data,
  header = TRUE,
  title = header,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "anova")
    )}

  # Choose de lang ----
  info_lang <- .infos_lang.anova(lang = lang)

  # Extract labels of data or origdata
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(x = data))

  if(grepl(x = attr(data, "heading")[2],pattern = "^Model")) {
    data_t$term <- paste(info_lang[["Model"]], 1:nrow(data_t))
    info_lang$labs[["term"]] <- info_lang$Model[["Model"]]
  }

  rownames(data_t) <- data_t$term

  if (isTRUE(show.signif.stars)) {
    if("p.value" %in% names(data_t)) {
      ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
    } else {
      ft <- flextable::flextable(data_t)
    }
  } else {
    ft <- flextable::flextable(data_t)
  }

  # Use tabularise with colformat_sci() ----
  ft <- colformat_sci(ft)
  if("p.value" %in% names(data_t))
    ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- .header_labels(ft, info_lang = info_lang)

  statis <- info_lang[["statistic"]]
  statis_red <- statis[names(statis) %in% names(data)]

  if (length(statis_red) == 1) {
    ft <- mk_par(ft, i = 1, j = "statistic",
      value = para_md(statis_red), part = "header")
  }

  # Rename terms column
  if (isTRUE(auto.labs)) {
    if (any(data_t$term %in% "Residuals")) {
      ft <- mk_par(ft, i = "Residuals", j = 1, part = "body",
        value = as_paragraph(info_lang[["residuals"]]))
    }
    if (any(data_t$term %in% "NULL")) {
      ft <- mk_par(ft, i = "NULL", j = 1, part = "body",
        value = as_paragraph(info_lang[["NULL"]]))
    }
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]
    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  # Add header ----
  if (isTRUE(header)) {

    if (isTRUE(title)) {

      heading <- attr(data, "heading")

      {
        heading ->.
        paste0(., collapse = "") ->.
        gsub(x = ., pattern = "\n\n", replacement = "\n")  ->.
        sub(x = ., pattern = "\n$", replacement = "") ->
          heading
      }

      if (!is.null(labs)) {
        header <- c(info_lang[["method"]], info_lang[["header"]],
          gsub(x = labs, pattern = "\n", replacement = " ")
        )
      } else {
        header <- c(info_lang[["method"]], info_lang[["header"]])
      }
      #header <- c(info_lang[["method"]], info_lang[["header"]])

      for (i in seq_len(length(header))) {
        heading <- gsub(x = heading, pattern = names(header)[i], replacement = header[i], fixed = TRUE)
      }


      if(length(heading) == 1) {
        ft <- add_header_lines(ft, values = heading)
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }


  # Add information on the p.value (with internal function) ----
  if (ncol_keys(ft) > ncol(data_t)) {
    ft <- .add_signif_stars(ft, j = "signif")
  }

  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  if("p.value" %in% names(data_t)) {
    if (isTRUE(show.signif.stars))
      ft <- width(ft, j = "signif", width = 0.4)
  }
  ft
}


#' Tidy version of the anova object into a flextable object
#'
#' @param data An **anova** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE` (by default), add a title to the table header
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically of the object
#' @param origdata The original dataset this model was fitted to. By default is `NULL`
#' @param labs Labs to change the names of elements in the `term` column of the table. By default is `NULL`
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars to the table.
#' @param ... Additional arguments (unused for now)
#' @param env The environment where to evaluate lazyeval expressions (unused for now)
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy anova
#' @examples
#' is.lm <- anova(lm(data = iris, Petal.Length ~ Species))
#' library(tabularise)
#' tabularise$tidy(is.lm)
tabularise_tidy.anova <- function(data,
  header = TRUE,
  title = header,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  env = parent.frame()) {

  if ( !requireNamespace("broom", quietly = TRUE)) {
    stop(sprintf(
      "'%s' package should be installed to create a flextable from an object of type '%s'.",
      "broom", "anova")
    )}

  # Choose de lang ----
  info_lang <- .infos_lang.anova(lang = lang)

  # Extract labels of data or origdata
  if (isTRUE(auto.labs)) {
    labs <- .labels2(x = data, origdata = origdata, labs = labs)
  } else {
    labs <- .labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(x = data))
  rownames(data_t) <- data_t$term

  if (isTRUE(show.signif.stars)) {
    if("p.value" %in% names(data_t)) {
      ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
    } else {
      ft <- flextable::flextable(data_t)
    }
  } else {
    ft <- flextable::flextable(data_t)
  }

  # Use tabularise with colformat_sci() ----
  ft <- colformat_sci(ft)
  if("p.value" %in% names(data_t))
    ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Rename headers labels
  ft <- .header_labels(ft, info_lang = info_lang)

  statis <- info_lang[["statistic"]]
  statis_red <- statis[names(statis) %in% names(data)]

  if (length(statis_red) == 1) {
    ft <- mk_par(ft, i = 1, j = "statistic",
      value = para_md(statis_red), part = "header")
  }

  # Rename terms column
  if (isTRUE(auto.labs)) {
    if (any(data_t$term %in% "Residuals")) {
      ft <- mk_par(ft, i = "Residuals", j = 1, part = "body",
        value = as_paragraph(info_lang[["residuals"]]))
    }
    if (any(data_t$term %in% "NULL")) {
      ft <- mk_par(ft, i = "NULL", j = 1, part = "body",
        value = as_paragraph(info_lang[["NULL"]]))
    }
  }

  if (!is.null(labs)) {
    labs_red <- labs[names(labs) %in% data_t$term]

    for (i in seq_along(labs_red))
      ft <- mk_par(ft, i = names(labs_red)[i], j = 1,
        value = para_md(labs_red[i]), part = "body")
  }

  # Add header ----
  if (isTRUE(header)) {
    if (isTRUE(title)) {
      method <- info_lang[["method"]]
      headings <- attr(data, "heading")[1]

      res <- sapply(names(method), function(name) grepl(paste0("^",name), x = headings))
      method <- method[res]

      if(length(method) == 1) {
        ft <- add_header_lines(ft, values = method)
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }

  # Add information on the p.value (with internal function) ----
  if (ncol_keys(ft) > ncol(data_t)) {
    ft <- .add_signif_stars(ft, j = "signif")
  }

  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  if("p.value" %in% names(data_t)) {
    if (isTRUE(show.signif.stars))
      ft <- width(ft, j = "signif", width = 0.4)
  }
  ft
}

#' Tidy version of the aov object into a flextable object
#'
#' @param data An **anova** object
#' @param ... Additional arguments passed to [modelit::tabularise_tidy.anova()]
#'
#' @return  **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy aov
#' @examples
#' is.aov <- aov(data = iris, Petal.Length ~ Species)
#' library(tabularise)
#' tabularise$tidy(is.aov)
tabularise_tidy.aov <- function(data, ...) {
  data <- anova(data)
  tabularise_tidy.anova(data)
}

## Internale function : Choose the lang and the infos_lang ----
.infos_lang.anova <- function(lang) {
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.anova
  } else {
    info_lang <- infos_en.anova
  }

  info_lang
}

infos_en.anova <- list(
  method = c(
    "Type III Analysis of Variance Table with Satterthwaite's method" = "Type III analysis of variance with Satterthwaite's method",
    "Analysis of Deviance Table" = "Analysis of deviance",
    "Analysis of Variance Table" = "Analysis of variance"
    ),
  header = c(
    "Response:" = "Response:"
  ),
  labs = c(
    term = "Term",
    "df" = "Df",
    "df.residual" = "Residuals Df",
    rss = "Residual sum of squares",
    "sumsq" = "Sum of squares",
    "meansq" = "Mean squares",
    "p.value" = "*p* value",
    num.df = "Num. Df",
    NumDF = "Num. Df",
    den.df = "Denom. Df",
    DenDF = "Denom. Df",
    deviance = "Deviance",
    residual.deviance = "Residual deviance"),
  statistic = c(
    "F value" = "*F*~obs.~ value",
    "F" = "*F*~obs.~ value",
    Chisq = "$$\\chi2_{obs.}$$"),
  residuals = c(Residuals = "Residuals"),
  "NULL" = c("NULL" = "None"),
  Model = c("Model" = "Model")
)

infos_fr.anova <- list(
  method = c(
    "Type III Analysis of Variance Table with Satterthwaite's method" = "Analyse de la variance de type III",
    "Analysis of Deviance Table" = "Analyse de la d\u00e9viance",
    "Analysis of Variance Table" = "Analyse de la variance"
  ),
  header = c(
    "Response:" = "R\u00e9ponse :",
    "Model:" = "Mod\u00e8le :",
    "Model" = "Mod\u00e8le",
    "link:" = "Lien :",
    "Terms added sequentially (first to last)" = "Termes ajout\u00e9s s\u00e9quentiellement (du premier au dernier)"
    ),
  labs = c(
    "term" = "Terme",
    "df.residual" = "Ddl des r\u00e9sidus",
    rss = "R\u00e9sidus de la somme des carr\u00e9s",
    "df" = "Ddl",
    "sumsq" = "Somme des carr\u00e9s",
    "meansq" = "Carr\u00e9 moyens",
    "p.value" = "Valeur de *p*",
    num.df = "Ddl num.",
    NumDF = "Ddl num.",
    den.df = "Ddl d\u00e9nom.",
    DenDF = "Ddl d\u00e9nom.",
    deviance = "D\u00e9viance",
    residual.deviance = "D\u00e9viance r\u00e9siduelle"),
  statistic = c(
    "F value" = "Valeur de *F*~obs.~",
    "F" = "Valeur de *F*~obs.~",
    Chisq = "$$\\chi2_{obs.}$$"),
  residuals = c(Residuals = "R\u00e9sidus"),
  "NULL" = c("NULL" = "Aucun"),
  Model = c("Model" = "Mod\u00e8le")
)
