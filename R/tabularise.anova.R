#' Create a rich-formatted table from an anova object
#'
#' @param data An **anova** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   (from `origdata=`)
#' @param origdata The original data set used for the ANOVA. By default it is
#'   `NULL`. Used to extract labels that are lost in the **anova** object.
#' @param labs Labels to change the default names in the `term` column of the
#'   table. By default it is `NULL` and nothing is changed.
#' @param lang The natural language to use. The default value is set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is taken from `getOption("show.signif.stars")`.
#' @param ... Additional arguments (not used for now)
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate lazyeval expressions (not used
#'   for now)
#'
#' @return  A **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default anova
#' @examples
#' iris_anova <- anova(lm(data = iris, Petal.Length ~ Species))
#' tabularise::tabularise(iris_anova)
tabularise_default.anova <- function(data, header = TRUE, title = header,
auto.labs = TRUE, origdata = NULL, labs = NULL,
lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft",
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Select the language
  info_lang <- .infos_lang.anova(lang = lang)

  # Extract labels of data or origdata
  if (isTRUE(auto.labs)) {
    labs <- tabularise:::.labels2(data, origdata = origdata, labs = labs)
  } else {
    labs <- tabularise:::.labels2(x = NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(data))

  if (grepl("^Model", attr(data, "heading")[2])) {
    data_t$term <- paste(info_lang[["Model"]], 1:nrow(data_t))
    info_lang$labs[["term"]] <- info_lang$Model[["Model"]]
  }

  rownames(data_t) <- data_t$term

  if (isTRUE(show.signif.stars) && "p.value" %in% names(data_t)) {
      ft <- flextable::flextable(data_t, col_keys = c(names(data_t), "signif"))
  } else {
    ft <- flextable::flextable(data_t)
  }

  ft <- colformat_sci(ft)
  if ("p.value" %in% names(data_t))
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

  # Add header
  if (isTRUE(header)) {
    if (isTRUE(title)) {
      heading <- paste0(attr(data, "heading"), collapse = "")
      heading <- gsub("\n\n", "\n", heading)
      heading <- sub("\n$", "", heading)

      if (!is.null(labs)) {
        header <- c(info_lang[["method"]], info_lang[["header"]],
          gsub("\n", " ", labs))
      } else {
        header <- c(info_lang[["method"]], info_lang[["header"]])
      }

      for (i in seq_len(length(header)))
        heading <- gsub(names(header)[i], header[i], heading, fixed = TRUE)


      if (length(heading) == 1) {
        ft <- add_header_lines(ft, values = heading)
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft, values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }

  # Add information on the p.value (with internal function)
  if (ncol_keys(ft) > ncol(data_t)) {
    ft <- .add_signif_stars(ft, j = "signif")
  }

  # Adjust cell with autofit()
  ft <- autofit(ft, part = c("header", "body"))

  if ("p.value" %in% names(data_t) && isTRUE(show.signif.stars))
      ft <- width(ft, j = "signif", width = 0.4)

  ft
}


#' Tidy version of the anova object into a flextable object
#'
#' @param data An **anova** object
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param auto.labs If `TRUE` (by default), use labels (and units) from
#'   `origdata=`.
#' @param origdata The original data set used for the ANOVA (used for changing
#'   the labels). By default, it is `NULL`.
#' @param labs Labels to use to change the names of elements in the `term`
#'   column. By default, it is `NULL`.
#' @param lang The natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is taken from `getOption("show.signif.stars")`.
#' @param ... Additional arguments (not used for now)
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate lazyeval expressions (not used
#'   for now)
#'
#' @return A **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy anova
#' @examples
#' iris_anova <- anova(lm(data = iris, Petal.Length ~ Species))
#' tabularise::tabularise$tidy(iris_anova)
tabularise_tidy.anova <- function(data, header = TRUE, title = header,
auto.labs = TRUE, origdata = NULL, labs = NULL,
lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE), ...,
kind = "ft", env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Select language
  info_lang <- .infos_lang.anova(lang = lang)

  # Extract labels
  if (isTRUE(auto.labs)) {
    labs <- tabularise:::.labels2(data, origdata = origdata, labs = labs)
  } else {
    labs <- tabularise:::.labels2(NULL, labs = labs)
  }

  # Turn an object into a tidy tibble
  data_t <- as.data.frame(broom::tidy(data))
  rownames(data_t) <- data_t$term

  if (isTRUE(show.signif.stars) && "p.value" %in% names(data_t)) {
    ft <- flextable(data_t, col_keys = c(names(data_t), "signif"))
  } else {
    ft <- flextable::flextable(data_t)
  }

  ft <- colformat_sci(ft)
  if ("p.value" %in% names(data_t))
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

  if (isTRUE(header)) {
    if (isTRUE(title)) {
      method <- info_lang[["method"]]
      headings <- attr(data, "heading")[1]

      res <- sapply(names(method), function(name)
        grepl(paste0("^",name), headings))
      method <- method[res]

      if (length(method) == 1) {
        ft <- add_header_lines(ft, values = method)
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft, values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }

  # Add information on the p.value
  if (ncol_keys(ft) > ncol(data_t))
    ft <- .add_signif_stars(ft, j = "signif")

  ft <- autofit(ft, part = c("header", "body"))

  if ("p.value" %in% names(data_t) && isTRUE(show.signif.stars))
      ft <- width(ft, j = "signif", width = 0.4)
  ft
}

#' Tidy version of the aov object into a flextable object
#'
#' @param data An **anova** object
#' @param ... Additional arguments passed to [tabularise_tidy.anova()]
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate the object.
#'
#' @return  **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy aov
#' @examples
#' iris_aov <- aov(data = iris, Petal.Length ~ Species)
#' tabularise::tabularise$tidy(iris_aov)
tabularise_tidy.aov <- function(data, ..., kind = "ft", env = parent.frame()) {
  tabularise_tidy(anova(data), ..., kind = kind, env = env)
}

# Choose the lang and the infos_lang
.infos_lang.anova <- function(lang) {
  lang <- tolower(lang)
  if (lang == "fr") {
    info_lang <- infos_fr.anova
  } else {# Only english for now as second alternative
    info_lang <- infos_en.anova
  }
  info_lang
}

infos_en.anova <- list(
  method = c(
    "Type III Analysis of Variance Table with Satterthwaite's method" =
      "Type III analysis of variance with Satterthwaite's method",
    "Analysis of Deviance Table" =
      "Analysis of deviance",
    "Analysis of Variance Table" =
      "Analysis of variance"
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
    Chisq = "$\\chi2_{obs.}$"),
  residuals = c(Residuals = "Residuals"),
  "NULL" = c("NULL" = "None"),
  Model = c("Model" = "Model")
)

infos_fr.anova <- list(
  method = c(
    "Type III Analysis of Variance Table with Satterthwaite's method" =
      "Analyse de la variance de type III avec m\u00e9thode Sattertwaite",
    "Analysis of Deviance Table" =
      "Analyse de la d\u00e9viance",
    "Analysis of Variance Table" =
      "Analyse de la variance"
  ),
  header = c(
    "Response:" = "R\u00e9ponse :",
    "Model:" = "Mod\u00e8le :",
    "Model" = "Mod\u00e8le",
    "link:" = "Lien :",
    "Terms added sequentially (first to last)" =
      "Termes ajout\u00e9s s\u00e9quentiellement (du premier au dernier)"
    ),
  labs = c(
    "term" = "Terme",
    "df.residual" = "Ddl des r\u00e9sidus",
    rss = "Somme des carr\u00e9s des r\u00e9sidus",
    "df" = "Ddl",
    "sumsq" = "Somme des carr\u00e9s",
    "meansq" = "Carr\u00e9s moyens",
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
    Chisq = "$\\chi2_{obs.}$"),
  residuals = c(Residuals = "R\u00e9sidus"),
  "NULL" = c("NULL" = "Aucun"),
  Model = c("Model" = "Mod\u00e8le")
)
