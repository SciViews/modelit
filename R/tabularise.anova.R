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
#'   e.g., `options(SciViews_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is taken from `getOption("show.signif.stars")`.
#' @param ... Additional arguments (not used for now)
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @return  A **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default anova
#' @examples
#' is <- data.io::read("iris", package = "datasets")
#'
#' is_lm1 <- lm(data = is, petal_length ~ species)
#'
#' library(tabularise)
#'
#' anova(is_lm1) |> tabularise_default()
#' # identical
#' anova(is_lm1) |> tabularise()
#' # Use labels
#' anova(is_lm1) |> tabularise(origdata = is)
#'
#' # alternative with anova_() in {modelit} package
#' anova_(is_lm1) |> tabularise()
#'
#' is_lm2 <- lm(data = is, petal_length ~ sepal_length + species)
#'
#' anova(is_lm1, is_lm2) |> tabularise(origdata = is)
#' anova_(is_lm1, is_lm2) |> tabularise()
#'
tabularise_default.anova <- function(data, header = TRUE, title = header,
    auto.labs = TRUE, origdata = NULL, labs = NULL,
    lang = getOption("SciViews_lang", "en"),
    show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_anova(
    data, show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, title = title,
    colnames = colnames_anova)

  # formatted table
  format_table(df_list, kind = kind, header = header)
}

#' Tidy version of the anova object into a flextable object
#'
#' @param data An **anova** object
#' @param ... Additional arguments used [tabularise_default.anova()]
#'
#' @return A **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy anova
#' @examples
#' is <- data.io::read("iris", package = "datasets")
#'
#' is_lm1 <- lm(data = is, petal_length ~ species)
#'
#' library(tabularise)
#'
#' anova(is_lm1) |> tabularise_tidy()
#' # identical
#' anova(is_lm1) |> tabularise$tidy()
#' # Use labels
#' anova(is_lm1) |> tabularise$tidy(origdata = is)
#'
#' # alternative with anova_() in {modelit} package
#' anova_(is_lm1) |> tabularise$tidy()
tabularise_tidy.anova <- function(data,...) {
  tabularise_default.anova(data = data, ...)
}

#' Create a rich-formatted table from an aov object
#'
#' @param data An **aov** object
#' @param ... Additional arguments passed to [tabularise_default.anova()]
#' @return  **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default aov
#' @examples
#' iris_aov <- aov(data = iris, Petal.Length ~ Species)
#' tabularise::tabularise$tidy(iris_aov)
tabularise_default.aov <- function(data, ...) {
  tabularise_default.anova(anova_(data), ...)
}

#' Tidy version of the aov object into a flextable object
#'
#' @param data An **aov** object
#' @param ... Additional arguments passed to [tabularise_default.anova()]
#' @return  **flextable** object you can print in different form or rearrange
#'   with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_tidy aov
#' @examples
#' iris_aov <- aov(data = iris, Petal.Length ~ Species)
#' tabularise::tabularise$tidy(iris_aov)
tabularise_tidy.aov <- function(data, ...) {
  tabularise_default.anova(anova_(data), ...)
}

# A list of internals functions and objects

colnames_anova <-  c(
  term = "Term",
  "model" = "Model",
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
  residual.deviance = "Residual deviance",
  "F value" = "*F*~obs.~ value",
  "F" = "*F*~obs.~ value",
  Chisq = "$\\chi2_{obs.}$",
  signif = "",
  npar = "Number of parameters")

.trad_anova <- gettext(
  "Type III Analysis of Variance Table with Satterthwaite's method" =
      "Type III analysis of variance with Satterthwaite's method",
  "Analysis of Deviance Table" = "Analysis of deviance",
  "Analysis of Variance Table" = "Analysis of variance",
  "Anova Table (Type II tests)" = "Type II analysis of variance",
  "Anova Table (Type III tests)" = "Type III analysis of variance",
  "Analysis of Deviance Table (Type II tests)" = "Type II analysis of deviance table",
  "Analysis of Deviance Table (Type III tests)" = "Type III analysis of deviance table",
  "Response:" = "Response:",
  "Model:" = "Model:",
  "Model" = "Model",
  "link:" = "link:",
  "Terms added sequentially (first to last)" = "Terms added sequentially (first to last)",
  term = "Term",
  "model" = "Model",
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
  residual.deviance = "Residual deviance",
  "F value" = "*F*~obs.~ value",
  "F" = "*F*~obs.~ value",
  Chisq = "$\\chi2_{obs.}$",
  "NULL" = "None",
  Residuals = "Residuals",
  npar = "Number of parameters"
)

# See utils.R for internal functions used by various .extract_infos_***
#
.extract_infos_anova <- function(data,
    show.signif.stars = getOption("show.signif.stars", TRUE),
    lang = "en", auto.labs = TRUE, origdata = NULL , labs = NULL,
    title = TRUE, colnames = colnames_anova, ...) {

  if (!inherits(data, c("anova")))
    stop(".extract_infos_anova() can apply only anova object.")

  # df
  df <- as.data.frame(broom::tidy(data))
  rownames(df) <- df$term

  # statistic variable has 3 possible signification: "F value", "F", "Chisq"
  statistic_cols <- c("F value", "F", "Chisq")
  names(df)[names(df) == "statistic"] <-
    statistic_cols[statistic_cols %in% names(data)][1]

  # the term variable
  if (grepl("^Model", attr(data, "heading")[2]))
    names(df)[names(df) == "term"] <- "model"

  if (isTRUE(show.signif.stars))
    df$signif <- .pvalue_format(df$p.value)

  # psignif
  if (isTRUE(show.signif.stars)) {
    psignif <- "0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"
  } else {
    psignif <- NULL
  }

  lang <- tolower(lang)
  cols <- .extract_colnames(df, labs = colnames, lang = lang)

  data_obj <- attr(data, "object")

  if (is.null(data_obj)) {
    labels <- .extract_labels(df = df, data = data, auto.labs = auto.labs,
      origdata = origdata, labs = labs)
  } else {
    labels <- .extract_labels(df = df, data = data_obj, auto.labs = auto.labs,
      origdata = origdata, labs = labs)
  }

  if (is.null(df[["term"]])) {
    if (isTRUE(title)) {
      df$model <- paste(gettext("Model", lang = lang)[[1]], 1:nrow(df))
    } else {
      if (!is.null(labels)) {
        labels_red <- gsub("\n", " ", labels)
        for (i in seq_len(length(labels_red)))
          df$model<- gsub(names(labels_red)[i], labels_red[i], df$model, fixed = TRUE)
      }
    }
  } else {
    terms <- .extract_terms(df, labs = labels, lang = lang)
  }


  # extract title
  # Handle the 'heading' attribute if title is TRUE
  if (isTRUE(title)) {
    heading <- attr(data, "heading")
    heading <- unlist(strsplit(heading, "\n\n"))
    heading <- sub("\n$", "", heading)

    # Standardize heading if it starts with "Data"
    if (grepl("^Data", heading)[[1]]) {
      heading[[1]] <- "Analysis of Variance Table"

      # Format model headings if present
      if (grepl("^Models:", heading)[[2]]) {
        heading <- heading[-2]
        heading[-1] <- paste0("Model ", seq_along(heading[-1]), ": ", heading[-1])
      }
    }

    # Replace label names in heading if labels are provided
    if (!is.null(labels)) {
      labels_red <- gsub("\n", " ", labels)
      for (i in seq_len(length(labels_red)))
        heading <- gsub(names(labels_red)[i], labels_red[i], heading, fixed = TRUE)
    }

    # Map verbose headers to simplified versions
    header_anova <- c(
      "Type III Analysis of Variance Table with Satterthwaite's method" =
        "Type III analysis of variance with Satterthwaite's method",
      "Analysis of Deviance Table" = "Analysis of deviance",
      "Analysis of Variance Table" = "Analysis of variance",
      "Anova Table (Type II tests)" = "Type II analysis of variance",
      "Anova Table (Type III tests)" = "Type III analysis of variance",
      "Analysis of Deviance Table (Type II tests)" = "Type II analysis of deviance table",
      "Analysis of Deviance Table (Type III tests)" = "Type III analysis of deviance table",
      "Response:" = "Response:",
      "Model:" = "Model:",
      "Model" = "Model",
      "link:" = "link:",
      "Terms added sequentially (first to last)" = "Terms added sequentially (first to last)"
    )

    # Translate headers based on language
    header_anovat <- gettext(header_anova, lang = lang)
    names(header_anovat) <- names(header_anova)

    # Apply header replacements
    for (i in seq_len(length(header_anovat)))
      heading <- gsub(names(header_anovat)[i], header_anovat[i], heading, fixed = TRUE)

    title <- paste0(heading, collapse = "\n")

  } else {
    title <- NULL
  }

  # Preserve title if it's a character string
  if (is.character(title)) {
    title <- title
  }

  list(
    df      = df,
    title   = title,
    cols    = cols,
    equa    = NULL,
    terms   = terms,
    psignif = psignif,
    footer  = NULL)
}

