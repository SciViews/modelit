#' Create a rich-formatted table using the coefficients of an lm object
#'
#' @description
#' This function extracts and formats the table of coefficients from an **lm**
#' object, similar to [stats::coef()], but in a rich-formatted table using
#' \{flextable\}.
#'
#' @param data An **lm** object
#' @param header Logical. If `TRUE` (`FALSE`by default), a header is added to
#'  the table. The header includes both the title and the equation (if
#'  applicable). If set to `FALSE`, neither the title nor the equation will be
#'  displayed in the table header, even if the `title` or `equation` parameters
#'  are provided.
#' @param title If `TRUE` (`FALSE`by default) , add a title to the table header.
#'  Default to the same value than header, except outside of a chunk where it is
#'   `FALSE` if a table caption is detected (`tbl-cap` YAML entry).
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
#' @param ... Additional arguments
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @return A **flextable** object that you can print in different formats
#'   (HTML, LaTeX, Word, PowerPoint) or rearrange with the \{flextable\}
#'   functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom knitr opts_current
#' @method tabularise_coef lm
#' @examples
#' data(iris)
#' # Fit a simple linear model: Petal.Length as a function of Sepal.Length
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$coef(iris_lm)
#'
#' # If the 'iris' dataset has labels and units, they can be used to enhance
#' # the output table
#' iris <- data.io::labelise(iris, self = FALSE, label = list(
#'     Sepal.Length = "Length of the sepals",
#'     Petal.Length = "Length of the petals",
#'     Species = "Species"), units = c(rep("cm", 4), NA))
#'
#' iris_lm1 <- lm(data = iris, Petal.Length ~ Sepal.Length + Species)
#' tabularise::tabularise$coef(iris_lm1)
#'
#' # The same table but without showing the model equation
#' tabularise::tabularise$coef(iris_lm, equation = FALSE)
#'
#' iris_lm2 <- lm(data = iris, Petal.Length ~ Sepal.Length * Species)
#' tabularise::tabularise$coef(iris_lm2)
#'
tabularise_coef.lm <- function(data, header = FALSE, title = header,
  equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
  lang = getOption("data.io_lang", default = Sys.getenv("LANGUAGE",unset = "en")),
  ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_lm(
    data, type = "coef", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = FALSE, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_lm, footer = FALSE)

  # formatted table ----
 format_table(df_list, kind = kind, header = header)
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
#' @return A **flextable** object that you can print in different formats (HTML,
#'   LaTeX, Word, PowerPoint) or rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @method tabularise_default lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise(iris_lm)
tabularise_default.lm <- function(data, ..., kind = "ft") {
  # Note: there isn't much in print(lm_obj) than the table of coefficients
  # so, we produce the same table as tabularise$coef() here
  tabularise_coef.lm(data = data, ..., kind = kind)
}

#' Tidy version of the lm object into a flextable object
#'
#' @description
#' Create a rich-formatted table with the 'tidy' information from an **lm**
#' object.
#'
#' @param data An **lm** object
#' @param header Logical. If `TRUE` (`TRUE`by default), a header is added to
#'  the table. The header includes both the title and the equation (if
#'  applicable). If set to `FALSE`, neither the title nor the equation will be
#'  displayed in the table header, even if the `title` or `equation` parameters
#'  are provided.
#' @param title If `TRUE` (by default) , add a title to the table header.
#'  Default to the same value than header, except outside of a chunk where it is
#'   `FALSE` if a table caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE`(by default): The equation is generated and added to the table
#'              header. Its parameters are also used in the "Term" column.
#'   - `FALSE`: No equation is generated or displayed, and its
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
tabularise_tidy.lm <- function(data, header = TRUE, title = header,
  equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
  conf.int = FALSE, conf.level = 0.95, lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_lm(
    data, type = "tidy", conf.int = conf.int, conf.level = 0.95,
    show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_lm, footer = FALSE, ...)


  # formatted table ----
 format_table(df_list, kind = kind, header = header)
}

#' Glance version of the lm object into a flextable object
#'
#' @description
#' Create a rich-formatted table with the 'glance' information from an **lm**
#' object.
#'
#' @param data An **lm** object
#' @param header Logical. If `TRUE` (`TRUE`by default), a header is added to
#'  the table. The header includes both the title and the equation (if
#'  applicable). If set to `FALSE`, neither the title nor the equation will be
#'  displayed in the table header, even if the `title` or `equation` parameters
#'  are provided.
#' @param title If `TRUE` (by default) , add a title to the table header.
#'  Default to the same value than header, except outside of a chunk where it is
#'   `FALSE` if a table caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE`(by default): The equation is generated and added to the table
#'              header. Its parameters are also used in the "Term" column.
#'   - `FALSE`: No equation is generated or displayed, and its
#'              parameters are not used in the "Term" column.
#'   - `NA`: The equation is generated but not displayed in the table header.
#'              Its parameters are used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'              the table header.
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
#'
#' @return A **flextable** object that you can print in different form or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @method tabularise_glance lm
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
#' tabularise::tabularise$glance(iris_lm)
tabularise_glance.lm <- function(data, header = TRUE, title = header,
  equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
  lang = getOption("data.io_lang", "en"), ..., kind = "ft") {
  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_lm(
    data, type = "glance", conf.int = FALSE, conf.level = 0.95,
    show.signif.stars = FALSE, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_lm, footer = FALSE, ...)

  # formatted table ----
 format_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table using the table of coefficients of the summary.lm object
#'
#' @param data An **summary.lm** object
#' @param header Logical. If `TRUE` (`TRUE`by default), a header is added to
#'  the table. The header includes both the title and the equation (if
#'  applicable). If set to `FALSE`, neither the title nor the equation will be
#'  displayed in the table header, even if the `title` or `equation` parameters
#'  are provided.
#' @param title If `TRUE` (by default) , add a title to the table header.
#'  Default to the same value than header, except outside of a chunk where it is
#'   `FALSE` if a table caption is detected (`tbl-cap` YAML entry).
#' @param equation Logical or character. Controls whether an equation is added
#' to the table header and how parameters are used. Accepted values are:
#'   - `TRUE`(by default): The equation is generated and added to the table
#'              header. Its parameters are also used in the "Term" column.
#'   - `FALSE`: No equation is generated or displayed, and its
#'              parameters are not used in the "Term" column.
#'   - `NA`: The equation is generated but not displayed in the table header.
#'              Its parameters are used in the "Term" column.
#'   - Character string: A custom equation is provided directly and added to
#'              the table header.
#' @param footer If `TRUE` (by default, it is FALSE), add a footer to the table.
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
#' #' @param footer If `FALSE` (by default), add a footer to the table.
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
tabularise_coef.summary.lm <- function(data, header = TRUE, title = header,
    equation = header, footer = FALSE, auto.labs = TRUE, origdata = NULL,
    labs = NULL, conf.int = FALSE, conf.level = 0.95,
    lang = getOption("data.io_lang", "en"),
    show.signif.stars = getOption("show.signif.stars", TRUE), ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_lm(
    data, type = "tidy", conf.int = conf.int, conf.level = conf.level,
    show.signif.stars = show.signif.stars, lang = lang, auto.labs = auto.labs,
    origdata = origdata, labs = labs, equation = equation, title = title,
    colnames = colnames_lm, footer = footer, ...)

  # formatted table ----
 format_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table from an summary.lm object
#'
#' @param data A **summary.lm** object
#' @param ... Additional arguments passed to [tabularise_coef.summary.lm()]
#' @param footer If `TRUE` (by default), add a footer to the table.
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
tabularise_default.summary.lm <- function(data, ..., footer = TRUE) {
  tabularise_coef.summary.lm(data = data, ..., footer = footer)
}


# A list of internals functions ------
colnames_lm <- c(
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
  "(Intercept)" = "Intercept")

.trads <- gettext(term = "Term",
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
        "header" = "Linear model",
        "(Intercept)" = "Intercept", lang = "fr")
#.trads

.pvalue_format <- function(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                           labels = c("***", " **", "  *", "  .", "   ")) {
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = breaks,
           labels = labels)
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

.extract_colnames <- function(df, labs, lang) {
  vec <- labs[names(labs) %in% names(df)]
  vec1 <- gettext(vec, lang = lang)
  names(vec1) <- names(vec)

  #Remove elements with missing or empty names
  vec1 <- vec1[!is.na(names(vec1)) & names(vec1) != ""]

  vec1
}

.labels_factor <- function(df) {
  if (!is.data.frame(df)) {
    # warning("No dataframe found.")
    return(NULL)
    # stop("Input must be a data frame.")
  }

  factor_cols <- which(sapply(df, is.factor))
  if (length(factor_cols) == 0) {
    # warning("No factor columns found in the data frame.")
    return(NULL)
  }

  if (!requireNamespace("data.io", quietly = TRUE)) {
    stop("Package 'data.io' is required but not installed.")
  }
  #class(df)
  df <- as.data.frame(df)
  #class(df)
  labels <- vapply(df[,factor_cols, drop = FALSE], data.io::label, character(1), units = FALSE)
  valid_vars <- names(labels)[labels != ""]
  if (length(valid_vars) == 0) {
    #warning("No labeled factor variables found.")
    return(NULL)
  }

  # Fusion of result and names generation
  result <- vector("character")
  result_names <- vector("character")

  for (var in valid_vars) {
    levs <- levels(df[[var]])
    result <- c(result, paste0(labels[[var]], " [", levs, "]"))
    result_names <- c(result_names, paste0(var, levs))
  }

  names(result) <- result_names
  return(result)
}

.labels3 <- function (x, origdata = NULL, labs = NULL) {
  if (is.null(origdata)) {
    labs_auto <- c(tabularise:::.labels(x$model), .labels_factor(x$model))
  }
  else {
    labs_auto <- c(tabularise:::.labels(origdata), .labels_factor(origdata))
  }
  if (!is.null(labs)) {
    if (!is.character(labs))
      stop("labs is not character vector")
    if (is.null(names(labs)))
      stop("labs must be named character vector")
    if (any(names(labs) %in% ""))
      stop("all element must be named")
    labs_res <- c(labs, labs_auto[!names(labs_auto) %in%
                                    names(labs)])
  }
  else {
    labs_res <- labs_auto
  }
  labs_res
}

.extend_labs_with_interactions <- function(labs, terms) {
  if (!is.character(labs) || is.null(names(labs))) {
    return(NULL)
  }
  if (!is.character(terms)) {
    return(labs)
  }

  for (term in terms) {
    if (grepl(":", term)) {
      parts <- unlist(strsplit(term, ":"))
      missing_parts <- parts[!parts %in% names(labs)]

      if (length(missing_parts) > 0) {
        warning(sprintf(
          "The following terms are missing in 'labs' for the interaction '%s': %s",
          term, paste(missing_parts, collapse = ", ")
        ))
        next
      }

      interaction_label <- paste(labs[parts], collapse = " x ")
      labs[term] <- interaction_label
    }
  }
  labs <- gsub("\n", " ", labs)
  return(labs)
}

.extract_labels <- function(df, data, auto.labs, origdata, labs) {
  if (isTRUE(auto.labs)) {
    labs <- .labels3(x = data, origdata = origdata, labs = labs)
    # Compare the names of labs with the rownames
    labs <- .extend_labs_with_interactions(labs = labs, terms = df[["term"]])
  } else {
    labs <- .labels3(x = NULL, labs = labs)
  }

  labs
}

.extract_terms <- function(df, labs, lang) {
  vals <- df[["term"]]
  terms <- labs[names(labs) %in% vals]

  if(any(vals == "(Intercept)"))
    terms <- c("(Intercept)"= gettext("Intercept", lang = lang)[[1]], terms)

  if(any(vals == "Residuals"))
    terms <- c(terms, "Residuals"= gettext("Residuals", lang = lang)[[1]])

  terms
}

.extract_equation <- function(data, equation, labs, ...) {

  if (!(is.logical(equation) || is.character(equation))) {
    stop("The 'equation' argument must be TRUE, FALSE, NA, or a character string.")
  }

  equa <- NULL

  if (isTRUE(equation) || is.na(equation)) {
   equa <-  try({
      if (!is.null(labs)) {
        tabularise::equation(data, swap_var_names = labs, ...)
      } else {
        tabularise::equation(data, auto.labs = FALSE, ...)
      }
    }, silent = TRUE)
   if (inherits(equa, "try-error"))
     equa <- NULL
  }

  if (is.character(equation)) {
    equa <- equation
  }

  equa
}




.params_equa <- function(x, intercept = "alpha", greek = "beta") {
  vals <- NULL

  if (intercept != greek && grepl(intercept, x)) {
    it <- paste0("\\\\", intercept)
    res <- regmatches(x, gregexpr(it, x))[[1]]
    vals <- paste0("$",res, "$")
  }

  if (grepl(greek, x)) {
    g <- paste0("\\\\", greek,"_\\{\\d*\\}")
    res <- regmatches(x, gregexpr(g, x))[[1]]
    res1 <- paste0("$",res, "$")
    vals <- c(vals, res1)
  }

  vals
}

.extract_title <- function(title, lang = "en", default = "Linear model") {
  res <- NULL

  if (isTRUE(title)) {
    res <- gettext(default, lang = lang)[[1]]
  }

  if (is.character(title)) {
    res <- title
  }
  return(res)
}

.extract_footer_lm <- function(data, lang) {
  digits <- max(3L, getOption("digits") - 3L)
  domain <- "R-modelit"
  res <- paste(gettextf("Residuals range: [%.*g, %.*g]",
              digits, min(data$residuals, na.rm = TRUE),
              digits, max(data$residuals, na.rm = TRUE),
              domain = domain, lang = lang),
        gettextf("Residuals standard error: %.*g on %.*g degrees of freedom",
              digits, data$sigma, digits, max(data$df),
              domain = domain, lang = lang),
        gettextf("Multiple *R*^2^: %.*g - adjusted *R*^2^: %.*g",
              digits, data$r.squared, digits, data$adj.r.squared,
              domain = domain, lang = lang),
        gettextf("*F*-statistic: %.*g on %.*g and %.*g df - *p* value: %s",
              digits, data$fstatistic[1L],
              digits, data$fstatistic[2L],
              digits, data$fstatistic[3L],
              format.pval(pf(data$fstatistic[1L], data$fstatistic[2L],
                            data$fstatistic[3L], lower.tail = FALSE)),
              domain = domain, lang = lang),
        sep = "\n")
  res
}

.extract_infos_lm <- function(data, type = "coef",
    conf.int = TRUE, conf.level = 0.95, show.signif.stars = getOption("show.signif.stars", TRUE),
    lang = "en", auto.labs = TRUE, origdata = NULL , labs = NULL, equation = TRUE,
    title = TRUE, colnames = colnames_lm , footer = FALSE, ...) {

  if (!inherits(data, c("lm", "summary.lm")))
    stop(".extract_infos_lm() can apply only lm and summary.lm object.")

  type <- match.arg(type, choices = c("coef", "glance", "tidy"))

  if (inherits(data, "summary.lm") && type == "coef") {
    message(".extract_infos_lm() cannot apply type = 'coef' to a summary.lm
            object. Use type = 'tidy' instead to extract a detailed coefficient table.")
    type <- "tidy"
    }

  df <- switch(type,
    coef = {df <- coef(data)
            data.frame(term = names(df), estimate = df)},
    glance = {df <- as.data.frame(broom::glance(x = data))
              rownames(df) <- df$term
              df
               },
    tidy = {df <- as.data.frame(broom::tidy(x = data, conf.int = conf.int,
                                                 conf.level = conf.level))
            rownames(df) <- df$term

            if (isTRUE(conf.int)) {
                df <- df[, c("term", "estimate", "conf.low", "conf.high",
                                "std.error", "statistic", "p.value")]
                }
            if (isTRUE(show.signif.stars)) {
                df$signif <- .pvalue_format(df$p.value)
                 }
              df}
  )


  if(isTRUE(show.signif.stars)) {
    psignif <- "0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"
  } else {
    psignif <- NULL
  }

  lang <- tolower(lang)
  cols <- .extract_colnames(df, labs = colnames_lm, lang = lang)

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

  title <- .extract_title(title, lang = lang, default = "Linear model")

   if(isTRUE(footer)) {
     footer <- .extract_footer_lm(data, lang = lang)
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

.create_flextable <- function(x, header = TRUE) {
  df <- x$df

  ft <- flextable(df) |>
    colformat_sci()

  if ("p.value" %in% colnames(df)) {
    ft <- ft |>
      colformat_sci(j = "p.value", lod = 2e-16)
  }

  if (!is.null(x$cols)) {
    ft <- .add_colnames(ft, x$cols)
  }

  if (!is.null(x$terms)) {
    vec <- x$terms
    if(is.character(vec) && !is.null(names(vec)) && all(nzchar(names(vec)))) {
      ft <- .add_labs(ft, vec)
    } else {
      ft <- .add_params(ft, vec)
      }
    }

  if (isTRUE(header)) {
    ft <- .add_header2(ft, title = x$title, equation = x$equa)
  }

  # footer and psignif
  n <- 0 # use to define align right and left

  if (!is.null(x$psignif)) {
    ft <- .add_signif(ft, x$psignif)
    n <- 1
  }

  if (!is.null(x$footer)) {
    vals <- x$footer
    ft <- add_footer_lines(ft, top = FALSE, values = para_md(vals))
    ft <- align(ft, i = seq_len(length(vals)) + n , align = "left",
              part = "footer")
  }

  ft <- autofit(ft, part = c("header", "body"))

  if (!is.null(df$signif)) {
    ft <- width(ft, j = "signif", width = 0.4)
  }

  return(ft)
}

format_table <- function(df, kind, header) {
  switch(kind,
         df = {df},
         tt = {
           stop("Not implemented yet")
         },
         ft = {
           .create_flextable(df, header = header)
         },
         gt = {
           stop("Not implemented yet")
         }
  )
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

.add_header2 <- function(x, title, equation) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      ".add_header2()")) }

  ft <- x

  if (is.character(equation)) {
    ft <- add_header_lines(ft,
      values = as_paragraph(as_equation(equation)))
    ft <- align(ft, i = 1, align = "center", part = "header")
  }

  if (is.character(title)) {
    ft <- add_header_lines(ft,
      values = as_paragraph(title))
    ft <- align(ft, i = 1, align = "center", part = "header")
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


# # TODO: Migrate this translation system into the 'svMisc' package
#
# # This function creates a translation handler that caches translations
# # for different languages and object types (e.g., "lm", "nls", etc.).
# # It avoids re-evaluating translation expressions by storing results
# # in a cache (.trad), indexed by a key combining language and type.
# .make_translation <- function() {
#   .trad <- list()  # Internal cache for translations
#
#   translation_fun <- structure(function(expr, lang = NULL, type = "lm", clear_cache = FALSE) {
#     # Clear the cache if requested
#     if (isTRUE(clear_cache)) {
#       .trad <<- list()
#       if (missing(expr))
#         return()
#     }
#
#     # Try to extract language from the expression if not explicitly provided
#     if (is.null(lang)) {
#       lang <- substitute(expr)[["lang"]]
#       if (is.null(lang))
#         stop("lang is not defined")
#     }
#
#     # Create a cache key based on language and type
#     slot <- paste(lang, type[[1]], sep = "-")
#     res <- .trad[[slot]]
#
#     # If translation is not cached, evaluate and store it
#     if (is.null(res)) {
#       message("Language ", lang, " not found in cache")
#       res <- eval(expr)
#       .trad2 <- .trad
#       .trad2[[slot]] <- res
#       .trad <<- .trad2  # Super assignment to update cache
#     }
#
#     res  # Return the cached or newly evaluated translation
#   }, class = c("function", "subsettable_type"))
#
#   translation_fun
# }
#
# # Create the translation handler
# .translation <- .make_translation()
#
# # Add translations for 'lm' objects in French and English
# .translation$lm(gettext(
#   term = "Term",
#   estimate = "Estimate",
#   conf.low = "Lower bound (CI)",
#   conf.high = "Upper bound (CI)",
#   std.error = "Standard Error",
#   t.value = "t value",
#   sigma = "RSE",
#   r.squared = "R^2^",
#   adj.r.squared = "Adj.R^2^",
#   AIC = "AIC",
#   BIC = "BIC",
#   deviance = "Deviance",
#   logLik = "Log-likelihood",
#   statistic = "*t* value",
#   p.value = "*p* value",
#   df = "Model df",
#   df.residual = "Residuals df",
#   nobs = "N",
#   "(Intercept)" = "Intercept",
#   lang = "fr"
# ))
#
# .translation$lm(gettext(
#   term = "Term",
#   estimate = "Estimate",
#   conf.low = "Lower bound (CI)",
#   conf.high = "Upper bound (CI)",
#   std.error = "Standard Error",
#   t.value = "t value",
#   sigma = "RSE",
#   r.squared = "R^2^",
#   adj.r.squared = "Adj.R^2^",
#   AIC = "AIC",
#   BIC = "BIC",
#   deviance = "Deviance",
#   logLik = "Log-likelihood",
#   statistic = "*t* value",
#   p.value = "*p* value",
#   df = "Model df",
#   df.residual = "Residuals df",
#   nobs = "N",
#   "(Intercept)" = "Intercept",
#   lang = "en"
# ))
#
# # Optional: Clear the cache
# #.translation(clear_cache = TRUE)
#
# # Access the internal translation cache
# environment(.translation)$.trad -> s
#
# # View cached translations for French 'lm' objects
# s$`fr-lm`
# s
