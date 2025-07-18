#' Create a rich-formatted table from the summary of a nls object
#'
#' @description
#' Create a table of a **summary.nls** object. This table looks like the output
#' of [print.summary.nls()] but richly formatted. The [tabularise_coef()]
#' function offers more customization options for this object.
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param footer If `TRUE` (by default), add a footer to the table.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.nls
#' @seealso [tabularise::tabularise()], [tabularise::tabularise_tidy()],
#'   [tabularise_coef.summary.nls()]
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the \{flextable\} functions.
#' @export
#'
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#' chick1_logis_sum <- summary(chick1_logis)
#'
#' tabularise::tabularise(chick1_logis_sum)
#' tabularise::tabularise(chick1_logis_sum, footer = FALSE)
tabularise_default.summary.nls <- function(data, header = TRUE, title = header,
    equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
    lang = getOption("data.io_lang", "en"), footer = TRUE,
    show.signif.stars = getOption("show.signif.stars", TRUE), ...,
    kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "coef", show.signif.stars = show.signif.stars, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = footer)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table using the table of coefficients of the summary.nls object
#'
#' @description
#' This function extracts and formats the table of coefficients from a
#' **summary.nls** object, similar to [stats::coef()], but in flextable object.
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param footer If `FALSE` (by default), add a footer to the table.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#' chick1_logis_sum <- summary(chick1_logis)
#'
#' tabularise::tabularise$coef(chick1_logis_sum)
#' tabularise::tabularise$coef(chick1_logis_sum, header = FALSE, equation = TRUE)
tabularise_coef.summary.nls <- function(data, header = TRUE, title = header,
     equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
     lang = getOption("data.io_lang", "en"), footer = FALSE,
    show.signif.stars = getOption("show.signif.stars", TRUE), ...,
    kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "coef", show.signif.stars = show.signif.stars, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = footer)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table from a nls object
#'
#' @description
#' This method extracts and formats an **nls** object, similar to [print()], but
#' in flextable object.
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param footer If `TRUE` (by default, it is TRUE), add a footer to the table.
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#'
#' tabularise::tabularise(chick1_logis)
tabularise_default.nls <- function(data, header = TRUE, title = header,
    equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
    lang = getOption("data.io_lang", "en"), footer = TRUE, ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "coef", show.signif.stars = FALSE, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = footer)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)
}

#' Create a rich-formatted table using the coefficients of the nls object
#'
#' @description
#' This method extracts and formats the coefficients from an **nls** object,
#' similar to [stats::coef()], but in flextable object.
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param footer If `TRUE` (by default, it is TRUE), add a footer to the table.
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @return A **flextable** object that you can print in different forms or
#' rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#'
#' tabularise::tabularise$coef(chick1_logis)
tabularise_coef.nls <- function(data, header = TRUE, title = header,
    equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
    lang = getOption("data.io_lang", "en"), footer = TRUE, ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "coef", show.signif.stars = FALSE, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = footer)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)

}

#' Tidy version of the nls object into a flextable object
#'
#' @description
#' Extract the information contained in a nls object into a rectangular table as
#' it could be obtained by [broom::tidy()]. Here, the table is nicely
#' formatted as an (almost) publication-ready form (good for informal reports,
#' notebooks, etc).
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE`, add the significance stars to the table.
#'   The default is `getOption("show.signif.stars")`
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @seealso [tabularise::tabularise()], [tabularise::tabularise_tidy()],
#'   [tabularise_coef.summary.nls()]
#' @return A **flextable** object that you can print in different forms or
#' rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#'
#' tabularise::tabularise$tidy(chick1_logis)
#' tabularise::tabularise$tidy(chick1_logis, lang = "fr")
tabularise_tidy.nls <- function(data, header = TRUE, title = header,
       equation = header, auto.labs = TRUE, origdata = NULL, labs = NULL,
       lang = getOption("data.io_lang", "en"),
       show.signif.stars = getOption("show.signif.stars", TRUE), ...,
       kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "tidy", show.signif.stars = show.signif.stars, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = FALSE)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)
}

#' Glance version of the nls object into a flextable object
#'
#' @description
#' Extract the information contained in an **nls** object in a table as it could
#' be obtained by [broom::glance()]. Here, the table is nicely formatted as an
#' (almost) publication-ready form (good for informal reports, notebooks, etc).
#'
#' @param data An **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX).
#' @param auto.labs If `TRUE` (by default), use labels (and units) automatically
#'   from data or `origdata=`.
#' @param origdata The original data set this model was fitted to. By default it
#'   is `NULL` and no label is used (only the name of the variables).
#' @param labs Labels to change the names of elements in the `term` column of
#'   the table. By default it is `NULL` and nothing is changed.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param ... Not used
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#'
#' @seealso [tabularise::tabularise_glance()], [tabularise_coef.summary.nls()]
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the \{flextable\} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @importFrom rlang .data
#' @method tabularise_glance nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#'
#' tabularise::tabularise$glance(chick1_logis)
#' tabularise::tabularise$glance(chick1_logis, lang = "fr")
tabularise_glance.nls <- function(data, header = TRUE, title = header,
    equation = header, auto.labs = TRUE, origdata = NULL,
    labs = NULL, lang = getOption("data.io_lang", "en"), ..., kind = "ft") {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  df_list <- .extract_infos_nls(
    data, type = "glance", show.signif.stars = FALSE, lang = lang,
    auto.labs = auto.labs, origdata = origdata, labs = labs, equation = equation,
    title = title, colnames = colnames_nls, footer = FALSE)
  # print(df_list) # use only for development

  # formatted table ----
  formate_table(df_list, kind = kind, header = header)
}

#' Get a LaTeX equation from an nls or the summary of a nls models
#'
#' @description Create the model equation of several self-starting nonlinear
#' models available in the stats package.
#'
#' @param object An **nls** or **summary.nls** object.
#' @param ital_vars Logical, defaults to `FALSE`. Should the variable names not
#'   be wrapped in the `\operatorname` command?
#' @param use_coefs Logical, defaults to `FALSE`. Should the actual model
#'   estimates be included in the equation instead of math symbols? If `TRUE`,
#'   `var_names=` is ignored.
#' @param coef_digits Integer, defaults to 2. The number of decimal places to
#'   round to when displaying model estimates with `use_coefs = TRUE`.
#' @param fix_signs Logical, defaults to `TRUE`. If disabled, coefficient
#'   estimates that are negative are preceded with a `+` (e.g. `5(x) + -3(z)`).
#'   If enabled, the `+ -` is replaced with a `-` (e.g. `5(x) - 3(z)`).
#' @param swap_var_names A named character vector as `c(old_var_name = "new name")`
#' @param var_names A named character vector as `c(old_var_name = "new name")`
#' @param op_latex 	The LaTeX product operator character to use in fancy
#'   scientific notation, either `\\cdot` (by default), or `\\times`.
#' @param ... Additional arguments (not used yet).
#'
#' @return A character string with a LaTeX equation.
#' @export
#' @method equation nls
#'
#' @examples
#' equation <- tabularise::equation
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' chick1_nls <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
#' summary(chick1_nls)
#'
#' equation(chick1_nls)
#' equation(summary(chick1_nls))
#'
#' chick1_nls2 <- nls(data = chick1,
#'   weight ~ SSlogis(Time, Asym = A, xmid = x, scal = scale))
#' summary(chick1_nls2)
#'
#' equation(chick1_nls2)
#' equation(summary(chick1_nls2))
#'
#' equation(summary(chick1_nls2), swap_var_names = c(
#'   weight = "Body weight [gm]",
#'   Time = "Number of days"))
equation.nls <- function(object, ital_vars = FALSE, use_coefs = FALSE,
coef_digits = 2L, fix_signs = TRUE, swap_var_names = NULL, var_names = swap_var_names,
op_latex = c("\\cdot", "\\times"), ...) {
  x <- object
  if (!class(x) %in% c("nls", "summary.nls"))
    stop("x must be an nls or summary.nls object")

  res <- try(stats::formula(x), silent = TRUE)

  if (inherits(res, "try-error"))
    stop("An error occurred when trying to extract the formula from 'x'")

  frhs <- as.character(rlang::f_rhs(res))
  flhs <- as.character(rlang::f_lhs(res))

  if (!grepl(frhs[1], pattern = "^SS"))
    stop("The formula is not a Self-Starting nls formula")

  if (length(flhs) != 1)
    stop("The formula is not a Self-Starting nls formula")

  SSequation <- c(
      SSasymp =
        "\\begin{aligned} \\operatorname{_y_} = _Asym_ + (_R0_ - _Asym_) \\cdot e^{(-e^{(_lrc_)} \\cdot \\operatorname{_input_)}} + \\epsilon \\end{aligned}",
      SSasympOff =
        "\\begin{aligned} \\operatorname{_y_} = _Asym_ \\cdot (1 - e^{(-e^{_lrc_} \\cdot (\\operatorname{_input_} - _c0_))}) + \\epsilon \\end{aligned}",
      SSasympOrig =
        "\\begin{aligned} \\operatorname{_y_} = _Asym_ \\cdot (1 - e^{(-e^{_lrc_} \\cdot \\operatorname{_input_})}) + \\epsilon \\end{aligned}",
      SSbiexp =
        "\\begin{aligned} \\operatorname{_y_} = _A1_ \\cdot e^{(-e^{_lrc1_} \\cdot \\operatorname{_input_})} + _A2_ \\cdot e^{(-e^{_lrc2_} \\cdot \\operatorname{_input_})} + \\epsilon \\end{aligned}",
      SSgompertz =
        "\\begin{aligned} \\operatorname{_y_} = _Asym_ \\cdot e^{(-_b2_ \\cdot _b3_^{\\operatorname{_x_}})} + \\epsilon \\end{aligned}",
      SSfol =
        "\\begin{aligned} \\operatorname{_y_} = _Dose_ \\cdot e^{{_lKe_ + _lKa_ - _lCl_)} \\cdot \\frac{e^{(-e^{_lKe_} \\cdot \\operatorname{_input_})} - e^{(-e^{_lKa_} \\cdot \\operatorname{_input_})}}{(e^{_lKa_} - e^{_lKe_})} + \\epsilon \\end{aligned}",
      SSlogis =
        "\\begin{aligned} \\operatorname{_y_} = \\frac{_Asym_}{1 + e^{(_xmid_ - \\operatorname{_input_}) /_scal_}} + \\epsilon \\end{aligned}",
      SSfpl =
      "\\begin{aligned} \\operatorname{_y_} = \\frac{_A_ + (_B_ - _A_)}{1 + e^{(_xmid_ - \\operatorname{_input_}) /_scal_}} + \\epsilon \\end{aligned}",
      SSmicmen =
        "\\begin{aligned} \\operatorname{_y_} = \\frac{_Vm_ \\cdot \\operatorname{_input_}}{(_K_+\\operatorname{_input_})} + \\epsilon \\end{aligned}",
      SSweibull =
        "\\begin{aligned} \\operatorname{_y_} = _Asym_ - _Drop_ \\cdot e^{-e^{_lrc_} \\cdot \\operatorname{_x_}^{_pwr_}} + \\epsilon \\end{aligned}"
    )

  SSequation <- SSequation[[frhs[1]]]
  if (is.null(SSequation))
    stop(sprintf("The %s is not available.", frhs[1]))

  if (isTRUE(ital_vars)) # Take all \\operatorname{...} out of the equation
    SSequation <- gsub("\\\\operatorname\\{([^}]+)\\}", "\\1", SSequation)

  SSvars <- list(
    SSasymp =  c("_input_"),
    SSasympOff = c("_input_"),
    SSasympOrig = c("_input_"),
    SSbiexp = c("_input_"),
    SSgompertz = c("_x_"),
    SSfol = c("_Dose_", "_input_"),
    SSlogis = c("_input_"),
    SSfpl = c("_input_"),
    SSmicmen = c("_input_"),
    SSweibull = c("_x_")
  )

  SSvars <- SSvars[[frhs[1]]]

  SSparams <- list(
    SSasymp =  c("_Asym_", "_R0_", "_lrc_"),
    SSasympOff = c("_Asym_", "_lrc_", "_c0_"),
    SSasympOrig = c("_Asym_", "_lrc_"),
    SSbiexp = c("_A1_", "_lrc1_", "_A2_", "_lrc2"),
    SSgompertz = c("_Asym_", "_b2_", "_b3_"),
    SSfol = c("_lKe_", "_lKa_", "_lCl_"),
    SSlogis = c("_Asym_", "_xmid_", "_scal_"),
    SSfpl = c("_A_", "_B_", "_xmid_", "_scal_"),
    SSmicmen = c("_Vm_", "_K_"),
    SSweibull = c("_Asym_", "_Drop_", "_lrc_", "_pwr_")
  )

  SSparams <- SSparams[[frhs[1]]]

  if (inherits(x, 'nls')) {
    names_coefs <- names(coef(x))
  } else {# summary.nls object
    names_coefs <- rownames(coef(x))
  }

  if (isTRUE(use_coefs)) {
    # We use \widehat{y} instead of y and get rid of + \epsilon at the end
    SSequation <- gsub("_y_", "\\widehat{_y_}", SSequation, fixed = TRUE)
    SSequation <- sub("\\+ +\\\\epsilon", "", SSequation)

    if (inherits(x, 'nls')) {
      coefs <- coef(x)
    } else {# summary.nls object
      coefs <- coef(x)[, 1L]
    }
    coefs <- round(coefs, coef_digits)
  } else {# Use names
    coefs <- names_coefs
  }

  names(names_coefs) <- SSparams
  names(coefs) <- SSparams


  xvars <- frhs[-1]
  xvars <- xvars[!xvars %in% names_coefs]
  names(xvars) <- SSvars

  yvar <- flhs
  names(yvar) <- "_y_"

  vals <- c(yvar, xvars, coefs)

  op_latex <- match.arg(op_latex, choices = c("\\cdot", "\\times"))

  if (op_latex != "\\cdot")
    vals <- c("\\cdot" = op_latex, vals)

  # TODO : This loop must be optimised
  for (i in 1:length(vals))
    SSequation <- gsub(names(vals)[i], vals[i], SSequation)

  if (!is.null(swap_var_names)) {
    if (!is.character(swap_var_names))
      stop("var_names is not character vector")
    if (is.null(names(swap_var_names)))
      stop("var_names must be named character vector")
    if (any(names(swap_var_names) %in% ""))
      stop("all elements must be named")

    swap_var_names <- gsub(" " , " \\\\\\\\ ", swap_var_names)

    for (i in 1:length(var_names))
      SSequation <- gsub(names(swap_var_names)[i], swap_var_names[i], SSequation)
  }

  # Possibly fix signs
  if (isTRUE(use_coefs) && isTRUE(fix_signs)) {
    SSequation <- gsub("\\+ +\\-", " - ", SSequation)
    SSequation <- gsub("\\- +\\-", " + ", SSequation)
  }

  class(SSequation) <- c("equation", "character")
  SSequation
}

#' @rdname equation.nls
#' @export
#' @method equation summary.nls
equation.summary.nls <- function(object, ital_vars = FALSE, use_coefs = FALSE,
coef_digits = 2L, fix_signs = TRUE, swap_var_names = NULL,
op_latex = c("\\cdot", "\\times"), ...) {
  # Same as equation.nls()
  equation.nls(object, ital_vars = ital_vars, use_coefs = use_coefs,
    coef_digits = coef_digits, fix_signs = fix_signs,
    swap_var_names = swap_var_names, op_latex = op_latex)
}


.trad <- gettext(
  term = "Term",
  estimate = "Estimate",
  std.error = "Standard Error",
  statistic = "*t*~obs.~ value",
  p.value = "*p* value",
  sigma = "Relative standard error",
  finTol = "Convergence tolerance",
  logLik = "Log-Likelihood",
  AIC = "AIC",
  BIC = "BIC",
  deviance = "Deviance",
  df.residual = "df",
  nobs = "N",
  SSasymp = "Nonlinear least squares asymptotic regression model (von Bertalanffy)",
  SSAsympOff = "Nonlinear least squares  asymptotic regression model (von Bertalanffy)",
  SSasympOrig = "Nonlinear least squares asymptotic regression model through the origin (von Bertalanffy)",
  SSbiexp = "Nonlinear least squares biexponential model",
  SSfol = "Nonlinear least squares  first-order compartment model",
  SSfpl = "Nonlinear least squares  four-parameter logistic model",
  SSgompertz = "Nonlinear least squares  Gompertz model",
  SSlogis = "Nonlinear least squares  logistic model",
  SSmicmen = "Nonlinear least squares Michaelis-Menten model",
  SSweibull = "Nonlinear least squares Weibull model")

colnames_nls <- c(
  term = "Term",
  estimate = "Estimate",
  std.error = "Standard Error",
  statistic = "*t*~obs.~ value",
  p.value = "*p* value",
  sigma = "Relative standard error",
  finTol = "Convergence tolerance",
  logLik = "Log-Likelihood",
  AIC = "AIC",
  BIC = "BIC",
  deviance = "Deviance",
  df.residual = "df",
  nobs = "N")

model_nls <- c(
  SSasymp = "Nonlinear least squares asymptotic regression model (von Bertalanffy)",
  SSAsympOff = "Nonlinear least squares  asymptotic regression model (von Bertalanffy)",
  SSasympOrig = "Nonlinear least squares asymptotic regression model through the origin (von Bertalanffy)",
  SSbiexp = "Nonlinear least squares biexponential model",
  SSfol = "Nonlinear least squares  first-order compartment model",
  SSfpl = "Nonlinear least squares  four-parameter logistic model",
  SSgompertz = "Nonlinear least squares  Gompertz model",
  SSlogis = "Nonlinear least squares  logistic model",
  SSmicmen = "Nonlinear least squares Michaelis-Menten model",
  SSweibull = "Nonlinear least squares Weibull model"
)

# Internal function for nls and summary.nls object

.extract_footer_nls <- function(data, lang) {
   digits <- max(3L, getOption("digits") - 3L)
   domain <- "R-modelit"

   if (inherits(data, "nls")) {
     val <- gettextf("Residual sum-of-squares: %.*g",
              digits, data$m$deviance(), domain = domain, lang = lang)
   } else {
     val <- gettextf("Residuals standard error: %.*g on %.*g degrees of freedom",
                     digits, data$sigma, digits, max(data$df),
                     domain = domain, lang = lang)
   }

   conv <- data$convInfo
   if (isTRUE(conv$isConv)) {
       convinfo <- paste(
         gettextf("Number of iterations to convergence: %.*g",
                  digits, conv$finIter, domain = domain, lang = lang),
         gettextf("Achieved convergence tolerance: %.*g",
                  digits, conv$finTol, domain = domain, lang = lang),
         sep = "\n")
       val <- c(val, convinfo)
       } else {
         val <- c(val, gettext("The model does not converge", lang = lang))
         }
   return(val)
}

.extract_infos_nls <- function(data, type = "coef",
    show.signif.stars = getOption("show.signif.stars", TRUE), lang = "en",
    colnames = colnames_nls, auto.labs = TRUE, origdata = NULL , labs = NULL,
    equation = TRUE, title = TRUE, footer = TRUE) {

  if (!inherits(data, c("nls", "summary.nls")))
      stop(".extract_infos_nls() can apply only nls and summary.nls object.")

  type <- match.arg(type, choices = c("coef", "glance", "tidy"))

  # Extract df ---------------------------------------------------------------
  if (inherits(data, "nls")) {
    df <- switch(type,
      coef = as.data.frame(t(coef(data))),
      glance = {
        res <- summary(data)
        res1 <- data.frame(
          sigma = res$sigma, finTol = res$convInfo$finTol,
          logLik = as.numeric(stats::logLik(data)), AIC = stats::AIC(data),
          BIC = stats::BIC(data), deviance = stats::deviance(data),
          df.residual = stats::df.residual(data), nobs = stats::nobs(data))
        res1
      },
      tidy = {
        res <- summary(data)
        res1 <- coef(res)
        df <- data.frame(term = rownames(res1), estimate = res1)
        names(df) <- c("term", "estimate", "std.error", "statistic", "p.value")

        if (isTRUE(show.signif.stars)) {
          df$signif <- .pvalue_format(df$p.value)
        }

        df
      }
    )
  } else {
    # only for summary.nls oject
    if(type == "glance") {
      stop(".extract_infos_nls() cannot apply type = 'glance' to a summary.nls
            object.")
    }
    res1 <- coef(data)
    df <- data.frame(term = rownames(res1), estimate = res1)
    names(df) <- c("term", "estimate", "std.error", "statistic", "p.value")

    if (isTRUE(show.signif.stars)) {
      df$signif <- .pvalue_format(df$p.value)
    }

    df
  }

  if(isTRUE(show.signif.stars)) {
    psignif <- "0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"
  } else {
    psignif <- NULL
  }

  lang <- tolower(lang)
  cols <- .extract_colnames(df, labs =  colnames, lang = lang)

  labels <- .extract_labels(df = df, data = data, auto.labs = auto.labs,
                            origdata = origdata, labs = labs)
  equa <- .extract_equation(data, equation = equation, labs = labels)

  terms <- NULL

  # title
  if (!isTRUE(title)) {
    title <- NULL
  }
  if (isTRUE(title)) {
    rhs <- as.character(rlang::f_rhs(formula(data)))[1]
    if (!is.na(model_nls[rhs])) {
      title <- gettext(model_nls[rhs], lang = lang)
    } else {
      title <- NULL
    }
  }

  if (is.character(title)) {
    title <- title
  }

  # footer
  if(isTRUE(footer)) {
    footer <- .extract_footer_nls(data, lang = lang)
  } else {
    footer <- NULL
  }

  # List with all elements
  list(
    df = df,
    title = title,
    cols = cols,
    equa = equa,
    terms = terms,
    psignif = psignif,
    footer = footer)
}

