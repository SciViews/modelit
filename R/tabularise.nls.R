#' Create a rich-formatted table from a summary.nls object
#'
#' @description
#' Create a table of a **summary.nls** object. This table looks like the output
#' of [print.summary.nls()] but richly formatted. The [tabularise_coef()]
#' function offers more customization options for this object.
#'
#' @param data A **summary.nls** object.
#' @param header If `TRUE` (by default), add a header to the table
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX equation).
#' @param footer If `TRUE` (by default), add a footer to the table.
#' @param lang The language to use. The default value can be set with, e.g.
#'   `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars
#'   to the table.
#' @param ... Additional arguments (Not used).
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.nls
#' @seealso [tabularise::tabularise()], [tabularise::tabularise_tidy()],
#'   [tabularise_coef.summary.nls()]
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the {flextable} functions.
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
tabularise_default.summary.nls <- function(data, header = TRUE, title = NULL,
equation = header, footer = TRUE, lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE), ...,
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  ft <- tabularise_coef.summary.nls(data, header = header, title = title,
    equation = equation, lang = lang, show.signif.stars = show.signif.stars,
    env = env)

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  if (isTRUE(footer)) {
    footer <- info_lang[["footer"]]

    # Use the same rule of print.summary.nls
    digs <- max(3L, getOption("digits") - 3L)

    val <- paste(footer[["rse"]], ":", format(signif(data$sigma,
      digits = digs)), footer[["on"]], data$df[2], footer["df"])

    conv <- data$convInfo
    if (isTRUE(conv$isConv)) {
      convinfo <- c(
        paste(footer[["nbc"]], ":", conv$finIter),
        paste(footer[["ctol"]], ":", format(conv$finTol,
        digits = digs)))
      val <- c(val, convinfo)
    } else {
      val <- c(val, footer[["stop"]])
    }
    ft <- add_footer_lines(ft, top = FALSE, values = val)
    ft <- align(ft, i = seq_len(length(val)) + 1 , align = "left",
      part = "footer")
  }
  ft
}

#' Create a rich-formatted table using the table of coefficients of the summary.nls object
#'
#' @description
#' This function extracts and formats the table of coefficients from a
#' **summary.nls** object, similar to [stats::coef()], but in flextable object.
#'
#' @param data A **summary.nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param title If `TRUE`, add a title to the table header. Default to the same
#'   value than header, except outside of a chunk where it is `FALSE` if a table
#'   caption is detected (`tbl-cap` YAML entry).
#' @param equation Add equation of the model to the table. If `TRUE`,
#'   [equation()] is used. The equation can also be passed in the form of a
#'   character string (LaTeX equation).
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars
#'   to the table.
#' @param ... Additional arguments passed to [equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#' now).
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the {flextable} functions.
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
tabularise_coef.summary.nls <- function(data, header = TRUE, title = NULL,
equation = header, lang = getOption("data.io_lang", "en"),
show.signif.stars = getOption("show.signif.stars", TRUE), ...,
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Extract the coef and rework it to obtain a data.frame
  co <- as.data.frame(coef(data))
  res <- cbind(term = rownames(co), co)
  names(res) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  # Use flextable
  if (isTRUE(show.signif.stars)) {
    ft <- flextable(res, col_keys = c(names(res), "signif"))
  } else {
    ft <- flextable(res)
  }
  ft <- colformat_sci(ft)
  ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Change labels
  ft <- header_labels(ft, lang = lang)

  # Add information on the p.value
  if (ncol_keys(ft) > ncol(res))
    ft <- add_signif_stars(ft, j = "signif")

  # Add headers
  ft <- add_header_nls(ft, data = data, header = header, title = title,
    equation = equation, lang = lang, ...)

  ft <- autofit(ft, part = c("header", "body"))
  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  if (isTRUE(equation) | is.character(equation))
    ft <- italic(ft, j = "term",part = "body")

  ft
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
#'   character string (LaTeX equation).
#' @param footer If `TRUE` (by default), add a footer to the table.
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments. Not used.
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the {flextable} functions.
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
tabularise_default.nls <- function(data, header = TRUE, title = NULL,
equation = header, footer = TRUE, lang = getOption("data.io_lang", "en"),
..., env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  ft <- tabularise_coef.nls(data, header = header, title = title,
    equation = equation, lang = lang, env = env)

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  # Add footer
  if (isTRUE(footer)) {
    footer <- info_lang[["footer"]]

    # Use the same rule of print.summary.nls
    digs <- max(3L, getOption("digits") - 3L)

    val <- paste(footer[["rss"]], ":", format(signif(data$m$deviance(),
      digits = digs)))

    conv <- data$convInfo
    if (isTRUE(conv$isConv)) {
      convinfo <- c(
        paste(footer[["nbc"]], ":", conv$finIter),
        paste(footer[["ctol"]], ":", format(conv$finTol,
        digits = digs)))
      val <- c(val, convinfo)
    } else {
      val <- c(val, footer[["stop"]])
    }

    ft <- add_footer_lines(ft, top = FALSE, values = val)
    ft <- align(ft, align = "left", part = "footer")
  }

  autofit(ft, part = c("header", "body"))
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
#' @param equation If `TRUE` (by default), add the equation of the model
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments.
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @return A **flextable** object that you can print in different forms or
#' rearrange with the {flextable} functions.
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
tabularise_coef.nls <- function(data, header = TRUE, title = NULL,
equation = header, lang = getOption("data.io_lang", "en"), ...,
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  co <- data.frame(rbind(coef(data)))

  ft <- flextable(co) |>
    colformat_sci()

  # Add headers
  ft <- add_header_nls(ft, data = data, header = header, title = title,
    equation = equation, lang = lang)

  autofit(ft, part = c("header", "body"))
}

#' Tidy version of the nls object into a flextable object
#'
#' @description
#' Extract the information contained in a nls object into a rectangular table as
#' it could be obtained by [broom::tidy()]. Here, the table is nicely
#' formatted as an (almost) publication-ready form (good for informal reports,
#' notebooks, etc).
#'
#' @param data A **nls** object
#' @param ... arguments of [tabularise_coef.summary.nls()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#' @seealso [tabularise::tabularise()], [tabularise::tabularise_tidy()],
#'   [tabularise_coef.summary.nls()]
#' @return A **flextable** object that you can print in different forms or
#' rearrange with the {flextable} functions.
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
tabularise_tidy.nls <- function(data, ..., env = parent.frame()) {
  data <- summary(data)
  tabularise_coef.summary.nls(data, ..., env = env)
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
#' @param lang The language to use. The default value can be set with, e.g.,
#'   `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for
#'   now).
#'
#' @seealso [tabularise::tabularise_glance()], [tabularise_coef.summary.nls()]
#' @return A **flextable** object that you can print in different forms or
#'   rearrange with the {flextable} functions.
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
tabularise_glance.nls <- function(data, header = TRUE, title = NULL,
equation = header, lang = getOption("data.io_lang", "en"), ...,
env = parent.frame()) {

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  res <- summary(data)

  res1 <- data.frame(
    sigma = res$sigma, finTol = res$convInfo$finTol,
    logLik = as.numeric(stats::logLik(data)), AIC = stats::AIC(data),
    BIC = stats::BIC(data), deviance = stats::deviance(data),
    df.residual = stats::df.residual(data), nobs = stats::nobs(data))

  ft <- flextable(res1) |>
    colformat_sci()

  # Add labels
  ft <- header_labels(ft, lang = lang)

  # Add headers
  ft <- add_header_nls(ft, data = data,  header = header,
    title = title, equation = equation, lang = lang)

  autofit(ft)
}

#' Get a LaTeX equation from an nls or summary.nls models
#'
#' @description
#' Create the model equation of several self-starting nonlinear models
#' available in {stats}.
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
#' equation(summary(chick1_nls2), var_names = c(
#'   weight = "Body weight [gm]",
#'   Time = "Number of days"))
equation.nls <- function(object, ital_vars = FALSE, use_coefs = FALSE,
coef_digits = 2L, fix_signs = TRUE, var_names = NULL,
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

  if (!is.null(var_names)) {
    if (!is.character(var_names))
      stop("var_names is not character vector")
    if (is.null(names(var_names)))
      stop("var_names must be named character vector")
    if (any(names(var_names) %in% ""))
      stop("all elements must be named")

    for (i in 1:length(var_names))
      SSequation <- gsub(names(var_names)[i], var_names[i], SSequation)
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
coef_digits = 2L, fix_signs = TRUE, var_names = NULL,
op_latex = c("\\cdot", "\\times"), ...) {
  # Same as equation.nls()
  equation.nls(object, ital_vars = ital_vars, use_coefs = use_coefs,
    coef_digits = coef_digits, fix_signs = fix_signs,  var_names = var_names,
    op_latex = op_latex)
}

infos_en.nls <- list(
  labs = c(
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
    nobs = "N"),
  SS = c(
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
  ),
  footer = c(
    rss = "Residual sum-of-squares",
    rse = "Residual standard error",
    on = "on",
    df = "degrees of freedom",
    nbc = "Number of iterations to convergence",
    ctol =  "Achieved convergence tolerance",
    stop = "The model does not converge")
)

infos_fr.nls <- list(
  labs = c(
    term = "Terme",
    estimate = "Valeur estim\u00e9e",
    std.error = "Ecart type",
    statistic = "Valeur de *t*~obs.~",
    p.value = "Valeur de *p*",
    sigma = "Ecart type relatif",
    AIC = "AIC",
    df.residual = "Ddl",
    nobs = "N",
    statistic = "Valeur de *t*~obs.~",
    sigma = "Ecart type des r\u00e9sidus",
    finTol = "Tol\u00e9rance de convergence",
    logLik = "Log-vraisemblance",
    AIC = "AIC",
    BIC = "BIC",
    deviance = "D\u00e9viance",
    df.residual = "Ddl",
    nobs = "N"
  ),
  SS = c(
    SSasymp = "Mod\u00e8le non lin\u00e9aire de r\u00e9gression asymptotique (von Bertalanffy)",
    SSAsympOff = "Mod\u00e8le non lin\u00e9aire de r\u00e9gression asymptotique (von Bertalanffy)",
    SSasympOrig = "Mod\u00e8le non lin\u00e9aire de r\u00e9gression asymptotique forc\u00e9e \u00e0 l'origine (von Bertalanffy)",
    SSbiexp = "Mod\u00e8le non lin\u00e9aire biexponentiel",
    SSfol = "Mod\u00e8le non lin\u00e9aire \u00e0 compartiments du premier ordre",
    SSfpl = "Mod\u00e8le non lin\u00e9aire logistique \u00e0 quatre param\u00e8tres",
    SSgompertz = "Mod\u00e8le non lin\u00e9aire de Gompertz",
    SSlogis = "Mod\u00e8le non lin\u00e9aire logistique",
    SSmicmen = "Mod\u00e8le non lin\u00e9aire de Michaelis-Menten",
    SSweibull = "Mod\u00e8le non lin\u00e9aire de Weibull"
  ),
  footer = c(
    rss = "Somme des carr\u00e9s des r\u00e9sidus",
    rse = "Ecart type des r\u00e9sidus",
    on = "pour",
    df = "degr\u00e9s de libert\u00e9",
    nbc = "Nombre d'it\u00e9rations pour converger",
    ctol =  "Tol\u00e9rance atteinte \u00e0 la convergence",
    stop = "Le mod\u00e8le ne converge pas"
  )
)

# Internal function of flextable
pvalue_format <- function(x){
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", " **", "  *", "  .", "   "))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

add_header_nls <- function(x, data,
  lang = lang, header = TRUE, title = NULL, equation = header, ...) {

  if (!inherits(x, "flextable"))
    stop(sprintf("Function `%s` supports only flextable objects.",
      "add_header_nls()"))

  # If title is not provided, determine if we have to use TRUE or FALSE
  if (missing(title)) {
    title <- header # Default to same as header, but...
    # if a caption is defined in the chunk, it defauts to FALSE
    if (!is.null(knitr::opts_current$get('tbl-cap')))
      title <- FALSE
  }

  # Choose the language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  ft <- x

  if (isTRUE(header)) {

    if (isTRUE(equation)) {
      ssequa <- equation(data, ...)
      ft <- add_header_lines(ft, values = as_paragraph(as_equation(ssequa)))
      ft <- align(ft, i = 1, align = "right", part = "header")
    } else if (is.character(equation)) {
      ft <- add_header_lines(ft, values = as_paragraph(as_equation(equation)))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

    if (isTRUE(title)) {
      ss <- info_lang[["SS"]]
      rhs <- as.character(rlang::f_rhs(formula(data)))[1]
      if (!is.na(ss[rhs])) {
        ft <- add_header_lines(ft, values = ss[rhs])
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    } else if (is.character(title)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
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

# Internal function to change the labels (accept markdown)
header_labels <- function(x, lang, ...) {

  if (!inherits(x, "flextable"))
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels()"))

  # Choose thev language
  lang <- tolower(lang)

  if (lang != "fr")
    lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  ft <- x

  labels_auto <- info_lang[["labs"]]
  labels_red <- labels_auto[names(labels_auto) %in% ft$header$col_keys]

  for (i in seq_along(labels_red))
    ft <- mk_par(ft, i = 1, j = names(labels_red)[i],
      value = para_md(labels_red[i]), part = "header")

  ft
}

# Internal function to add pvalue signif
add_signif_stars <- function(x, i = NULL, j = NULL, part = "body",
align = "right", ...) {

  if (!inherits(x, "flextable"))
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels()"))

  ft <- x

  ft <- mk_par(ft, i = i,  j = j,
    value =  as_paragraph(pvalue_format(.data$p.value)))
  ft <- add_footer_lines(ft,
    values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
  ft <- align(ft, i = 1, align = align, part = "footer")

  ft
}
