#' @title Create a rich-formatted table from a summary.nls object
#'
#' @description
#' This function `tabularises_default()` tabularizes a **summary.nls** object. This table looks like the output of `print.summary.nls()` but richly formatted.
#' The [tabularise_coef()] function offers more customization options for this object.
#'
#' @param data A **summary.nls** object.
#' @param header If `TRUE` (by default), add a header to the table
#' @param footer If `TRUE` (by default), add a footer to the table.
#' @param lang The language to use. The default value can be set with, e.g. `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars to the table.
#' @param ... Additional arguments (Not used).
#' @param env The environment where to evaluate lazyeval expressions (unused for now).
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default summary.nls
#' @seealso [tabularise::tabularise()] [tabularise::tabularise_tidy()] [tabularise_coef.summary.nls()]
#'
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#'
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' slogis <- summary(chick1_logis)
#' # tabularise
#' # More practical
#' library(tabularise)
#' tabularise(slogis)
#' tabularise(slogis, footer = FALSE)
#'
tabularise_default.summary.nls <- function(data,
  header = TRUE,
  footer = TRUE,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  env = parent.frame()) {

  ft <- tabularise_coef.summary.nls(data,
    header = FALSE,
    equation = FALSE,
    lang = lang,
    show.signif.stars = show.signif.stars,
    env = parent.frame())

  # Choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  if (isTRUE(header)) {
    header <- paste(deparse(data$formula), sep = "\n",
      collapse = "\n")
    ft <- add_header_lines(ft, values = header)
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  if (isTRUE(footer)) {
    footer <- info_lang[["footer"]]

    # use the same rule of print.summaru.nls
    digs <- max(3L, getOption("digits") - 3L)

    val <- paste(footer[["rse"]], ":", format(signif(data$sigma,
      digits = digs)) ,
      footer[["on"]], data$df[2], footer["df"])

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
    ft <- align(ft, i = seq_len(length(val))+1 , align = "left", part = "footer")
  }
  ft
}

#' @title Create a rich-formatted table using the table of coefficients of the summary.nls object
#'
#' @description
#' This function extracts and formats the table of coefficients from a summary.nls object, similar to [stats::coef()], but in flextable object.
#'
#' @param data A **summary.nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param equation Add equation of the model to the table. If TRUE, [nls_equation()] is used. The equation can also be passed in the form of a character string.
#' @param lang The language to use. The default value can be set with, e.g., `options(data.io_lang = "fr")` for French.
#' @param show.signif.stars If `TRUE` (by default), add the significance stars to the table.
#' @param ... Additional arguments passed to [nls_equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for now).
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef summary.nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' slogis <- summary(chick1_logis)
#'
#' # More practical and pretty
#' library(tabularise)
#' tabularise$coef(slogis)
#' tabularise$coef(slogis, header = FALSE, equation = TRUE)
#'
tabularise_coef.summary.nls <- function(
  data,
  header = TRUE,
  equation = header,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  env = parent.frame()) {

  # Extract the coef and rework it to obtain a data.frame ----
  co <- as.data.frame(coef(data))
  res <- cbind(term = rownames(co), co)
  names(res) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # Choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  # Use flextable ----
  if (isTRUE(show.signif.stars)) {
    ft <- flextable(res, col_keys = c(names(res), "signif"))
  } else {
    ft <- flextable(res)
  }

  # Use tabularise with colformat_sci() ----
  ft <- colformat_sci(ft)
  ft <- colformat_sci(ft, j = "p.value", lod = 2e-16)

  # Change labels (with internal function) ----
  ft <- header_labels(ft, lang = lang)

  # Add information on the p.value (with internal function) ----
  if (ncol_keys(ft) > ncol(res)) {
    ft <- add_signif_stars(ft, j = "signif")
  }

  # Add headers (with internal function) ----
  ft <- add_header_nls(ft, data = data,
    header = header, equation = equation, lang = lang, ...)


  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  if(isTRUE(equation) | is.character(equation))
    ft <- italic(ft, j = "term",part = "body")

  ft
}

#' @title Create a rich-formatted table from a nls object
#'
#' @description
#' This method extracts and formats the nls object, similar to print(), but in flextable object.
#'
#' @param data A **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param footer If `TRUE` (by default), add a footer to the table.
#' @param lang The language to use. The default value can be set with, e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments. Not used.
#' @param env The environment where to evaluate lazyeval expressions (unused for now).
#'
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_default colformat_sci
#' @importFrom rlang .data
#' @method tabularise_default nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' library(tabularise)
#' tabularise(chick1_logis)
tabularise_default.nls <- function(data,
  header = TRUE,
  footer = TRUE,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()) {

  ft <- tabularise_coef.nls(data, header = FALSE, lang = lang)

  if (isTRUE(header)) {
    header <- paste("model :", deparse(formula(data)),
      collapse = "\n")
    ft <- add_header_lines(ft, values = header)
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  # choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  # Add footer
  if (isTRUE(footer)) {
    footer <- info_lang[["footer"]]

    # use the same rule of print.summaru.nls
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

#' @title Create a rich-formatted table using the coefficients of the nls object
#'
#' @description
#' This method extracts and formats the coefficients from nls object, similar to [stats::coef()], but in flextable object.
#'
#' @param data A **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param equation If `TRUE` (by default), add the equation of the model
#' @param lang The language to use. The default value can be set with, e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments.
#' @param env The environment where to evaluate lazyeval expressions (unused for now).
#'
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_coef colformat_sci
#' @importFrom rlang .data
#' @method tabularise_coef nls
#' @examples
#' # example code
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' library(tabularise)
#' tabularise$coef(chick1_logis)
tabularise_coef.nls <- function(data,
  header = TRUE,
  equation = header,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()) {

  # choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  co <- data.frame(rbind(coef(data)))

  ft <- flextable(co) |>
    colformat_sci()

  # Add headers (with internal function) ----
  ft <- add_header_nls(ft, data = data,
    header = header, equation = equation, lang = lang)

  autofit(ft, part = c("header", "body"))
}

#' @title Tidy version of the nls object into a flextable object
#'
#' @description
#' Extract the information contained in a nls object into a rectangular table as it could be obtained by [generics::tidy()]. Here, the table is nicely formatted as an (almost) publication-ready form (good for informal reports, notebooks, etc).
#'
#' @param data A **nls** object
#' @param ... arguments of [tabularise_coef.summary.nls()]
#' @seealso [tabularise::tabularise()] [tabularise::tabularise_tidy()] [tabularise_coef.summary.nls()]
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_tidy colformat_sci
#' @importFrom rlang .data
#' @method tabularise_tidy nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' library(tabularise)
#' tabularise$tidy(chick1_logis)
#' tabularise$tidy(chick1_logis, lang = "fr")
tabularise_tidy.nls <- function(data,
  ...) {
  data <- summary(data)
  tabularise_coef.summary.nls(data, ...)
}


#' @title Glance version of the nls object into a flextable object
#' @description
#' Extract the information contained in a nls object in one row of a rectangular table as it could be obtained by [generics::glance()]. Here, the table is nicely formatted as an (almost) publication-ready form (good for informal reports, notebooks, etc).
#'
#' @param data a **nls** object.
#' @param header If `TRUE` (by default), add a title to the table.
#' @param equation Add equation of the model to the table. If TRUE, [nls_equation()] is used. The equation can also be passed in the form of a character string.
#' @param lang The language to use. The default value can be set with, e.g., `options(data.io_lang = "fr")` for French.
#' @param ... Additional arguments passed to [nls_equation()]
#' @param env The environment where to evaluate lazyeval expressions (unused for now).
#'
#' @seealso [tabularise::tabularise_glance()] [tabularise_coef.summary.nls()]
#' @return A **flextable** object you can print in different forms or rearrange
#' with the {flextable} functions.
#' @export
#' @importFrom tabularise tabularise_glance colformat_sci
#' @importFrom rlang .data
#' @method tabularise_glance nls
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#'
#' library(tabularise)
#' tabularise$glance(chick1_logis)
#' tabularise$glance(chick1_logis, lang = "fr")
tabularise_glance.nls <- function(data,
  header = TRUE,
  equation = header,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()) {

  # choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

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

  # Add headers ----
  ft <- add_header_nls(ft, data = data,
    header = header, equation = equation, lang = lang)

  autofit(ft)
}

#' @title Extract equation of nls models
#'
#' @description
#' This function try to extract the equation of the Self-starting Nonlinear Models available in {stats}.
#'
#' @param x The nls object or summary.nls
#' @param var_names A named character vector as c("old_var_name" = "new name")
#' @param op_latex 	The operator character to use in fancy scientific notation
#'
#' @return a character value with latex equation
#' @export
#'
#' @examples
#' Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'fm1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
#'summary(fm1)
#'
#'nls_equation(fm1)
#'nls_equation(summary(fm1))
#'
#'fm1 <- nls(weight ~ SSlogis(Time, Asym = A, xmid = x, scal = scale), data = Chick.1)
#'summary(fm1)
#'
#'nls_equation(fm1)
#'nls_equation(summary(fm1))
#'
#'nls_equation(summary(fm1), var_names = c("weight" = "Body weight [gm]", "Time" = "Number of days"))
#'
nls_equation <- function(x,
  var_names = NULL,
  op_latex = c("\\cdot", "\\times")) {

  if(!class(x) %in% c("nls", "summary.nls"))
    stop("x must be nls or summary.nls object")

  res <- try(stats::formula(x), silent = TRUE)

  if(inherits(res, "try-error"))
    stop("Do not possible to extract formula")

  frhs <- as.character(rlang::f_rhs(res))
  flhs <- as.character(rlang::f_lhs(res))

  if (!grepl(frhs[1], pattern = "^SS"))
    stop("The formula is not a Self-Starting Nls formula")

  if (length(flhs) != 1)
    stop("The formula is not a Self-Starting Nls formula")

  SSequation <- c(
      SSasymp =  "_y_ = _Asym_+(_R0_ - _Asym_) \\cdot e^{(-e^{(_lrc_)} \\cdot _input_)} + \\epsilon",
      SSasympOff = "_y_ = _Asym_ \\cdot (1 - e^{(-e^{_lrc_} \\cdot (_input_ - _c0_))}) + \\epsilon",
      SSasympOrig = "_y_ = _Asym_ \\cdot (1 - e^{(-e^{_lrc_} \\cdot _input_)}) + \\epsilon",
      SSbiexp = "_y_ = _A1_ \\cdot e^{(-e^{_lrc1_} \\cdot _input_)} + _A2_ \\cdot e^{(-e^{_lrc1_} \\cdot _XVAR_)} + \\epsilon",
      SSgompertz = "_y_ = _Asym_ \\cdot e^{(-_b2_ \\cdot _b3_^{_x_})} + \\epsilon",
      SSfol = "_y_ = _Dose_ \\cdot e^{{_lKe_+_lKa_-_lCl_)} \\cdot \\frac{e^{(-e^{_lKe_} \\cdot _input_)} - e^{(-e^{_lKa_} \\cdot _input_)}}{(e^{_lKa_} - e^{_lKe_})} + \\epsilon",
      SSlogis = "_y_ = \\frac{_Asym_}{1 + e^{\\frac{_xmid_ - _input_}{_scal_}}} + \\epsilon",
      SSmicmen = "_y_ = \\frac{_Vm_ \\cdot _input_}{(_K_+_input_)} + \\epsilon",
      SSweibull = "_y_ = _Asym_ - _Drop_ \\cdot e^{-e^{_lrc_} \\cdot _x_^{_pwr_}} + \\epsilon"
    )

  SSequation <- SSequation[[frhs[1]]]
  if (is.null(SSequation))
    stop(sprintf("The %s is not available.", frhs[1]))

  SSvars <- list(
      SSasymp =  c("_input_"),
      SSasympOff = c("_input_"),
      SSasympOrig = c("_input_"),
      SSbiexp = c("_input_"),
      SSgompertz = c("_x_"),
      SSfol = c("_Dose_", "_input_"),
      SSlogis = c("_input_"),
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
    SSmicmen = c("_Vm_", "_K_"),
    SSweibull = c("_Asym_", "_Drop_", "_lrc_", "_pwr_"))

  SSparams <- SSparams[[frhs[1]]]

  if (methods::is(x, "nls")) {
    coefs <- names(coef(x))
  } else {
    coefs <- rownames(coef(x))
  }

  names(coefs) <- SSparams

  {
    frhs[-1] -> .
    .[!. %in% coefs] ->
      xvars
    names(xvars) <- SSvars
  }

  {
    yvar <- flhs
    names(yvar) <- "_y_"
  }

  vals <- c(yvar, xvars, coefs)
  # vals

  op_latex <- match.arg(op_latex, choices = c("\\cdot", "\\times"))

  if (op_latex != "\\cdot")
    vals <- c("\\cdot" = op_latex, vals)

  # TODO : This loop must be optimised
  for (i in 1:length(vals))
    SSequation <- gsub(x = SSequation, pattern = names(vals)[i], replacement = vals[i])

  if(!is.null(var_names)) {
    if (!is.character(var_names))
      stop("var_names is not character vector")
    if (is.null(names(var_names)))
      stop("var_names must be named character vector")
    if (any(names(var_names) %in% ""))
      stop("all elements must be named")

    for (i in 1:length(var_names))
      SSequation <- gsub(x = SSequation, pattern = names(var_names)[i], replacement = var_names[i])
  }

  SSequation
}

infos_en.nls <- list(
  labs = c(
    term = "Term",
    estimate = "Estimate",
    std.error = "Standard Error",
    statistic = "*t*~obs.~ value",
    p.value = "*p* value",
    sigma = "RSE",
    finTol = "Convergence tolerance",
    logLik = "Log-Likelihood",
    AIC = "AIC",
    BIC = "BIC",
    deviance = "Deviance",
    df.residual = "df",
    nobs = "N"),
  SS = c(
    SSasymp = "Nonlinear least squares asymptotic regression model",
    SSAsympOff = "Nonlinear least squares  asymptotic regression model with an Offset",
    SSasympOrig = "Nonlinear least squares asymptotic regression model through the Origin",
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
    stop = "The model do not converge")
)

infos_fr.nls <- list(
  labs = c(
    term = "Terme",
    estimate = "Valeur estim\u00e9e",
    std.error = "Erreur standard",
    statistic = "Valeur de *t*~obs.~",
    p.value = "Valeur de *p*",
    sigma = "RSE",
    AIC = "AIC",
    df.residual = "Ddl",
    nobs = "N",
    statistic = "Valeur de *t*~obs.~",
    sigma = "Erreur standard r\u00e9siduel",
    finTol = "Tol\u00e9rance de convergence",
    logLik = "Log-vraisemblance",
    AIC = "AIC",
    BIC = "BIC",
    deviance = "D\u00e9viance",
    df.residual = "Ddl",
    nobs = "N"
  ),
  SS = c(
    SSasymp = "Mod\u00e8le non lin\u00e9aire de r\u00e9gression asymptotique par les moindres carr\u00e9s",
    SSAsympOff = "Nonlinear least squares  asymptotic regression model with an Offset", # mod\u00e8le de von Bertalanffy
    SSasympOrig = "Mod\u00e8le non lin\u00e9aire de r\u00e9gression asymptotique passant par l'origine par les moindres carr\u00e9s", #mod\u00e8le de von Bertalanffy
    SSbiexp = "Mod\u00e8le non lin\u00e9aire biexponentiel par les moindres carr\u00e9s",
    SSfol = "Mod\u00e8le non lin\u00e9aire \u00e0 compartiments du premier ordre par les moindres carr\u00e9s",
    SSfpl = "Mod\u00e8le non lin\u00e9aire logistique \u00e0 quatre param\u00e8tres par les moindres carr\u00e9s",
    SSgompertz = "Mod\u00e8le non lin\u00e9aire de Gompertz par les moindres carr\u00e9s",
    SSlogis = "Mod\u00e8le non lin\u00e9aire logistique par les moindres carr\u00e9s",
    SSmicmen = "Mod\u00e8le non lin\u00e9aire de Michaelis-Menten par les moindres carr\u00e9s ",
    SSweibull = "Mod\u00e8le non lin\u00e9aire de Weibull par les moindres carr\u00e9s"
  ),
  footer = c(
    rss = "Somme des carr\u00e9s des r\u00e9sidus",
    rse = "Erreur standard r\u00e9siduel",
    on = "sur",
    df = "degr\u00e9s de libert\u00e9",
    nbc = "Nombre d'it\u00e9ration pour converger",
    ctol =  "Tol\u00e9rance de convergence",
    stop = "Le mod\u00e8le ne converge pas"
  )
)

# Internal function of flextable
pvalue_format <- function(x){
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels = c("***", " **", "  *", "  .", "   "))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

# Internal function to add header and equation
add_header_nls <- function(x, data, lang = lang, header = TRUE, equation = header, ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "add_header_nls()")) }

  # choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

  if (lang == "fr") {
    info_lang <- infos_fr.nls
  } else {
    info_lang <- infos_en.nls
  }

  ft <- x

  if (isTRUE(equation)) {
    ssequa <- nls_equation(data, ...)
    ft <- add_header_lines(ft,
      values = as_paragraph(
        as_equation(ssequa)
      ))
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  if (is.character(equation)) {
    ft <- add_header_lines(ft,
      values = as_paragraph(
        as_equation(equation)
      ))
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  if (isTRUE(header)) {
    ss <- info_lang[["SS"]]

    rhs <- as.character(rlang::f_rhs(formula(data)))[1]

    if(!is.na(ss[rhs])) {
      ft <- add_header_lines(ft, values = ss[rhs])
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

# Internal function to change the labels (accept markdown)
header_labels <- function(x, lang, ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels()")) }

  # choose de lang ----
  lang <- tolower(lang)

  if (lang != "fr") lang <- "en" # Only en or fr for now

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
