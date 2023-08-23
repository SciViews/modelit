#' @title Tabularise a summary.nls object into a flextable object
#'
#' @description
#' This method tabularises a summary.nls object as a flextable.
#'
#' @param data A summary.nls object
#' @param header If TRUE add a title to the table
#' @param footer If TRUE add a footer to the table
#' @param lang The language to use. The default value can be set with, e.g., options(data.io_lang = "fr") for French.
#' @param show.signif.stars If TRUE add the significance stars to the table
#' @param ... Unused argument
#' @param env The environment where to evaluate the function
#' @importFrom tabularise tabularise_default
#' @importFrom rlang .data
#' @method tabularise_default summary.nls
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the {flextable} functions.
#' @export
#'
#' @examples
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#'
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' library(tabularise)
#' tabularise(summary(chick1_logis))
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

  if(isTRUE(header)) {
    header <- paste(deparse(data$formula), sep = "\n",
      collapse = "\n")
    ft <- add_header_lines(ft, values = header)
    ft <- align(ft, i = 1, align = "right", part = "header")
  }

  if(isTRUE(footer)) {
    footer <- info_lang[["footer"]]

    # use the same rule of print.summaru.nls
    digs <- max(3L, getOption("digits") - 3L)

    val <- paste(footer[["rse"]], ":", format(signif(data$sigma,
      digits = digs)) ,
      footer[["on"]], data$df[2], footer["df"])

    conv <- data$convInfo
    if(isTRUE(conv$isConv)){
      convinfo <- c(
        paste(footer[["nbc"]], conv$finIter),
        paste(footer[["ctol"]], format(conv$finTol,
          digits = digs))
      )
      val <- c(val, convinfo)
    }

    ft <- add_footer_lines(ft, top = FALSE, values = val)
    ft <- align(ft, i = seq_len(length(val))+1 , align = "left", part = "footer")
  }
  ft
}


#' @title Tabularise a table of coefficients from summary.nls object
#'
#' @description
#' This method extracts and formats the table of coefficients from summary.nls object, similar to [stats::coef()], but in flextable object.
#'
#' @param data A summary.nls object.
#' @param header If TRUE, add a title to the table.
#' @param lang The language to use. The default value can be set with, e.g., options(data.io_lang = "fr") for French.
#' @param show.signif.stars If TRUE, add the significance stars to the table.
#' @param ... Additional arguments. Not used.
#' @param env The environment where to evaluate formulas (you probably do not need to change the default).
#'
#' @export
#' @importFrom tabularise tabularise_coef
#' @importFrom rlang .data
#' @method tabularise_coef summary.nls
#' @examples
#' # example code
#' data("ChickWeight", package = "datasets")
#' chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' # Adjust a logistic curve
#' chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)
#' chick1_logis_sum <- summary(chick1_logis)
#' library(tabularise)
#' tabularise$coef(chick1_logis_sum)
#'
tabularise_coef.summary.nls <- function(
  data,
  header = TRUE,
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

  # Change labels
  labels_auto <- info_lang[["labs"]]
  labels_red <- labels_auto[names(labels_auto) %in% ft$header$col_keys]

  for(i in seq_along(labels_red))
    ft <- mk_par(ft, i = 1, j = names(labels_red)[i],
      value = para_md(labels_red[i]), part = "header")

  # Add information on the p.value ----
  if (ncol_keys(ft) > ncol(res)) {
    ft <- mk_par(ft, j = "signif", value =  as_paragraph(
      pvalue_format(.data$p.value)))
    ft <- add_footer_lines(ft,
      values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
    ft <- align(ft, i = 1, align = "right", part = "footer")
  }

  # Add headers ----
  if(isTRUE(header)) {
    ss <- info_lang[["SS"]]
    rhs <- as.character(rlang::f_rhs(data$formula))[1]

    if(!is.na(ss[rhs])) {
      ft <- add_header_lines(ft, values = ss[rhs])
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }

  # Adjust cell with autofit() ----
  ft <- autofit(ft, part = c("header", "body"))

  if (isTRUE(show.signif.stars))
    ft <- width(ft, j = "signif", width = 0.4)

  ft
}

#' @title Tidy version of the nls object into a flextable object
#'
#' @description
#' Extract the information contained in a nls object into a rectangular table as it could be obtained by [generics::tidy()]. Here, the table is nicely formatted as an (almost) publication-ready form (good for informal reports, notebooks, etc).
#'
#' @param data An nls object
#' @param ... arguments of [tabularise_coef.summary.nls()]
#' @seealso [tabularise::tabularise_tidy()] [tabularise_coef.summary.nls()]
#'
#' @export
#' @importFrom tabularise tabularise_tidy
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
#' @param data a nls object
#' @param header If TRUE, add a title to the table.
#' @param lang The language to use. The default value can be set with, e.g., options(data.io_lang = "fr") for French.
#' @param ... Additional arguments. Not used.
#' @param env The environment where to evaluate formulas (you probably do not need to change the default).
#' #' @seealso [tabularise::tabularise_glance()] [tabularise_coef.summary.nls()]
#'
#' @export
#' @importFrom tabularise tabularise_glance
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
#'
tabularise_glance.nls <- function(data,
  header = TRUE,
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

  labels_auto <- info_lang[["labs"]]
  labels_red <- labels_auto[names(labels_auto) %in% ft$header$col_keys]

  for(i in seq_along(labels_red))
    ft <- mk_par(ft, i = 1, j = names(labels_red)[i],
      value = para_md(labels_red[i]), part = "header")

  # Add headers ----
  if (isTRUE(header)){
    ss <- info_lang[["SS"]]
    rhs <- as.character(rlang::f_rhs(res$formula))[1]

    if (!is.na(ss[rhs])){
      ft <- add_header_lines(ft, values = ss[rhs])
      ft <- align(ft, i = 1, align = "right", part = "header")
    }
  }
  autofit(ft)
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
    SSgompertz = "Nonlinear least squares  Gompertz growth model",
    SSlogis = "Nonlinear least squares  logistic model",
    SSmicmen = "Nonlinear least squares Michaelis-Menten model",
    SSweibull = "Nonlinear least squares Weibull growth curve model"
  ),
  footer = c(
    rse = "Residual standard error",
    on = "on",
    df = "degrees of freedom",
    nbc = "Number of iterations to convergence",
    ctol =  "Achieved convergence tolerance")
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
    p.value = "*Valeur de *p*",
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
    SSgompertz = "Mod\u00e8le non lin\u00e9aire de croissance de Gompertz par les moindres carr\u00e9s",
    SSlogis = "Mod\u00e8le non lin\u00e9aire logistique par les moindres carr\u00e9s",
    SSmicmen = "Mod\u00e8le non lin\u00e9aire de Michaelis-Menten par les moindres carr\u00e9s ",
    SSweibull = "Mod\u00e8le non lin\u00e9aire de croissance de Weibull par les moindres carr\u00e9s"
  ),
  footer = c(
    rse = "Erreur standard r\u00e9siduel",
    on = "sur",
    df = "Degr\u00e9s de libert\u00e9",
    nbc = "Nombre d'it\u00e9ration pour converger",
    ctol =  "Tol\u00e9rance de convergence"
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

# Internal function to try to extract equation
nls_equation <- function(x) {

  if(!class(x) %in% c("nls", "summary.nls"))
    stop("x must be nls or summary.nls object")

  res <- try(stats::formula(x), silent = TRUE)

  if(inherits(res, "try-error"))
    stop("do not possible to extract formula")

  frhs <- as.character(rlang::f_rhs(res))
  flhs <- as.character(rlang::f_lhs(res))

  if (!grepl(frhs[1], pattern = "^SS"))
    stop("The formula is not a Self-Starting Nls formula")

  if (length(flhs) != 1)
    stop("The formula is not a Self-Starting Nls formula")

  SSequa <- SSequation[[frhs[1]]]
  SSpara <- SSparams[[frhs[1]]]

  if (methods::is(x, "nls")) {
    coefs <- names(coef(x))
  } else(
    coefs <- rownames(coef(x))
  )
  names(coefs) <- SSpara

  {
    frhs[-1] -> .
    .[!. %in% coefs] ->
      xvar
  }

  vals <- c("_YVAR_" = flhs , "_XVAR_" = xvar , coefs)
  #vals

  for (i in 1:length(vals))
    SSequa <- gsub(x = SSequa, pattern = names(vals)[i], replacement = vals[i])

  SSequa
}

SSparams = list(
  SSasymp =  c("_Asym_", "_R0_", "_lrc_"),
  SSasympOff = c("_Asym_", "_lrc_", "_c0_"),
  SSasympOrig = c("_Asym_", "_lrc_"),
  SSbiexp = c("_A1_", "_lrc1_", "_A2_", "_lrc2"),
  SSgompertz = c("_Asym_", "_b2_", "_b3_"),
  SSlogis = c("_Asym_", "_xmid_", "_scal_"),
  SSmicmen = c("_Vm_", "_K_"),
  SSweibull = c("_Asym_", "_Drop_", "lrc", "pwr")
)

SSequation = c(
  SSasymp =  "_YVAR_ = _Asym_+(_R0_ - _Asym_) \\times e^{(-e^{(_lrc_)} \\times _XVAR_)}",
  SSasympOff = "_YVAR_ = _Asym_ \\times (1 - e^{(-e^{_lrc_} \\times (_XVAR_ - _c0_))})",
  SSasympOrig = "_YVAR_ = _Asym_ \\times (1 - e^{(-e^{_lrc_} \\times _XVAR_)})",
  SSbiexp = "_YVAR_ = _A1_ \\times e^{(-e^{_lrc1_} \\times _XVAR_)} + _A2_ \\times e^{(-e^{_lrc1_} \\times _XVAR_)}",
  SSgompertz = "_YVAR_ = _Asym_ \\times e^{(-_b2_ \\times _b3_^{_XVAR_})}",
  SSlogis = "_YVAR_ = \\frac{_Asym_}{1 + e^{\\frac{_xmid_ - _XVAR_}{_scal_}}}",
  SSmicmen = "_YVAR_ = \\frac{_Vm_ \\times _XVAR_}{(_K_+_XVAR_)}",
  SSweibull = "_YVAR_ = _Asym_ - _Drop_ \\times e^{-e^{_lrc_} \\times _XVAR_^{_pwr_}}"
)
