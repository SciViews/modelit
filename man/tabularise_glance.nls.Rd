% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.nls.R
\name{tabularise_glance.nls}
\alias{tabularise_glance.nls}
\title{Glance version of the nls object into a flextable object}
\usage{
\method{tabularise_glance}{nls}(
  data,
  header = TRUE,
  lang = getOption("data.io_lang", "en"),
  ...,
  env = parent.frame()
)
}
\arguments{
\item{data}{a nls object}

\item{header}{If TRUE, add a title to the table.}

\item{lang}{The language to use. The default value can be set with, e.g., options(data.io_lang = "fr") for French.}

\item{...}{Additional arguments. Not used.}

\item{env}{The environment where to evaluate formulas (you probably do not need to change the default).
#' @seealso \code{\link[tabularise:tabularise_glance]{tabularise::tabularise_glance()}} \code{\link[=tabularise_coef.summary.nls]{tabularise_coef.summary.nls()}}}
}
\description{
Extract the information contained in a nls object in one row of a rectangular table as it could be obtained by \code{\link[generics:glance]{generics::glance()}}. Here, the table is nicely formatted as an (almost) publication-ready form (good for informal reports, notebooks, etc).
}
\examples{
data("ChickWeight", package = "datasets")
chick1 <- ChickWeight[ChickWeight$Chick == 1, ]

# Adjust a logistic curve
chick1_logis <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = chick1)

library(tabularise)
tabularise$glance(chick1_logis)
tabularise$glance(chick1_logis, lang = "fr")

}