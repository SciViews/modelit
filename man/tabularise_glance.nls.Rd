% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.nls.R
\name{tabularise_glance.nls}
\alias{tabularise_glance.nls}
\title{Glance version of the nls object into a flextable object}
\usage{
\method{tabularise_glance}{nls}(
  data,
  header = TRUE,
  title = NULL,
  equation = header,
  lang = getOption("data.io_lang", "en"),
  ...,
  kind = "ft",
  env = parent.frame()
)
}
\arguments{
\item{data}{An \strong{nls} object.}

\item{header}{If \code{TRUE} (by default), add a title to the table.}

\item{title}{If \code{TRUE}, add a title to the table header. Default to the same
value than header, except outside of a chunk where it is \code{FALSE} if a table
caption is detected (\code{tbl-cap} YAML entry).}

\item{equation}{Add equation of the model to the table. If \code{TRUE},
\code{\link[=equation]{equation()}} is used. The equation can also be passed in the form of a
character string (LaTeX).}

\item{lang}{The language to use. The default value can be set with, e.g.,
\code{options(data.io_lang = "fr")} for French.}

\item{...}{Additional arguments passed to \code{\link[=equation]{equation()}}}

\item{kind}{The kind of table to produce: "tt" for tinytable, or "ft" for
flextable (default).}

\item{env}{The environment where to evaluate lazyeval expressions (unused for
now).}
}
\value{
A \strong{flextable} object that you can print in different forms or
rearrange with the \{flextable\} functions.
}
\description{
Extract the information contained in an \strong{nls} object in a table as it could
be obtained by \code{\link[broom:reexports]{broom::glance()}}. Here, the table is nicely formatted as an
(almost) publication-ready form (good for informal reports, notebooks, etc).
}
\examples{
data("ChickWeight", package = "datasets")
chick1 <- ChickWeight[ChickWeight$Chick == 1, ]

# Adjust a logistic curve
chick1_logis <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))

tabularise::tabularise$glance(chick1_logis)
tabularise::tabularise$glance(chick1_logis, lang = "fr")
}
\seealso{
\code{\link[tabularise:tabularise_glance]{tabularise::tabularise_glance()}}, \code{\link[=tabularise_coef.summary.nls]{tabularise_coef.summary.nls()}}
}
