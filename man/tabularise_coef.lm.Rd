% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.lm.R
\name{tabularise_coef.lm}
\alias{tabularise_coef.lm}
\title{Create a rich-formatted table using the coefficients of an lm object}
\usage{
\method{tabularise_coef}{lm}(
  data,
  header = TRUE,
  title = NULL,
  equation = header,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  ...,
  kind = "ft",
  env = parent.frame()
)
}
\arguments{
\item{data}{An \strong{lm} object}

\item{header}{If \code{TRUE} (by default), add a header to the table}

\item{title}{If \code{TRUE}, add a title to the table header. Default to the same
value than header, except outside of a chunk where it is \code{FALSE} if a table
caption is detected (\code{tbl-cap} YAML entry).}

\item{equation}{If \code{TRUE} (by default), add a equation to the table header.
The equation can also be passed in the form of a character string.}

\item{auto.labs}{If \code{TRUE} (by default), use labels (and units) automatically
from data or \verb{origdata=}.}

\item{origdata}{The original data set this model was fitted to. By default it
is \code{NULL} and no label is used.}

\item{labs}{Labels to change the names of elements in the \code{term} column of
the table. By default it is \code{NULL} and nothing is changed.}

\item{lang}{The natural language to use. The default value can be set with,
e.g., \code{options(data.io_lang = "fr")} for French.}

\item{...}{Additional arguments}

\item{kind}{The kind of table to produce: "tt" for tinytable, or "ft" for
flextable (default).}

\item{env}{The environment where to evaluate lazyeval expressions (unused for
now).}
}
\value{
A \strong{flextable} object that you can print in different formats
(HTML, LaTeX, Word, PowerPoint) or rearrange with the \{flextable\}
functions.
}
\description{
This function extracts and formats the table of coefficients from an \strong{lm}
object, similar to \code{\link[stats:coef]{stats::coef()}}, but in a rich-formatted table using
\{flextable\}.
}
\examples{
iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length)
tabularise::tabularise$coef(iris_lm)
}
