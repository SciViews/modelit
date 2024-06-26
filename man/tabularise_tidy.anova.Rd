% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.anova.R
\name{tabularise_tidy.anova}
\alias{tabularise_tidy.anova}
\title{Tidy version of the anova object into a flextable object}
\usage{
\method{tabularise_tidy}{anova}(
  data,
  header = TRUE,
  title = header,
  auto.labs = TRUE,
  origdata = NULL,
  labs = NULL,
  lang = getOption("data.io_lang", "en"),
  show.signif.stars = getOption("show.signif.stars", TRUE),
  ...,
  kind = "ft",
  env = parent.frame()
)
}
\arguments{
\item{data}{An \strong{anova} object}

\item{header}{If \code{TRUE} (by default), add a header to the table}

\item{title}{If \code{TRUE}, add a title to the table header. Default to the same
value than header, except outside of a chunk where it is \code{FALSE} if a table
caption is detected (\code{tbl-cap} YAML entry).}

\item{auto.labs}{If \code{TRUE} (by default), use labels (and units) from
\verb{origdata=}.}

\item{origdata}{The original data set used for the ANOVA (used for changing
the labels). By default, it is \code{NULL}.}

\item{labs}{Labels to use to change the names of elements in the \code{term}
column. By default, it is \code{NULL}.}

\item{lang}{The natural language to use. The default value can be set with,
e.g., \code{options(data.io_lang = "fr")} for French.}

\item{show.signif.stars}{If \code{TRUE}, add the significance stars to the table.
The default is taken from \code{getOption("show.signif.stars")}.}

\item{...}{Additional arguments (not used for now)}

\item{kind}{The kind of table to produce: "tt" for tinytable, or "ft" for
flextable (default).}

\item{env}{The environment where to evaluate lazyeval expressions (not used
for now)}
}
\value{
A \strong{flextable} object you can print in different form or rearrange
with the \{flextable\} functions.
}
\description{
Tidy version of the anova object into a flextable object
}
\examples{
iris_anova <- anova(lm(data = iris, Petal.Length ~ Species))
tabularise::tabularise$tidy(iris_anova)
}
