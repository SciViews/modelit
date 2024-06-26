% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.glm.R
\name{tabularise_default.glm}
\alias{tabularise_default.glm}
\title{Create a rich-formatted table from a glm object}
\usage{
\method{tabularise_default}{glm}(
  data,
  footer = TRUE,
  lang = getOption("data.io_lang", "en"),
  ...,
  kind = "ft",
  env = parent.frame()
)
}
\arguments{
\item{data}{A \strong{glm} object}

\item{footer}{If \code{TRUE} (by default), add a footer to the table}

\item{lang}{The natural language to use. The default value can be set with,
e.g., \code{options(data.io_lang = "fr")} for French.}

\item{...}{Additional arguments passed to \code{\link[=tabularise_coef.glm]{tabularise_coef.glm()}}}

\item{kind}{The kind of table to produce: "tt" for tinytable, or "ft" for
flextable (default).}

\item{env}{The environment where to evaluate the model.}
}
\value{
A \strong{flextable} object is returned. You can print it in different
formats (HTML, LaTeX, Word, PowerPoint) or rearrange it with the
\{flextable\} functions.
}
\description{
Create a rich-formatted table from a glm object
}
\examples{
iris_glm <- glm(data = iris, Petal.Length ~ Sepal.Length)
tabularise::tabularise(iris_glm)
}
