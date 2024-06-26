% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabularise.nls.R
\name{equation.nls}
\alias{equation.nls}
\alias{equation.summary.nls}
\title{Get a LaTeX equation from an nls or the summary of a nls models}
\usage{
\method{equation}{nls}(
  object,
  ital_vars = FALSE,
  use_coefs = FALSE,
  coef_digits = 2L,
  fix_signs = TRUE,
  var_names = NULL,
  op_latex = c("\\\\cdot", "\\\\times"),
  ...
)

\method{equation}{summary.nls}(
  object,
  ital_vars = FALSE,
  use_coefs = FALSE,
  coef_digits = 2L,
  fix_signs = TRUE,
  var_names = NULL,
  op_latex = c("\\\\cdot", "\\\\times"),
  ...
)
}
\arguments{
\item{object}{An \strong{nls} or \strong{summary.nls} object.}

\item{ital_vars}{Logical, defaults to \code{FALSE}. Should the variable names not
be wrapped in the \verb{\\operatorname} command?}

\item{use_coefs}{Logical, defaults to \code{FALSE}. Should the actual model
estimates be included in the equation instead of math symbols? If \code{TRUE},
\verb{var_names=} is ignored.}

\item{coef_digits}{Integer, defaults to 2. The number of decimal places to
round to when displaying model estimates with \code{use_coefs = TRUE}.}

\item{fix_signs}{Logical, defaults to \code{TRUE}. If disabled, coefficient
estimates that are negative are preceded with a \code{+} (e.g. \code{5(x) + -3(z)}).
If enabled, the \verb{+ -} is replaced with a \code{-} (e.g. \code{5(x) - 3(z)}).}

\item{var_names}{A named character vector as \code{c(old_var_name = "new name")}}

\item{op_latex}{The LaTeX product operator character to use in fancy
scientific notation, either \verb{\\\\cdot} (by default), or \verb{\\\\times}.}

\item{...}{Additional arguments (not used yet).}
}
\value{
A character string with a LaTeX equation.
}
\description{
Create the model equation of several self-starting nonlinear
models available in the stats package.
}
\examples{
equation <- tabularise::equation
chick1 <- ChickWeight[ChickWeight$Chick == 1, ]
chick1_nls <- nls(data = chick1, weight ~ SSlogis(Time, Asym, xmid, scal))
summary(chick1_nls)

equation(chick1_nls)
equation(summary(chick1_nls))

chick1_nls2 <- nls(data = chick1,
  weight ~ SSlogis(Time, Asym = A, xmid = x, scal = scale))
summary(chick1_nls2)

equation(chick1_nls2)
equation(summary(chick1_nls2))

equation(summary(chick1_nls2), var_names = c(
  weight = "Body weight [gm]",
  Time = "Number of days"))
}
