#' Statistical Models for 'SciViews::R'
#'
#' The \{modelit\} package provides an extension to base R functions for model
#' fitting like [lm()], [glm()] or [nls()] with enhanced plots and utilitarian
#' functions.
#'
#' @section Important functions:
#'
#' - [fit_model()] creates a **model_fit** object that has many methods.
#'
#' - [tabularise()] methods for **lm**, **glm**, **nls**, **model_fit**,
#' **anova** and **aov** objects.
#'
#' - [chart()] methods for **lm**, **glm**, **nls** and **model_fit** objects.
#'
#' - [as.function()] transforms an **lm** or **nls** model into a function that
#' can be plotted using `stat_function()`.
#'
#' @docType package
#' @name inferit-package

## usethis namespace: start
#' @importFrom stats AIC anova BIC coef confint cooks.distance deviance family fitted formula hatvalues nobs predict residuals rstandard variable.names vcov
#' @importFrom chart chart combine_charts
#' @importFrom ggplot2 aes aes_string geom_abline geom_bar geom_histogram geom_hline geom_point geom_qq geom_qq_line geom_smooth geom_vline ggtitle labs stat_function stat_smooth theme
#' @importFrom svFlow %>.%
#' @importFrom broom augment glance tidy
#' @importFrom modelr add_predictions add_residuals geom_ref_line mae qae rmse rsquare
#' @importFrom generics fit
#' @importFrom data.io label
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_lines
#' @importFrom flextable align
#' @importFrom flextable as_equation
#' @importFrom flextable as_paragraph
#' @importFrom flextable autofit
#' @importFrom flextable border_inner_h
#' @importFrom flextable flextable
#' @importFrom flextable hline
#' @importFrom flextable italic
#' @importFrom flextable mk_par
#' @importFrom flextable ncol_keys
#' @importFrom flextable nrow_part
#' @importFrom flextable width
#' @importFrom officer fp_border
#' @importFrom stats coef pf
#' @importFrom tabularise colformat_sci equation
#' @importFrom tabularise para_md
## usethis namespace: end
NULL
