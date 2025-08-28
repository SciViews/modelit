#' 'SciViews::R' - Statistical Models
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
## usethis namespace: start
#' @importFrom broom augment glance tidy
#' @importFrom chart chart combine_charts
#' @importFrom flextable add_footer_lines add_header_lines align as_equation
#' @importFrom flextable as_paragraph autofit border_inner_h flextable
#' @importFrom flextable hline italic mk_par ncol_keys nrow_part width
#' @importFrom generics fit
#' @importFrom ggplot2 aes aes_string geom_abline geom_bar geom_histogram geom_hline geom_point geom_qq geom_qq_line geom_smooth geom_vline ggtitle labs stat_function stat_smooth theme
#' @importFrom modelr add_predictions add_residuals geom_ref_line mae qae rmse rsquare
#' @importFrom officer fp_border
#' @importFrom stats AIC anova BIC coef confint cooks.distance deviance family fitted formula hatvalues nobs predict residuals rstandard variable.names vcov
#' @importFrom stats coef pf
#' @importFrom svFlow %>.%
#' @importFrom svBase label gettext_ gettextf_ ngettext_ stop_ warning_ prepare_data_dot recall_with_data_dot
#' @importFrom tabularise colformat_sci para_md tabularise
#' @importFrom equatiomatic equation
## usethis namespace: end
"_PACKAGE"
