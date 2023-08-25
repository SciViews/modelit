#' @details
#' The {modelit} package provides an extension to base R functions for model
#' fitting like [lm()], [glm()] or [nls()] with enhanced plots and utilitarian
#' functions.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats AIC anova BIC coef confint cooks.distance deviance family fitted formula hatvalues nobs predict residuals rstandard variable.names vcov
#' @importFrom chart chart combine_charts
#' @importFrom ggplot2 aes aes_string geom_abline geom_bar geom_histogram geom_hline geom_point geom_qq geom_qq_line geom_smooth geom_vline ggtitle labs stat_function stat_smooth theme
#' @importFrom svFlow %>.%
#' @importFrom broom augment glance tidy
#' @importFrom modelr add_predictions add_residuals geom_ref_line mae qae rmse rsquare
#' @importFrom generics fit
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
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
#' @importFrom stats coef
#' @importFrom tabularise colformat_sci
#' @importFrom tabularise para_md
## usethis namespace: end
NULL
