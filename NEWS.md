# modelit 1.4.0

-   Use of the generic `equation()`. `equation_nls()` is transformed into an `equation()` method for **nls** and **summary.nls** objects.

-   Dependency to {equatiomatic} is dropped. It is now included in {tabularise}.

-   The default for `title=` argument is now automatically set to `FALSE` if the table is generated inside a chunk that has `tbl-cap` YAML entry defined (thus replaced by that caption).

# modelit 1.3.0

-   `tabularise_default()` and `tabularise_tidy()` added for **anova** objects.

-   `tabularise_tidy()` added for **aov** objects.

# modelit 1.2.0

-   `tabularise_default()`, `tabularise_coef()`, `tabularise_glance()`, `tabularise_tidy()` for **lm** objects

-   `tabularise_default()`, `tabularise_coef()` for **summary.lm** objects

-   `tabularise_coef()`, `tabularise_glance()`, `tabularise_tidy()` for **glm** objects

-   `tabularise_coef()` for **summary.glm** objects

# modelit 1.1.0

-   `tabularise_default()`, `tabularise_coef()`, `tabularise_glance()`, `tabularise_tidy()` for **nls** objects

-   `tabularise_default()`, `tabularise_coef()` for **summary.nls** objects

# modelit 1.0.0

-   `autoplot()` and `chart()` for objects **lm**, **glm** and **nls**: both the plot of the model and several residual analyses, plus a composed 2x2 plot for residuals analysis.

-   `as.function()` for **lm** and **nls** objects.

-   Reexportation of several functions from {modelr}: `add_predictions()`, `add_residuals()`, `geom_ref_line()`, `rmse()`, `rsquare()`, `mae()` and `qae()`.

-   `fit_model()` to easily fit a {parsnip} model, and a series of methods like `summary()`, `anova()` or `chart()` directly implemented for these objects.

# modelit 0.1.0

-   Added hex sticker (logo) with:

``` r
SciViews::R
p <- chart(data = mtcars, wt ~ mpg) + geom_point()
p <- p + geom_smooth(method = "lm", formula = y ~ I(x^2) + x)
p <- p + theme_void() + hexSticker::theme_transparent()
dir.create ("inst/figures", showWarnings = FALSE)
hexSticker::sticker(p, package = "modelit", p_size = 9, s_x = 1, s_y = .75,
  s_width = 1.3, s_height = 0.9, h_fill="lightblue3", h_color="blue",
  filename = "inst/figures/modelit.png")
```

-   Added a `NEWS.md` file to track changes to the package.
