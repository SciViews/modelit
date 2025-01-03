# modelit 1.4.6

-   Better selection of X and Y variables in `chart.nls()` and `autoplot.nls()`.

-   Bug correction in `tabularise()` for **lm** and **glm** object. Managing a conflict between `auto.labs=` and `equation=`.

# modelit 1.4.5

-   Remotes SciViews/equatiomatic instead of datalorax/equatiomatic for correct compilation on R-Universe (no remote dependency required to yonicd/texPreview).

# modelit 1.4.4

-   Argument `kind =` added to all `tabularise()` methods and dependency update to v0.6.0 of {tabularise}.

-   Enhanced vignette and README file.

-   License switched to MIT.

# modelit 1.4.3

-   Lack of `...` argument in `equation.nl()` and `equation.summary.nls()` was causing a warning in `R CMD check`. Corrected now.

# modelit 1.4.2

-   Bug correction in `autoplot.nls()`: X and Y axes were inverted. Also the deprecated `aes_string()` function is replaced with `aes()` with proper arguments.

# modelit 1.4.1

-   Slight adjustments to have a space between the label and the units in equations, and to allow beta instead of beta_1 if there is no beta_0 or beta_2 in the same equations and in the tables from `tabularise()`.

# modelit 1.4.0

-   Use of the generic `equation()`. `equation_nls()` is transformed into an `equation()` method for **nls** and **summary.nls** objects. Implementation of `ital_vars=`, `use_coefs=`, `coef_digits=` and `fix_signs=` arguments in order to get parameterized as well as non parameterized equation, with or without italic for the variables.

-   Dependency to {equatiomatic} is dropped. It is now included in {tabularise}.

-   The default for `title=` argument is now automatically set to `FALSE` if the table is generated inside a chunk that has `tbl-cap` YAML entry defined (thus replaced by that caption).

-   The `tabularise_default()` methods for **summary.lm** and **summary.glm** were lacking important information (they were identical to `tabularise_coef)` for the same objects. Corrected now.

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
