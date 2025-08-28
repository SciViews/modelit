# modelit 1.4.8

-   Dependency to svBase and equatiomatic updated, and dependencies to data.io and svMisc eliminated.

-   Refactored all tabularise\_\*\*\*() methods for glm, summary.glm, anova, aov objects (e.g., tabularise_glance.glm(), tabularise_default.glm(), tabularise_tidy.glm(), etc.) to improve internal consistency and prepare for multi-format table rendering using {flextable}, {tinytable}, and {gt}.

-   added summary\_() and anova\_() functions. These two functions provide the same information as the summary() and anova() functions. They add an attribute that preserves a link to the object used in these two functions. Adding this argument makes it possible to retrieve information from the original object that would otherwise be lost when using summary() or anova().

-   Added the experimental glm\_() and nls\_() functions, which extend [stats::glm()] and [stats::nls()] by attaching additional metadata such as variable labels and units. The order of arguments has been modified to start with data =. All three functions — lm\_(), glm\_(), and nls\_() — use a data-dot mechanism inspired by svMisc::eval_data_dot(). Note: nls\_() uses model = TRUE by default, whereas the base nls() function uses model = FALSE.

-   The `equation=` argument now accepts `NA` in addition to `TRUE`, `FALSE`, and character strings, offering more flexibility in how model equations are displayed and used:

    -   `TRUE` *(default)*: The equation is automatically generated and added to the table header. Its parameters are also used in the "Term" column.

    -   `FALSE`: No equation is generated or displayed, and its parameters are not used in the "Term" column.

    -   `NA`: The equation is generated but not displayed in the table header. Its parameters are still used in the "Term" column.

    -   *Character string*: A custom equation is provided directly and added to the table header.

    This enhancement allows for finer control over equation display and ensures that model parameters remain accessible even when the equation is hidden.

# modelit 1.4.7

-   Refactored all tabularise\_\*\*\*() methods for lm, summary.lm, nls, summary.nls objects (e.g., tabularise_glance.lm(), tabularise_default.lm(), tabularise_tidy.lm(), etc.) to improve internal consistency and prepare for multi-format table rendering using {flextable}, {tinytable}, and {gt}.

-   Labels are now fully integrated into tabularise\_\*\*\*.nls and tabularise\_\*\*\*.summary.nls, allowing consistent and readable output for nonlinear model summaries.

-   Added the experimental lm\_() function, which extends [stats::lm()] by attaching additional metadata such as variable labels and units. Also introduced summary.lm\_() and anova.lm\_() methods to preserve the additional metadata.

# modelit 1.4.6

-   Bug correction in `tabularise()` for **lm** and **glm** object. Managing a conflict between auto.labs= and equation=.

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
