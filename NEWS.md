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
