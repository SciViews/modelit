# Internal function of flextable
.pvalue_format <- function(x){
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels = c("***", " **", "  *", "  .", "   "))
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

# Internal function to add pvalue signif
.add_signif_stars <- function(x, i = NULL, j = NULL, part = "body", align = "right", ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "add_signif_stars()"))}

  ft <- x

  ft <- mk_par(ft, i =i,  j = j, value =  as_paragraph(
    .pvalue_format(.data$p.value)))
  ft <- add_footer_lines(ft,
    values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
  ft <- align(ft, i = 1, align = align, part = "footer")

  ft
}


# Internal function : Extract labels and units  ----
.labels <- function(x, units = TRUE, ...) {
  labels <- sapply(x, data.io::label, units = units)

  if (any(labels != "")) {
    # Use a \n before labels and the units
    if (isTRUE(units))
      labels <- sub(" +\\[([^]]+)\\]$", "\n[\\1]", labels)
    # set names if empty
    labels[labels == ""] <- names(x)[labels == ""]
    # Specific case for I() using in a formula
    labels[grepl("^I\\(.*\\)$", names(labels))] <- names(labels)[grepl("^I\\(.*\\)$", names(labels))]
  }

  if (all(labels == "")) {
    labels <- NULL
  }
  labels
}

.labels2 <- function(x, origdata = NULL, labs = NULL) {

  labs_auto <- NULL
  labs_auto <- .labels(x$model)

  if (!is.null(origdata)) {
    labs_auto <- .labels(origdata)
  }

  if (!is.null(labs)) {
    if (!is.character(labs))
      stop("labs is not character vector")
    if (is.null(names(labs)))
      stop("labs must be named character vector")
    if (any(names(labs) %in% ""))
      stop("all element must be named")
    labs_res <- c(labs, labs_auto[!names(labs_auto) %in% names(labs)])
  } else {
    labs_res <- labs_auto
  }

  labs_res
}

# Internal function :Retrieve model parameters
.params_equa <- function(x, intercept = "alpha", greek = "beta") {
  vals <- NULL

  if (intercept != greek) {
    if (grepl(intercept, x)) {
      it <- paste0("\\\\", intercept)
      res <- regmatches(x,
        gregexpr(it, x))[[1]]

      vals <- paste0("$",res, "$")
    }
  }

  if (grepl(greek, x)) {
    g <- paste0("\\\\", greek,"_\\{\\d+\\}")
    res <- regmatches(x,
      gregexpr(g, x)
    )[[1]]
    res1 <- paste0("$",res, "$")
    vals <- c(vals, res1)
  }

  vals

}

# Internal function : Change labels of header
.header_labels <- function(x, info_lang, ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "header_labels_lm()")) }

  ft <- x

  hlabs <- info_lang[["labs"]]
  hlabs_red <- hlabs[names(hlabs) %in% ft$header$col_keys]

  for (i in seq_along(hlabs_red))
    ft <- mk_par(ft, i = 1, j = names(hlabs_red)[i],
      value = para_md(hlabs_red[i]), part = "header")
  ft
}

# Internal function : add header
.add_header <- function(x,
  info_lang,
  header = TRUE,
  title = header,
  equation,
  ...) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
      "add_header_lm()")) }

  ft <- x

  if (isTRUE(header)) {
    if (is.character(equation)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(
          as_equation(equation)
        ))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

    if (isTRUE(title)) {
      ft <- add_header_lines(ft, values = info_lang[["header"]])
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

    if (is.character(title)) {
      ft <- add_header_lines(ft,
        values = as_paragraph(title))
      ft <- align(ft, i = 1, align = "right", part = "header")
    }

  }

  h_nrow <- nrow_part(ft, part = "header")

  if (h_nrow > 2) {
    ft |>
      border_inner_h(border = officer::fp_border(width = 0), part = "header") |>
      hline(i= nrow_part(ft, "header")-1,
        border = officer::fp_border(width = 1.5, color = "#666666"),
        part = "header") ->
      ft
  }

  ft
}
