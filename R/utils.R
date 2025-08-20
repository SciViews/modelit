# gettext(), stop(), warning()
gettext <- svMisc::gettext_
gettextf <- svMisc::gettextf_
ngettext <- svMisc::ngettext_
# stop <- svMisc::stop_ #
# warning <- svMisc::warning_

# Need this for R CMD check to pass
. <- NULL

# Internal functions for .extract_infos_***()
.pvalue_format <- function(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                           labels = c("***", " **", "  *", "  .", "   ")) {
  #x <- get(as.character(substitute(x)), inherits = TRUE)
  z <- cut(x, breaks = breaks,
           labels = labels)
  z <- as.character(z)
  z[is.na(x)] <- ""
  z
}

.extract_colnames <- function(df, labs, lang) {
  vec <- labs[names(labs) %in% names(df)]
  vec1 <- gettext(vec, lang = lang)
  names(vec1) <- names(vec)

  #Remove elements with missing or empty names
  vec1 <- vec1[!is.na(names(vec1)) & names(vec1) != ""]

  vec1
}

.labels_factor <- function(df) {
  if (!is.data.frame(df)) {
    # warning("No dataframe found.")
    return(NULL)
    # stop("Input must be a data frame.")
  }

  factor_cols <- which(sapply(df, is.factor))
  if (length(factor_cols) == 0) {
    # warning("No factor columns found in the data frame.")
    return(NULL)
  }

  if (!requireNamespace("data.io", quietly = TRUE)) {
    stop("Package 'data.io' is required but not installed.")
  }
  #class(df)
  df <- as.data.frame(df)
  #class(df)
  labels <- vapply(df[,factor_cols, drop = FALSE], data.io::label, character(1), units = FALSE)
  valid_vars <- names(labels)[labels != ""]
  if (length(valid_vars) == 0) {
    #warning("No labeled factor variables found.")
    return(NULL)
  }

  # Fusion of result and names generation
  result <- vector("character")
  result_names <- vector("character")

  for (var in valid_vars) {
    levs <- levels(df[[var]])
    result <- c(result, paste0(labels[[var]], " [", levs, "]"))
    result_names <- c(result_names, paste0(var, levs))
  }

  names(result) <- result_names
  return(result)
}

.labels3 <- function (x, origdata = NULL, labs = NULL) {
  if (is.null(origdata)) {
    labs_auto <- c(tabularise:::.labels(x$model), .labels_factor(x$model))
  }
  else {
    labs_auto <- c(tabularise:::.labels(origdata), .labels_factor(origdata))
  }
  if (!is.null(labs)) {
    if (!is.character(labs))
      stop("labs is not character vector")
    if (is.null(names(labs)))
      stop("labs must be named character vector")
    if (any(names(labs) %in% ""))
      stop("all element must be named")
    labs_res <- c(labs, labs_auto[!names(labs_auto) %in%
                                    names(labs)])
  }
  else {
    labs_res <- labs_auto
  }
  labs_res
}

.extend_labs_with_interactions <- function(labs, terms) {
  if (!is.character(labs) || is.null(names(labs))) {
    return(NULL)
  }
  if (!is.character(terms)) {
    return(labs)
  }

  for (term in terms) {
    if (grepl(":", term)) {
      parts <- unlist(strsplit(term, ":"))
      missing_parts <- parts[!parts %in% names(labs)]

      if (length(missing_parts) > 0) {
        warning(sprintf(
          "The following terms are missing in 'labs' for the interaction '%s': %s",
          term, paste(missing_parts, collapse = ", ")
        ))
        next
      }

      interaction_label <- paste(labs[parts], collapse = " x ")
      labs[term] <- interaction_label
    }
  }
  labs <- gsub("\n", " ", labs)
  return(labs)
}

.extract_labels <- function(df, data, auto.labs, origdata, labs) {
  if (isTRUE(auto.labs)) {
    labs <- .labels3(x = data, origdata = origdata, labs = labs)
    # Compare the names of labs with the rownames
    labs <- .extend_labs_with_interactions(labs = labs, terms = df[["term"]])
  } else {
    labs <- .labels3(x = NULL, labs = labs)
  }

  labs
}

.extract_terms <- function(df, labs, lang) {
  vals <- df[["term"]]
  terms <- labs[names(labs) %in% vals]

  if(any(vals == "(Intercept)"))
    terms <- c("(Intercept)"= gettext("Intercept", lang = lang)[[1]], terms)

  if(any(vals == "Residuals"))
    terms <- c(terms, "Residuals"= gettext("Residuals", lang = lang)[[1]])

  terms
}

.extract_equation <- function(data, equation, labs, ...) {

  if (!(is.logical(equation) || is.character(equation))) {
    stop("The 'equation' argument must be TRUE, FALSE, NA, or a character string.")
  }

  equa <- NULL

  if (isTRUE(equation) || is.na(equation)) {
    equa <-  try({
      if (!is.null(labs)) {
        tabularise::equation(data, swap_var_names = labs, ...)
      } else {
        tabularise::equation(data, auto.labs = FALSE, ...)
      }
    }, silent = TRUE)
    if (inherits(equa, "try-error"))
      equa <- NULL
  }

  if (is.character(equation)) {
    equa <- equation
  }

  equa
}

.params_equa <- function(x, intercept = "alpha", greek = "beta") {
  vals <- NULL

  if (intercept != greek && grepl(intercept, x)) {
    it <- paste0("\\\\", intercept)
    res <- regmatches(x, gregexpr(it, x))[[1]]
    vals <- paste0("$",res, "$")
  }

  if (grepl(greek, x)) {
    g <- paste0("\\\\", greek,"_\\{\\d*\\}")
    res <- regmatches(x, gregexpr(g, x))[[1]]
    res1 <- paste0("$",res, "$")
    vals <- c(vals, res1)
  }

  vals
}

.extract_title <- function(title, lang = "en", default = "Linear model") {
  res <- NULL

  if (isTRUE(title)) {
    res <- gettext(default, lang = lang)[[1]]
  }

  if (is.character(title)) {
    res <- title
  }
  return(res)
}

# Internal functions for format_table()

.create_flextable <- function(x, header = TRUE) {
  df <- x$df

  ft <- flextable(df) |>
    colformat_sci()

  if ("p.value" %in% colnames(df)) {
    ft <- ft |>
      colformat_sci(j = "p.value", lod = 2e-16)
  }

  if (!is.null(x$cols)) {
    ft <- .add_colnames(ft, x$cols)
  }

  if (!is.null(x$terms)) {
    vec <- x$terms
    if(is.character(vec) && !is.null(names(vec)) && all(nzchar(names(vec)))) {
      ft <- .add_labs(ft, vec)
    } else {
      ft <- .add_params(ft, vec)
    }
  }

  if (isTRUE(header)) {
    ft <- .add_header2(ft, title = x$title, equation = x$equa)
  }

  # footer and psignif
  n <- 0 # use to define align right and left

  if (!is.null(x$psignif)) {
    ft <- .add_signif(ft, x$psignif)
    n <- 1
  }

  if (!is.null(x$footer)) {
    vals <- x$footer
    ft <- add_footer_lines(ft, top = FALSE, values = para_md(vals))
    ft <- align(ft, i = seq_len(length(vals)) + n , align = "left",
                part = "footer")
  }

  ft <- autofit(ft, part = c("header", "body"))

  if (!is.null(df$signif)) {
    ft <- width(ft, j = "signif", width = 0.4)
  }

  return(ft)
}

format_table <- function(df, kind, header) {
  switch(kind,
         df = {df},
         tt = {
           stop("Not implemented yet")
         },
         ft = {
           .create_flextable(df, header = header)
         },
         gt = {
           stop("Not implemented yet")
         }
  )
}

.add_signif <- function(x, signif) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
                 ".add_signif_stars()"))}

  ft <- x
  s <- signif

  ft <- add_footer_lines(ft,
                         values = s)
  align(ft, i = 1, align = "right", part = "footer")
}

.add_header2 <- function(x, title, equation) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
                 ".add_header2()")) }

  ft <- x

  if (is.character(equation)) {
    ft <- add_header_lines(ft,
                           values = as_paragraph(as_equation(equation)))
    ft <- align(ft, i = 1, align = "center", part = "header")
  }

  if (is.character(title)) {
    ft <- add_header_lines(ft,
                           values = as_paragraph(title))
    ft <- align(ft, i = 1, align = "center", part = "header")
  }

  h_nrow <- nrow_part(ft, part = "header")

  if (h_nrow > 2) {
    ft |>
      border_inner_h(border = officer::fp_border(width = 0), part = "header") |>
      hline(i = nrow_part(ft, "header") - 1,
            border = officer::fp_border(width = 1.5, color = "#666666"),
            part = "header") ->
      ft
  }

  ft
}

.add_colnames <- function(x, labs) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
                 ".add_colnames()")) }

  ft <- x

  for (i in seq_along(labs))
    ft <- mk_par(ft, i = 1, j = names(labs)[i],
                 value = para_md(labs[i]), part = "header")

  ft
}

.add_labs <- function(x, labs) {
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
                 ".add_colnames()")) }

  ft <- x

  labs_red <- labs[names(labs) %in%ft$body$dataset$term]

  for (i in seq_along(labs_red))
    ft <- mk_par(ft, i = names(labs_red)[i], j = "term",
                 value = para_md(labs_red[i]), part = "body")

  ft
}

.add_params <- function(x, params) {

  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.",
                 ".add_colnames()")) }

  ft <- x

  if (length(params) == length(ft$body$dataset$term))
    ft <- mk_par(ft, j = "term", value = para_md(params), part = "body")

  ft
}

# Internal functions of flextable
# .pvalue_format <- function(x) {
#   #x <- get(as.character(substitute(x)), inherits = TRUE)
#   z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
#     labels = c("***", " **", "  *", "  .", "   "))
#   z <- as.character(z)
#   z[is.na(x)] <- ""
#   z
# }
#
#
# # Add pvalue signif
# .add_signif_stars <- function(x, i = NULL, j = NULL, part = "body",
# align = "right", ...) {
#
#   if (!inherits(x, "flextable")) {
#     stop(sprintf("Function `%s` supports only flextable objects.",
#       ".add_signif_stars()"))}
#
#   ft <- mk_par(x, i = i,  j = j,
#     value = as_paragraph(.pvalue_format(.data$p.value)))
#   ft <- add_footer_lines(ft,
#     values = c("0 <= '***' < 0.001 < '**' < 0.01 < '*' < 0.05"))
#   align(ft, i = 1, align = align, part = "footer")
# }
#
# # TODO: this is duplicated in tabularise -> export from there and reuse here!
# # Extract labels and units
# # .labels <- function(x, units = TRUE, ...) {
# #   labels <- sapply(x, data.io::label, units = units)
# #
# #   if (any(labels != "")) {
# #     # Use a \n before labels and the units
# #     if (isTRUE(units))
# #       labels <- sub(" +\\[([^]]+)\\]$", "\n [\\1]", labels)
# #     # set names if empty
# #     labels[labels == ""] <- names(x)[labels == ""]
# #     # Specific case for I() using in a formula
# #     labels[grepl("^I\\(.*\\)$", names(labels))] <- names(labels)[grepl("^I\\(.*\\)$", names(labels))]
# #   }
# #
# #   if (all(labels == ""))
# #     labels <- NULL
# #
# #   labels
# # }
#
# # .labels2 <- function(x, origdata = NULL, labs = NULL) {
# #
# #   #labs_auto <- NULL
# #   if (is.null(origdata)) {
# #     labs_auto <- .labels(x$model)
# #   } else {
# #     labs_auto <- .labels(origdata)
# #   }
# #
# #   if (!is.null(labs)) {
# #     if (!is.character(labs))
# #       stop("labs is not character vector")
# #     if (is.null(names(labs)))
# #       stop("labs must be named character vector")
# #     if (any(names(labs) %in% ""))
# #       stop("all element must be named")
# #     labs_res <- c(labs, labs_auto[!names(labs_auto) %in% names(labs)])
# #   } else {
# #     labs_res <- labs_auto
# #   }
# #
# #   labs_res
# # }
#
# # Retrieve model parameters
# .params_equa <- function(x, intercept = "alpha", greek = "beta") {
#   vals <- NULL
#
#   if (intercept != greek && grepl(intercept, x)) {
#     it <- paste0("\\\\", intercept)
#     res <- regmatches(x, gregexpr(it, x))[[1]]
#     vals <- paste0("$",res, "$")
#   }
#
#   if (grepl(greek, x)) {
#     g <- paste0("\\\\", greek,"_\\{\\d*\\}")
#     res <- regmatches(x, gregexpr(g, x))[[1]]
#     res1 <- paste0("$",res, "$")
#     vals <- c(vals, res1)
#   }
#
#   vals
# }
#
# # Change labels of header
# .header_labels <- function(x, info_lang, ...) {
#
#   if (!inherits(x, "flextable")) {
#     stop(sprintf("Function `%s` supports only flextable objects.",
#       "header_labels_lm()")) }
#
#   ft <- x
#
#   hlabs <- info_lang[["labs"]]
#   hlabs_red <- hlabs[names(hlabs) %in% ft$header$col_keys]
#
#   for (i in seq_along(hlabs_red))
#     ft <- mk_par(ft, i = 1, j = names(hlabs_red)[i],
#       value = para_md(hlabs_red[i]), part = "header")
#
#   ft
# }
#
# # Add header
# .add_header <- function(x, info_lang, header = TRUE, title = NULL, equation,
# ...) {
#
#   # If title is not provided, determine if we have to use TRUE or FALSE
#   if (missing(title)) {
#     title <- header # Default to same as header, but...
#     # if a caption is defined in the chunk, it defauts to FALSE
#     if (!is.null(knitr::opts_current$get('tbl-cap')))
#       title <- FALSE
#   }
#
#   if (!inherits(x, "flextable")) {
#     stop(sprintf("Function `%s` supports only flextable objects.",
#       ".add_header()")) }
#
#   ft <- x
#
#   if (isTRUE(header)) {
#     if (is.character(equation)) {
#       ft <- add_header_lines(ft,
#         values = as_paragraph(as_equation(equation)))
#       ft <- align(ft, i = 1, align = "right", part = "header")
#     }
#
#     if (isTRUE(title)) {
#       ft <- add_header_lines(ft, values = info_lang[["header"]])
#       ft <- align(ft, i = 1, align = "right", part = "header")
#     }
#
#     if (is.character(title)) {
#       ft <- add_header_lines(ft,
#         values = as_paragraph(title))
#       ft <- align(ft, i = 1, align = "right", part = "header")
#     }
#   }
#
#   h_nrow <- nrow_part(ft, part = "header")
#
#   if (h_nrow > 2) {
#     ft |>
#       border_inner_h(border = officer::fp_border(width = 0), part = "header") |>
#       hline(i = nrow_part(ft, "header") - 1,
#         border = officer::fp_border(width = 1.5, color = "#666666"),
#         part = "header") ->
#       ft
#   }
#
#   ft
# }
