test_that("Update .po and .mo files", {
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    res <- try(tools::update_pkg_po("../.."), silent = TRUE)
  }
})
