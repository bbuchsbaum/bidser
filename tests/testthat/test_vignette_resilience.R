test_that("quickstart exits cleanly when the example dataset is unavailable", {
  skip_if_not_installed("knitr")

  vignette_path <- testthat::test_path(
    "..", "..", "vignettes", "quickstart.Rmd"
  )
  if (!file.exists(vignette_path)) {
    vignette_path <- system.file(
      "doc", "quickstart.Rmd", package = "bidser"
    )
  }
  skip_if(!nzchar(vignette_path) || !file.exists(vignette_path))

  knit_env <- new.env(parent = globalenv())
  knit_env$params <- list(family = "red", preset = "homage")
  knit_env$get_example_bids_dataset <- function(...) {
    stop("simulated offline failure")
  }
  output_path <- tempfile(fileext = ".md")

  expect_no_error(
    knitr::knit(
      vignette_path,
      output = output_path,
      envir = knit_env,
      quiet = TRUE
    )
  )
  output <- paste(readLines(output_path, warn = FALSE), collapse = "\n")
  expect_match(output, "Example dataset not available.", fixed = TRUE)
})
