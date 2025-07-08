test_that("Coverage across all examples, without stats parsing", {
  files <- list.files(
    system.file("extdata", "examples", package = "artful"),
    full.names = TRUE
  )

  safe_rtf_to_ard <- safely(rtf_to_ard)

  results <- map(
    files,
    safe_rtf_to_ard
  )

  num_errors <- map_lgl(results, ~ !is.null(.x$error)) |>
    sum()

  # "inst/extdata/examples/rt-ae-eair3.rtf" cannot currently be parsed and so
  # we expect a single error.
  expect_equal(num_errors, 1)
})
