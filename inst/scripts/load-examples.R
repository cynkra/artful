# RTF examples exist at: `inst/extdata/bms-*.rtf/`
# This script converts these example RTF files into data frames, and then
# exports them for use as example data in the functions in `R/parse.R`.
pkgload::load_all()

bms_1 <- system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()

bms_2 <- system.file("extdata", "bms-2.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()

bms_3 <- system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()

bms_4 <- system.file("extdata", "bms-4.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()

bms_5 <- system.file("extdata", "bms-5.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()
