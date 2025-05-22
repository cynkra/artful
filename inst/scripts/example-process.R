library(rvest)
pkgload::load_all()

rtf_file <- system.file("extdata", "bms-1.rtf", package = "artful")

html <- rtf_to_html(rtf_file)

df <- html |>
  minimal_html() |>
  html_element("table") |>
  html_table()
