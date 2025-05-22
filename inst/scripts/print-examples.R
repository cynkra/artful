library(purrr)
library(rvest)
pkgload::load_all()

load_and_view <- function(file) {
  rtf_file <- system.file("extdata", file, package = "artful")

  html <- rtf_to_html(rtf_file)

  html |>
    minimal_html() |>
    html_element("table") |>
    html_table() |>
    View()
}

walk(
  paste0("bms-", 1:5, ".rtf"),
  load_and_view
)
