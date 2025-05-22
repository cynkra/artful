library(purrr)
library(rvest)
pkgload::load_all()

load_and_view <- function(file) {
  system.file("extdata", file, package = "artful") |>
    rtf_to_html() |>
    html_to_dataframe() |>
    View()
}

walk(
  paste0("bms-", 1:5, ".rtf"),
  load_and_view
)
