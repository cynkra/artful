library(tidyverse)
pkgload::load_all()

df <- system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe()

# Strip empty rows
df |>
  filter(!if_all(everything(), ~ . == ""))
