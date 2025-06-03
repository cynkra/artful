pkgload::load_all()

# ---- Double indentation ----
# Calling sub-functions
system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group() |>
  # custom code
  mutate(
    variable = stringr::str_to_upper(paste(variable_label1, variable_label2))
  ) |>
  mutate(variable = stringr::str_remove(variable, "FROM ")) |>
  mutate(variable = stringr::str_replace_all(variable, " ", "_")) |>
  relocate(variable, .before = "variable_level")

# Calling control function
system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_ard()

# ---- Single indentation ----
# Calling sub-functions
system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()

# Calling control function
system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_ard()
