temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "bms-1.rtf", package = "artful") |>
  readr::read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  readr::write_file(temp_rtf)

bms_1 <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

bms_1 |>
  mutate(
    guessed_regex = try_regex_match(stat)
  )
