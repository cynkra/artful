library(tidyverse)
pkgload::load_all()

# Separate indented columns
# Goals:
# - Every level of indentation representings a grouping and should reside in a
#   new `group<N>` and  `group<N>_level` variables
# - The original columns also need separating out into `group<N>` and
#   `group<N>_level` variables (i.e., they are currently in wide format and need
#   to be converted to long format).

# Double indentation
bms_1 <- system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()

# Single indentation
bms_3 <- system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()

# Separate table in multiple tables, split where the max number of indents
# reappears
split_data <- function(data) {
  data_with_table_flag <- data |>
    mutate(table_start = .data[[colnames(data)[[2]]]] == "")

  indents <- min(which(data[[2]] != "" | is.na(data))) - 1

  if (indents > 1) {
    for (i in 1:(indents - 1)) {
      data_with_table_flag <- data_with_table_flag |>
        mutate(
          table_start_additional = dplyr::lead(
            table_start,
            n = i,
            default = FALSE
          ),
          table_start = table_start & table_start_additional
        ) |>
        select(-table_start_additional)
    }
  }

  list_of_tables <- data_with_table_flag |>
    mutate(table_id = cumsum(table_start)) |>
    select(-table_start) |>
    group_split(table_id, .keep = FALSE)

  return(list_of_tables)
}

split_data(bms_1)
split_data(bms_3)

# In each subtitle, extract indentation labels, and create new cols

# Bind all tables back together

# Pivot longer the original column names into final group
