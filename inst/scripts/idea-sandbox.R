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


# Find number of indentation
bms_1_num_indents <- min(which(bms_1[[2]] != "" | is.na(bms_1))) - 1
print(bms_1_num_indents)

bms_3_num_indents <- min(which(bms_3[[2]] != "" | is.na(bms_3))) - 1
print(bms_3_num_indents)

# Separate table in multiple tables, split where the max number of indents
# reappears
bms_1 |>
  mutate(
    is_empty = .data[[colnames(bms_1)[[2]]]] == "",
    prev_is_empty = lead(is_empty, default = FALSE),
    start_table = is_empty & prev_is_empty,
    group_id = cumsum(start_table)
  ) |>
  select(
    -all_of(
      c(
        "is_empty",
        "prev_is_empty",
        "start_table"
      )
    )
  ) |>
  group_split(group_id, .keep = FALSE)

bms_3 |>
  mutate(
    is_empty = .data[[colnames(bms_3)[[2]]]] == "",
    prev_is_empty = lead(is_empty, default = FALSE),
    group_id = cumsum(prev_is_empty)
  ) |>
  select(
    -all_of(
      c(
        "is_empty",
        "prev_is_empty"
      )
    )
  ) |>
  group_split(group_id, .keep = FALSE)


library(dplyr)

split_tables <- function(data, n_consecutive) {
  df_with_marker <- data |>
    mutate(
      .starts_new_table_here = .data[[colnames(bms_3)[[2]]]] == "",
    )

  if (n_consecutive > 1) {
    for (i in 1:(n_consecutive - 1)) {
      df_with_marker <- df_with_marker |>
        mutate(
          .lead_is_empty = dplyr::lead(
            .data[[colnames(bms_3)[[2]]]] == "",
            n = i,
            default = FALSE
          ),
          .starts_new_table_here = .starts_new_table_here & .lead_is_empty
        ) |>
        select(-.lead_is_empty)
    }
  }

  list_of_tables <- df_with_marker |>
    mutate(
      .table_group_id = cumsum(.starts_new_table_here)
    ) |>
    filter(.table_group_id > 0) |>
    select(-.starts_new_table_here) |>
    group_split(.table_group_id, .keep = FALSE)

  return(list_of_tables)
}

split_tables(bms_1, 1)
split_tables(bms_3, 1)

# In each subtitle, extract indentation labels, and create new cols

# Bind all tables back together

# Pivot longer the original column names into final group
