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
          shift_table_start = dplyr::lead(
            table_start,
            n = i,
            default = FALSE
          ),
          table_start = table_start & shift_table_start
        ) |>
        select(-shift_table_start)
    }
  }

  list_of_tables <- data_with_table_flag |>
    mutate(table_id = cumsum(table_start)) |>
    select(-table_start) |>
    group_split(table_id, .keep = FALSE)

  return(list_of_tables)
}

bms_1_tables <- split_data(bms_1)
bms_3_tables <- split_data(bms_3)

# In each sub table, extract indentation labels, and create new cols
pivot_indentation <- function(data) {
  indents <- min(which(data[[2]] != "" | is.na(data))) - 1

  col1 <- names(data)[1]
  col2 <- names(data)[2]

  if (indents == 1) {
    data |>
      slice(-1) |>
      mutate(variable_label1 = data[[1]][[1]])
  } else if (indents == 2) {
    data |>
      slice(-1) |>
      mutate(variable_label1 = data[[1]][[1]]) |>
      mutate(
        row = row_number(),
        is_label = (.data[[col2]] == "" |
          is.na(.data[[col2]]))
      ) |>
      mutate(label_id = if_else(is_label, row, NA_integer_)) |>
      fill(label_id, .direction = "down") |>
      mutate(
        variable_label2 = if_else(is_label, .data[[col1]], NA_character_),
        .by = label_id
      ) |>
      fill(variable_label2, .direction = "down") |>
      filter(!is_label) |>
      select(-row, -is_label, -label_id)
  }
}

pivot_group <- function(data) {
  data |>
    rename(variable_level = 1) |>
    pivot_longer(
      !starts_with("variable_"),
      names_to = "group1_level",
      values_to = "stat"
    )
}

bms_1 |>
  split_data() |>
  map(pivot_indentation) |>
  list_rbind() |>
  pivot_group() |>
  # ---- Custom call to match expected formatting ----
  mutate(group1 = "TRT") |>
  mutate(variable = str_to_upper(paste(variable_label1, variable_label2))) |>
  mutate(variable = str_remove(variable, "FROM ")) |>
  mutate(variable = str_replace_all(variable, " ", "_")) |>
  select(
    group1,
    group1_level,
    variable,
    variable_label1,
    variable_label2,
    variable_level,
    stat
  )

bms_3 |>
  split_data() |>
  map(pivot_indentation) |>
  list_rbind() |>
  pivot_group()

# ============================ EXPERIMENTAL ====================================
# Below is an alternative solution that may scale to any number of indentations,
# but the logic is hard to reason about, so for now, the simpler function above
# is preferred until examples with more indentation levels arrive.

# pivot_indentations <- function(data) {
#   col1 <- names(data)[1]
#   col2 <- names(data)[2]

#   value_col <- data[[2]]
#   num_levels <- which(value_col != "" | is.na(value_col))[1] - 1
#   num_levels <- max(num_levels, 0, na.rm = TRUE)

#   data_clean <- data |>
#     mutate(.row = row_number())

#   labels <- data_clean |>
#     filter(.data[[col2]] == "" & !is.na(.data[[col2]])) |>
#     transmute(.row, label = .data[[col1]]) |>
#     mutate(level = row_number())

#   if (
#     nrow(filter(data_clean, .data[[col2]] != "" & !is.na(.data[[col2]]))) == 0
#   ) {
#     return(data)
#   }

#   label_df <- tibble(.row = seq_len(nrow(data)))
#   for (i in seq_len(num_levels)) {
#     label_df[[paste0("variable_label", i)]] <- NA_character_
#   }

#   if (nrow(labels) > 0) {
#     label_df <- label_df |>
#       left_join(labels, by = ".row") |>
#       mutate(across(starts_with("variable_label"), ~NA_character_))

#     label_names <- paste0("variable_label", seq_len(num_levels))
#     label_values <- rep(NA_character_, num_levels)
#     if (nrow(labels) >= num_levels) {
#       label_values[num_levels] <- labels$label[nrow(labels)]
#     }
#     label_df[label_names] <- map_dfc(
#       seq_len(num_levels),
#       ~ {
#         if (.x < num_levels && .x <= nrow(labels)) {
#           rep(labels$label[.x], nrow(label_df))
#         } else {
#           rep(NA_character_, nrow(label_df))
#         }
#       }
#     )
#     label_df[[label_names[num_levels]]] <- if_else(
#       labels$level >= num_levels,
#       labels$label,
#       NA_character_
#     )[match(label_df$.row, labels$.row)]
#     label_df <- fill(label_df, all_of(label_names), .direction = "down")
#   }

#   data_clean |>
#     filter(.data[[col2]] != "" & !is.na(.data[[col2]])) |>
#     left_join(label_df, by = ".row") |>
#     select(-.row, -label, -level)
# }
