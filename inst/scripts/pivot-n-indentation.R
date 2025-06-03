pivot_n_indentations <- function(data) {
  col1 <- names(data)[1]
  col2 <- names(data)[2]

  value_col <- data[[2]]
  num_levels <- which(value_col != "" | is.na(value_col))[1] - 1
  num_levels <- max(num_levels, 0, na.rm = TRUE)

  data_clean <- data |>
    mutate(.row = row_number())

  labels <- data_clean |>
    filter(.data[[col2]] == "" & !is.na(.data[[col2]])) |>
    transmute(.row, label = .data[[col1]]) |>
    mutate(level = row_number())

  if (
    nrow(filter(data_clean, .data[[col2]] != "" & !is.na(.data[[col2]]))) == 0
  ) {
    return(data)
  }

  label_df <- tibble(.row = seq_len(nrow(data)))
  for (i in seq_len(num_levels)) {
    label_df[[paste0("variable_label", i)]] <- NA_character_
  }

  if (nrow(labels) > 0) {
    label_df <- label_df |>
      left_join(labels, by = ".row") |>
      mutate(across(starts_with("variable_label"), ~NA_character_))

    label_names <- paste0("variable_label", seq_len(num_levels))
    label_values <- rep(NA_character_, num_levels)
    if (nrow(labels) >= num_levels) {
      label_values[num_levels] <- labels$label[nrow(labels)]
    }
    label_df[label_names] <- map_dfc(
      seq_len(num_levels),
      ~ {
        if (.x < num_levels && .x <= nrow(labels)) {
          rep(labels$label[.x], nrow(label_df))
        } else {
          rep(NA_character_, nrow(label_df))
        }
      }
    )
    label_df[[label_names[num_levels]]] <- if_else(
      labels$level >= num_levels,
      labels$label,
      NA_character_
    )[match(label_df$.row, labels$.row)]
    label_df <- fill(label_df, all_of(label_names), .direction = "down")
  }

  data_clean |>
    filter(.data[[col2]] != "" & !is.na(.data[[col2]])) |>
    left_join(label_df, by = ".row") |>
    select(-.row, -label, -level)
}
