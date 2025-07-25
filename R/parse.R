#' Remove empty rows
#'
#' This function removes empty rows (where all cells are empty strings) inside a
#' data frame generated by calling [rtf_to_html()].
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame with empty rows removed.
#'
#' @keywords internal
strip_empty_rows <- function(data) {
  filter(data, !if_all(everything(), ~ . == ""))
}

#' Remove the header
#'
#' This function removes the rows representing a header (e.g., titles and
#' subtitles) inside a data frame generated by calling [rtf_to_html()].
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame with the header removed.
#'
#' @keywords internal
strip_header <- function(data) {
  # The second+ columns are populated with empty strings "" alongside the
  # header information which can be used to determine the row indices of the
  # header:
  header_last_row <- min(which(data[[2]] != "" | is.na(data))) - 1

  # Some tables repeat across pages in the original RTF. This results in
  # data frames containing combined data frames with headers that repeat across
  # several points. Capture the original header:
  header <- slice(data, 1L:header_last_row)

  # Remove all copies of the header:
  data |>
    anti_join(header)
}

#' Remove the footer
#'
#' This function removes the rows representing a footer inside a data frame
#' generated by calling [rtf_to_html()].
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame with the footer removed.
#'
#' @keywords internal
strip_footer <- function(data) {
  # The last row in the third column that is not an empty string signals the
  # last row before the footer. All following rows until the end of the data
  # frame are populated with empty strings alonside the footer values in
  # the first column. We also need an escape hatch in this instance the table
  # only has two columns:
  col_index <- if (ncol(data) >= 3) 3 else 2
  footer_first_row <- max(
    which(
      data[[col_index]] != "" | is.na(data[[col_index]])
    )
  ) +
    1

  # Some tables repeat across pages in the original RTF. This results in
  # data frames containing combined data frames with footers that repeat across
  # several points. Capture the original footer:
  footer <- slice(data, footer_first_row:nrow(data))

  # Remove all copies of the footer:
  data |>
    anti_join(footer)
}

#' Remove repeat column names
#'
#' This function removes columns names that repeat across multiple rows (caused
#' by pagination in the original RTF files) inside a data frame generated by
#' calling [rtf_to_html()]. Only the first occurance should be kept.
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame with the repeat column names removed.
#'
#' @keywords internal
strip_repeat_colnames <- function(data) {
  # This assumes column names are in the first row (which is the case if this
  # function is called after calling `strip_header()`)
  first_row <- dplyr::slice(data, 1L)
  remaining_rows <- dplyr::slice(data, -1L)

  remaining_rows_without_colnames <- remaining_rows |>
    dplyr::anti_join(first_row)

  dplyr::bind_rows(first_row, remaining_rows_without_colnames)
}

#' Set column names
#'
#' This function sets columns names inside a data frame generated by calling
#' [rtf_to_html()]. It assumes the first row contains the actual column names.
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame with the repeat column names removed.
#'
#' @keywords internal
set_colnames <- function(data) {
  # This assumes column names are in the first row (which is the case if this
  # function is called after calling `strip_header()`)
  colnames <- slice(data, 1L) |> as.character()

  # For any missing column names, it is assumed that the missing column can be
  # found on the first row of the corresponding column where all other columns
  # are empty strings.
  colnames_missing_indices <- which(colnames == "")

  for (i in colnames_missing_indices) {
    filtered <- filter(non_repeating, if_all(!i, ~ .x == ""))
    if (nrow(filtered != 0)) {
      replacement <- filtered |>
        slice(1L) |>
        pull(i)
    } else {
      replacement <- "X1"
    }
    colnames[i] <- replacement
  }

  data |>
    rename_with(~colnames) |>
    slice(-1L)
}

#' Strip empty columns
#'
#' Remove any columns where all values are NA or empty strings.
#'
#' @keywords internal
strip_empty_cols <- function(data) {
  data |>
    select(where(~ !all(is.na(.)))) |>
    select(where(~ !all(. == "")))
}

#' Strip pagination from a table
#'
#' This function combines the functionality of [strip_empty_rows()],
#' [strip_empty_cols()], [strip_header()], [strip_footer()],
#' [strip_repeat_colnames()], and [set_colnames()] into a single function. If
#' fine grained control is not required to remove certain elements of a table,
#' this function is preferred because the actions of stripping table elements
#' are dependent on each other (e.g., to remove repeat column names, we first
#' must identify these by stripping the header).
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame where pagination has been stripped.
#'
#' @keywords internal
strip_pagination <- function(data) {
  data <- strip_empty_rows(data)
  data <- strip_empty_cols(data)
  header_last_row <- min(which(data[[2]] != "" | is.na(data))) - 1
  header <- slice(data, 1:header_last_row)

  col_index <- if (ncol(data) >= 3) 3 else 2
  footer_first_row <- max(
    which(
      data[[col_index]] != "" | is.na(data[[col_index]])
    )
  ) +
    1
  footer <- slice(data, footer_first_row:nrow(data))

  colnames <- slice(data, header_last_row + 1)

  table_cells <- data |>
    anti_join(header) |>
    anti_join(footer) |>
    anti_join(colnames) |>
    suppressMessages()

  non_repeating <- bind_rows(colnames, table_cells)

  colnames <- slice(non_repeating, 1L) |> as.character()
  colnames_missing_indices <- which(colnames == "")

  for (i in colnames_missing_indices) {
    filtered <- filter(non_repeating, if_all(!i, ~ .x == ""))
    if (nrow(filtered != 0)) {
      replacement <- filtered |>
        slice(1L) |>
        pull(i)
    } else {
      replacement <- "X1"
    }
    colnames[i] <- replacement
  }

  non_repeating |>
    rename_with(~colnames) |>
    slice(-1L)
}

#' Strip indentation
#'
#' Convert indentation identified by "&nbsp;" values into a series of
#' `variable_*` columns.
#'
#' @param data A data frame generated by calling [strip_pagination()] containing
#' indentation in the first column, represented by "&nbsp;" values in each cell.
#'
#' @return A data frame where the indented first column has been parsed into
#' separate `variable_*` columns in accordance with the ARD standard.
#'
#' @autoglobal
#' @keywords internal
strip_indentation <- function(data) {
  # Label indentation levels
  indents <- data |>
    mutate(
      nbsp_count = stringr::str_count(data[[1]], stringr::fixed("&nbsp;"))
    ) |>
    mutate(nbsp_group = cumsum(nbsp_count == 0)) |>
    mutate(
      counter = case_when(
        nbsp_count == 0 ~ 0,
        nbsp_count > lag(nbsp_count, default = 0) ~ 1,
        nbsp_count == lag(nbsp_count, default = 0) ~ 0,
        nbsp_count < lag(nbsp_count, default = 0) ~ -1
      )
    ) |>
    mutate(indent_level = cumsum(counter), .by = nbsp_group) |>
    mutate(
      indent_level_rev = max(indent_level) + min(indent_level) - indent_level,
      .by = nbsp_group
    )

  # Create `variable_*` columns corresponding to indentation
  variable_cols <- indents |>
    rename(value = 1) |>
    mutate(variable_label = paste0("variable_label", indent_level_rev)) |>
    mutate(id = row_number()) |> # prevents `pivot_wider()` errors caused by duplicate values
    pivot_wider(
      names_from = variable_label,
      values_from = value,
      id_cols = everything()
    ) |>
    group_by(nbsp_group) |>
    fill(starts_with("variable_label")) |>
    ungroup() |>
    rename(variable_level = variable_label0)

  # Clean up
  variable_cols |>
    select(
      variable_level,
      starts_with("variable_label"),
      everything(),
      -id,
      -nbsp_count,
      -nbsp_group,
      -counter,
      -indent_level,
      -indent_level_rev
    ) |>
    mutate(
      across(
        everything(),
        ~ stringr::str_replace_all(., stringr::fixed("&nbsp;"), "")
      )
    )
}

#' Pivot a grouping variable longer
#'
#' The grouping variables in the final ARD are normally presented in a wide
#' format in the input RTF table. This functions pivots the grouping levels into
#' a long format.
#'
#' @param data A data frame with the grouping variable in a wide format.
#'
#' @return A data frame with the grouping variable in a long format.
#'
#' @keywords internal
pivot_group <- function(data) {
  data |>
    rename(variable_level = 1) |>
    pivot_longer(
      !starts_with("variable_"),
      names_to = "group1_level",
      values_to = "stat"
    ) |>
    mutate(group1 = "TRT", .before = 1) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

#' Separate the Big N stat
#'
#' Extract the Big N stat and place it at the top of a table.
#'
#' @param data A data frame containing a column called "group1_level" which
#' contains the Big N statistic merged as part of the text.
#'
#' @return A data frame with the Big N stat separated and placed at the top.
#'
#' @autoglobal
#' @keywords internal
separate_bign <- function(data) {
  big_n <- data |>
    distinct(group1_level) |>
    separate_wider_regex(
      group1_level,
      patterns = c(
        variable_label1 = ".*?\\S+",
        "\\s+",
        "(?:\\(N = |N = )",
        stat = "\\d+",
        "\\)?"
      )
    ) |>
    mutate(stat_name = "N_header", stat_label = "N", .before = "stat")

  data <- data |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, data) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

#' Separate a merged columns into multiple columns
#'
#' This function separates columns containing merged values (e.g., where a count
#' and percentage are merged into a single string such as "34 (.56)") into
#' multiple columns inside a data frame generated by calling [rtf_to_html()].
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame where merged columns have been separated.
#'
#' @keywords internal
separate_merges <- function(data) {
  # Placeholder
}

#' Separate a spanner
#'
#' This function separates spanning column labels out inside a data frame
#' generated by calling [rtf_to_html()]. A spanning column header represents a
#' unique grouping identifier for the columns it spans over.
#'
#' @param data A data frame generated by calling [rtf_to_html()].
#'
#' @return A data frame where merged columns have been separated.
#'
#' @keywords internal
separate_spanners <- function(data) {
  # Placeholder
}

#' Manage table expcetions
#'
#' Several tables do not align with the general ruleset and must be treated as
#' exceptions that get special processing to re-align them.
#'
#' @keywords internal
manage_exceptions <- function(data) {
  if (
    data[[1]][[1]] ==
      "Table 14.1.4.1Baseline Disease Characteristics SummaryRandomized Population"
  ) {
    result <- map_dfr(
      .x = 1:nrow(data),
      .f = \(x) {
        current_row <- as.character(data[x, ])
        if (identical(current_row, as.character(data[2L, ]))) {
          shifted <- c("", current_row[-length(current_row)])
        } else {
          shifted <- current_row
        }
        tibble(!!!set_names(shifted[-2L], names(data)[-2L]))
      }
    )
    return(result)
  } else {
    return(data)
  }
}
