# ---- Parse RTF's in inst/extdata/mckinsey/* ----------------------------------
pkgload::load_all()

files <- fs::dir_ls("inst/extdata/mckinsey", recurse = TRUE, glob = "*.rtf")

safe_rtf_to_ard <- safely(rtf_to_ard)

results <- map(
  files,
  safe_rtf_to_ard
)

num_errors <- map_lgl(results, ~ !is.null(.x$error)) |>
  sum()

print(paste0("success rate = ", (1 - num_errors / length(results)) * 100, "%"))

# ---- Successes ---------------------------------------------------------------
successes <- results |>
  keep(~ !is.null(.$result)) %>%
  map(~ .$result)

# File names
successes |>
  names() |>
  stringr::str_extract("[^/]+$") |>
  print()

# ---- Failures ----------------------------------------------------------------
failures <- results |>
  keep(~ !is.null(.$error))

failure_paths <- failures |>
  names() |>
  print()

# ---- Correct sec-8-1 and 8-2 -------------------------------------------------
failure_paths_sec_81_82 <- failure_paths |>
  stringr::str_subset("sec-8-1|sec-8-2")

# ---- rt-ae-overallaecat.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

failure_paths_sec_81_82[[1]] |>
  readr::read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  readr::write_file(temp_rtf)

df <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  strip_empty_rows() |>
  strip_empty_cols()

header <- slice(df, 1:3)
footer_first_row <- max(which(df[[3]] != "" | is.na(df[[3]]))) + 1
footer <- slice(df, footer_first_row:nrow(df))
colnames <- slice(df, 4)
col_group <- slice(df, 5)

table_cells <- df |>
  anti_join(header) |>
  anti_join(footer) |>
  anti_join(colnames) |>
  anti_join(col_group)

df_w_row_groups <- table_cells |>
  filter(!if_all(everything(), ~ . == "" | . == "&nbsp;")) |>
  mutate(
    row_group = if_else(
      if_all(.cols = !1, .fns = ~ .x == ""),
      X1,
      NA_character_
    )
  ) |>
  fill(row_group) |>
  filter(if_all(.cols = !1, .fns = ~ .x != "" | is.na(.x)))

# colnames_unique <-
colnames <- c(
  "Safety Parameter",
  "Arm A: Nivo N = 351 Any Grade",
  "Arm A: Nivo N = 351 Grade 3-4",
  "Arm A: Nivo N = 351 Grade 5",
  "Arm B: Nivo + Ipi N = 352 Any Grade",
  "Arm B: Nivo + Ipi N = 352 Grade 3-4",
  "Arm B: Nivo + Ipi N = 352 Grade 5",
  "Arm C: Chemo N = 115 Any Grade",
  "Arm C: Chemo N = 115 Grade 3-4",
  "Arm C: Chemo N = 115 Grade 5"
)

rt_ae_overallaecat <- df_w_row_groups |>
  rename_with(~ as.character(colnames), .cols = !row_group) |>
  pivot_longer(!c(1, 11)) |>
  separate_wider_regex(
    cols = name,
    patterns = c(arm_details = ".* N = \\d+", " ", grade = ".*")
  ) |>
  rename(
    variable = `Safety Parameter`,
    group1_level = arm_details,
    col_group = grade,
    stat = value
  )
