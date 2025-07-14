#' Parse statistic value using pseudo-pattern system with context awareness
#'
#' @param stat_value Character string containing the statistic to parse.
#' @param variable_context Variable name/label for context-based pattern
#' selection.
#' @param patterns Optional list of patterns to use (if NULL, uses global
#' patterns).
#'
#' @return Data frame with parsed statistics
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' parse_stat_value_pseudo_with_context("24")
#' parse_stat_value_pseudo_with_context("12 - 30")
#' parse_stat_value_pseudo_with_context("12 (30)")
#' }
parse_stat_value_pseudo_with_context <- function(
  stat_value,
  variable_context = NULL,
  patterns = NULL
) {
  stat_value <- trimws(as.character(stat_value))

  # Skip if empty or NA
  if (is.na(stat_value) || stat_value == "") {
    return(data.frame(
      stat = stat_value,
      stat_name = "missing",
      stat_label = "Missing",
      stringsAsFactors = FALSE
    ))
  }

  # Get patterns
  if (is.null(patterns)) {
    patterns <- get_stat_patterns()
    # Initialize pseudo-patterns if not already done
    if (length(patterns) <= 2) {
      update_stat_patterns_to_pseudo()
      patterns <- get_stat_patterns()
    }
  }

  # Reorder patterns based on variable context for better matching
  if (!is.null(variable_context)) {
    patterns <- reorder_patterns_by_context(patterns, variable_context)
  }

  # Try each pattern (greedy - first match wins)
  for (pattern_name in names(patterns)) {
    pattern <- patterns[[pattern_name]]

    if (grepl(pattern$regex, stat_value)) {
      # Extract matches
      matches <- regmatches(stat_value, regexec(pattern$regex, stat_value))[[1]]

      if (length(matches) > 1) {
        # matches[1] is full match, rest are capture groups
        values <- matches[-1]

        # Clean up values - remove trailing % from percentages
        for (i in seq_along(values)) {
          if (pattern$stats[i] == "pct" && grepl("%$", values[i])) {
            values[i] <- sub("%$", "", values[i])
          }
        }

        # Create result with one row per statistic
        result <- data.frame(
          stat = values,
          stat_name = pattern$stats,
          stat_label = pattern$labels,
          stringsAsFactors = FALSE
        )

        return(result)
      }
    }
  }

  # No pattern matched - return as "other"
  return(data.frame(
    stat = stat_value,
    stat_name = "other",
    stat_label = "Other",
    stringsAsFactors = FALSE
  ))
}

#' Update statistical patterns to use pseudo-patterns
#' @export
update_stat_patterns_to_pseudo <- function() {
  # Define patterns using pseudo-pattern syntax
  pseudo_patterns <- list(
    # Most common patterns first (for greedy matching)
    "n_pct" = "{n} ({pct}%)", # Handles all variations: space/no-space, %/no-%
    "mean_sd" = "{mean} ({sd})",
    "mean_se" = "{mean} ({se})",
    "median_range" = "{median} ({min}, {max})",
    "median_iqr" = "{median} ({q1}, {q3})",
    "median_q1_q3" = "{median} ({q1} - {q3})",
    "range" = "{min} - {max}",
    "range_comma" = "{min}, {max}",
    "ci" = "{estimate} ({ci_lower}, {ci_upper})",
    "ci_dash" = "{estimate} ({ci_lower}-{ci_upper})",
    "hr_ci" = "{hr} ({ci_lower}-{ci_upper})",
    "count_only" = "{n}",
    "pct_only" = "{pct}%",
    "count_total" = "{n}/{total}",
    "mean_sd_n" = "{mean} ({sd}) [{n}]"
  )

  # Convert each pseudo-pattern to full pattern definition
  patterns <- list()
  for (name in names(pseudo_patterns)) {
    template <- pseudo_patterns[[name]]
    pattern_info <- pseudo_to_regex(template)

    patterns[[name]] <- list(
      template = template,
      regex = pattern_info$regex,
      stats = pattern_info$stats,
      labels = pattern_info$labels
    )
  }

  # Update global patterns
  set_stat_patterns(patterns)

  invisible(patterns)
}

#' Reorder patterns based on variable context for better pattern matching
#' @param patterns List of statistical patterns
#' @param variable_context Variable name/label for context
#' @return Reordered patterns list
#' @keywords internal
reorder_patterns_by_context <- function(patterns, variable_context) {
  if (
    is.null(variable_context) ||
      is.na(variable_context) ||
      variable_context == ""
  ) {
    return(patterns)
  }

  variable_lower <- tolower(variable_context)

  # Define context indicators for different stat types
  continuous_indicators <- c(
    "mean",
    "median",
    "std",
    "sd",
    "se",
    "min",
    "max",
    "q1",
    "q3",
    "average",
    "variance",
    "deviation",
    "quartile",
    "range",
    "cfb",
    "change from baseline",
    "baseline",
    "score"
  )

  categorical_indicators <- c(
    "n (%)",
    "count",
    "frequency",
    "number",
    "percent",
    "proportion",
    "rate",
    "incidence",
    "events",
    "subjects",
    "patients",
    "responders",
    "response"
  )

  ci_indicators <- c(
    "confidence",
    "ci",
    "interval",
    "estimate",
    "hazard",
    "odds",
    "risk",
    "ratio",
    "difference",
    "effect"
  )

  # Check which type this variable likely represents
  is_continuous <- any(sapply(continuous_indicators, function(x) {
    grepl(x, variable_lower)
  }))
  is_categorical <- any(sapply(categorical_indicators, function(x) {
    grepl(x, variable_lower)
  }))
  is_ci <- any(sapply(ci_indicators, function(x) grepl(x, variable_lower)))

  # Create ordered pattern preference
  priority_patterns <- character(0)

  if (is_ci) {
    priority_patterns <- c("ci_extra_spaces", "ci", "ci_dash", "hr_ci")
  } else if (is_continuous) {
    priority_patterns <- c(
      "mean_sd",
      "mean_se",
      "median_range",
      "median_iqr",
      "median_q1_q3",
      "range",
      "range_comma"
    )
  } else if (is_categorical) {
    priority_patterns <- c("n_pct", "count_only", "pct_only", "count_total")
  }

  # Reorder: priority patterns first, then the rest
  remaining_patterns <- names(patterns)[!names(patterns) %in% priority_patterns]
  new_order <- c(
    priority_patterns[priority_patterns %in% names(patterns)],
    remaining_patterns
  )

  return(patterns[new_order])
}

#' Convert pseudo-pattern to regex pattern
#'
#' Use numbered capture groups for compatibility.
#'
#' @param template Character string with placeholders like "n (pct%)"
#' @return List with regex pattern and metadata
#' @keywords internal
#'
#' @examples
#' \dontrun {
#' pseudo_to_regex("{n} ( {%})")
#' pseudo_to_regex("{median} ({min}, {max})")
#' pseudo_to_regex("{estimate} ({ci_lower}-{ci_upper})")
#' }
pseudo_to_regex <- function(template) {
  # Extract placeholders
  placeholder_pattern <- "\\{([^}]+)\\}"
  matches <- gregexpr(placeholder_pattern, template)

  if (matches[[1]][1] == -1) {
    stop("No placeholders found in template")
  }

  # Get placeholder names
  placeholder_matches <- regmatches(template, matches)[[1]]
  stat_names <- gsub("\\{|\\}", "", placeholder_matches)

  # Start with the template
  regex_pattern <- template

  # Escape special characters (but preserve our placeholders)
  # Replace placeholders with markers first
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- sub(
      fixed = TRUE,
      pattern = placeholder_matches[i],
      replacement = paste0("<<<MARKER", i, ">>>"),
      x = regex_pattern
    )
  }

  # Escape parentheses and other special chars
  regex_pattern <- gsub("(", "\\(", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub(")", "\\)", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub(".", "\\.", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("[", "\\[", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("]", "\\]", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("+", "\\+", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("*", "\\*", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("?", "\\?", regex_pattern, fixed = TRUE)

  # Replace markers with capture groups
  # Use pattern that matches numbers/decimals (positive or negative)
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- sub(
      fixed = TRUE,
      pattern = paste0("<<<MARKER", i, ">>>"),
      replacement = "([+-]?\\d+(?:\\.\\d+)?)", # Matches: -123, 123, -12.34, 12.34
      x = regex_pattern
    )
  }

  # Handle whitespace - make it flexible
  regex_pattern <- gsub("\\s+", "\\\\s*", regex_pattern)

  # Make % optional
  regex_pattern <- gsub("%", "%?", regex_pattern, fixed = TRUE)

  # Add anchors
  regex_pattern <- paste0("^\\s*", regex_pattern, "\\s*$")

  return(list(
    template = template,
    regex = regex_pattern,
    stats = stat_names,
    labels = create_default_labels(stat_names)
  ))
}

#' Create default labels from stat names
#' @keywords internal
#' @examples
#' \dontrun{
#' create_default_labels("hr")
#' create_default_labels("No match found")
#' }
create_default_labels <- function(stat_names) {
  label_map <- c(
    n = "n",
    pct = "pct",
    mean = "Mean",
    sd = "SD",
    se = "SE",
    median = "Median",
    q1 = "Q1",
    q3 = "Q3",
    min = "Min",
    max = "Max",
    ci_lower = "CI Lower",
    ci_upper = "CI Upper",
    hr = "Hazard Ratio",
    or = "Odds Ratio",
    rr = "Risk Ratio",
    p = "p-value",
    total = "Total",
    estimate = "Estimate"
  )

  coalesce(label_map[stat_names], stat_names)
}

#' Get statistical patterns
#'
#' Returns the current global statistical patterns.
#'
#' @return Named list of statistical pattern definitions
#' @export
get_stat_patterns <- function() {
  if (is.null(.stateful_patterns$STAT_PATTERNS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$STAT_PATTERNS)
}

#' Initialize global pattern libraries
#' @keywords internal
.init_global_patterns <- function() {
  .stateful_patterns$BIGN_PATTERNS <- c(
    "\\(N\\s*=\\s*\\d+\\)", # (N = 123)
    "N\\s*=\\s*\\d+", # N = 123
    "N\\s*:\\s*\\d+", # N: 123
    "N\\s+\\d+", # N 123
    "\\(n\\s*=\\s*\\d+\\)", # (n = 123)
    "n\\s*=\\s*\\d+", # n = 123
    "\\(N\\d+\\)", # (N123)
    "N\\d+" # N123
  )

  # Flexible metadata extraction patterns
  .stateful_patterns$TITLE_PATTERNS <- c(
    "Table\\s+\\d+", # Table 14.3.2.1.1.1
    "Table\\s+[A-Z]\\d+", # Table A1
    "^\\d+\\.\\d+", # 14.3.2.1.1.1
    "Appendix\\s+\\w+" # Appendix 16.2.7.4.1
  )

  .stateful_patterns$POPULATION_PATTERNS <- c(
    "All Treated Subjects?",
    "Safety Population",
    "ITT Population",
    "Intent[- ]to[- ]Treat",
    "Per[- ]Protocol",
    "Modified ITT",
    "Full Analysis Set",
    "Randomized Set"
  )

  .stateful_patterns$FOOTNOTE_INDICATORS <- c(
    "MedDRA Version.*CTC Version",
    "Includes events reported between",
    "Excludes data collected on or after",
    "Program Source:",
    "^\\s*\\([0-9]+\\)", # (1), (2), etc.
    "^\\s*\\{[a-z]\\}", # {a}, {b}, etc.
    "^\\s*[a-z]\\)", # a), b), etc.
    "\\d{2}[A-Z]{3}\\d{4}\\s+\\d{2}:\\d{2}" # Date/time stamps
  )

  .stateful_patterns$TABLE_HEADER_INDICATORS <- c(
    "SELECT AES BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "IMAES.*WITHIN.*BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "OESIS.*WITHIN.*BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "ENDOCRINE.*WITHIN.*BY.*CATEGORY.*\\| \\\\\\\\\\\\$",
    "Safety Parameters.*Grade.*Grade",
    "Any Grade.*Grade 3-4.*Any Grade.*Grade 3-4",
    "Within \\d+ days.*\\| \\\\\\\\\\\\$",
    "Last Dose.*\\| \\\\\\\\\\\\$",
    "; \\| \\\\\\\\\\\\$",
    "Category; \\| \\\\\\\\\\\\$"
  )

  # Pattern indicators for post-processing
  .stateful_patterns$PERCENTAGE_INDICATORS <- c(
    "\\(%\\)", # (%) in variable names
    "\\bpercent\\b", # "percent" text
    "\\b%\\b" # % symbol
  )

  .stateful_patterns$VARIABLE_NAMES <- list(
    bign = "BIGN",
    missing = "MISSING"
  )

  .stateful_patterns$STAT_NAMES <- list(
    count = "n",
    percentage = "p",
    total = "N",
    missing = "missing"
  )

  .stateful_patterns$STAT_PATTERNS <- list(
    # Common clinical trial formats - with trailing semicolon support
    "n_pct_semicolon" = list(
      template = "{n} ({pct});",
      regex = "^(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "n_pct" = list(
      template = "{n} ({pct}%)",
      regex = "^(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)%?\\s*\\)$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "n_pct_space" = list(
      template = "{n}{ }( {pct})",
      regex = "^(\\d+)\\{\\s*\\}\\(\\s*(\\d+\\.?\\d*)\\s*\\)$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "mean_sd" = list(
      template = "{mean} ({sd})",
      regex = "^(\\d+\\.?\\d*)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("mean", "sd"),
      labels = c("Mean", "SD")
    ),
    "median_iqr" = list(
      template = "{median} ({q1}, {q3})",
      regex = "^(\\d+\\.?\\d*)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*,\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("median", "q1", "q3"),
      labels = c("Median", "Q1", "Q3")
    ),
    "min_max" = list(
      template = "{min} - {max}",
      regex = "^(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*);?$",
      stats = c("min", "max"),
      labels = c("Min", "Max")
    ),
    # Single value patterns with semicolon support
    "count_semicolon" = list(
      template = "{n};",
      regex = "^(\\d+);$",
      stats = c("n"),
      labels = c("n")
    ),
    "count" = list(
      template = "{n}",
      regex = "^(\\d+)$",
      stats = c("n"),
      labels = c("n")
    ),
    "decimal_semicolon" = list(
      template = "{value};",
      regex = "^(\\d+\\.\\d+);$",
      stats = c("value"),
      labels = c("Value")
    ),
    "decimal" = list(
      template = "{value}",
      regex = "^(\\d+\\.\\d+)$",
      stats = c("value"),
      labels = c("Value")
    )
  )
}

#' Global Pattern Registry Environment
#' @keywords internal
.stateful_patterns <- new.env(parent = emptyenv())

#' Initialize patterns when package loads
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize with minimal BIGN patterns (will be replaced by pseudo patterns)
  .stateful_patterns$BIGN_PATTERNS <- NULL # Will use pseudo-pattern system

  # Initialize with basic patterns
  .stateful_patterns$STAT_PATTERNS <- list(
    "n_pct" = list(
      template = "{n} ({pct}%)",
      regex = "^\\s*([^\\s()]+)\\s*\\(([^\\s()]+)%?\\)\\s*$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "count" = list(
      template = "{n}",
      regex = "^\\s*([^\\s()]+)\\s*$",
      stats = c("n"),
      labels = c("n")
    )
  )

  # Initialize pseudo-patterns after loading
  # This will be called after pseudo_pattern.R is loaded
  setHook(packageEvent("stateful", "onLoad"), function(...) {
    if (exists("update_stat_patterns_to_pseudo", mode = "function")) {
      update_stat_patterns_to_pseudo()
    }
  })
}

#' Set statistical patterns
#'
#' Updates the global statistical patterns used for parsing.
#'
#' @param patterns Named list of pattern definitions
#' @export
set_stat_patterns <- function(patterns) {
  .stateful_patterns$STAT_PATTERNS <- patterns
  invisible(.stateful_patterns$STAT_PATTERNS)
}
