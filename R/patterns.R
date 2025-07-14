# ==============================================================================
# CORE PATTERN DEFINITIONS
# ==============================================================================

#' Get default statistical patterns
#' @return Named list of pattern definitions
get_default_patterns <- function() {
  list(
    # Categorical patterns (most common first)
    "n_pct" = list(
      regex = "^\\s*(\\d+)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*%?\\s*\\)\\s*;?$",
      stats = c("n", "pct"),
      labels = c("Count", "Percentage")
    ),
    "count_total" = list(
      regex = "^\\s*(\\d+)\\s*/\\s*(\\d+)\\s*;?$",
      stats = c("n", "total"),
      labels = c("Count", "Total")
    ),
    "count_only" = list(
      regex = "^\\s*(\\d+)\\s*;?$",
      stats = c("n"),
      labels = c("Count")
    ),
    "pct_only" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*%\\s*;?$",
      stats = c("pct"),
      labels = c("Percentage")
    ),

    # Continuous patterns
    "mean_sd" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*;?$",
      stats = c("mean", "sd"),
      labels = c("Mean", "SD")
    ),
    "median_iqr" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*;?$",
      stats = c("median", "q1", "q3"),
      labels = c("Median", "Q1", "Q3")
    ),
    "range" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*-\\s*(\\d+(?:\\.\\d+)?)\\s*;?$",
      stats = c("min", "max"),
      labels = c("Min", "Max")
    ),

    # Confidence intervals
    "ci" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*;?$",
      stats = c("estimate", "ci_lower", "ci_upper"),
      labels = c("Estimate", "CI Lower", "CI Upper")
    ),
    "ci_dash" = list(
      regex = "^\\s*(\\d+(?:\\.\\d+)?)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*-\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*;?$",
      stats = c("estimate", "ci_lower", "ci_upper"),
      labels = c("Estimate", "CI Lower", "CI Upper")
    ),

    # Decimal values
    "decimal" = list(
      regex = "^\\s*(\\d+\\.\\d+)\\s*;?$",
      stats = c("value"),
      labels = c("Value")
    )
  )
}

#' Get context-based pattern priorities
#' @return Named list mapping context types to preferred patterns
get_context_priorities <- function() {
  list(
    continuous = c("mean_sd", "median_iqr", "range", "decimal"),
    categorical = c("n_pct", "count_total", "count_only", "pct_only"),
    confidence = c("ci", "ci_dash"),
    default = c("n_pct", "mean_sd", "count_only", "decimal")
  )
}

# ==============================================================================
# CONTEXT DETECTION
# ==============================================================================

#' Detect variable context type
#' @param variable_context Variable name or label
#' @return Context type: "continuous", "categorical", "confidence", or "default"
detect_context_type <- function(variable_context) {
  if (
    is.null(variable_context) ||
      is.na(variable_context) ||
      variable_context == ""
  ) {
    return("default")
  }

  variable_lower <- tolower(trimws(variable_context))

  # Define context indicators
  context_indicators <- list(
    continuous = c(
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
      "score"
    ),
    categorical = c(
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
      "response"
    ),
    confidence = c(
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
  )

  # Check each context type
  for (context_type in names(context_indicators)) {
    if (
      any(sapply(context_indicators[[context_type]], function(x) {
        grepl(x, variable_lower)
      }))
    ) {
      return(context_type)
    }
  }

  "default"
}

#' Reorder patterns based on context
#' @param patterns List of patterns
#' @param context_type Context type from detect_context_type()
#' @return Reordered patterns list
reorder_patterns_by_context <- function(patterns, context_type) {
  priorities <- get_context_priorities()
  preferred_order <- priorities[[context_type]] %||% priorities$default

  # Get patterns in preferred order, then add any remaining
  available_preferred <- preferred_order[preferred_order %in% names(patterns)]
  remaining <- names(patterns)[!names(patterns) %in% available_preferred]

  patterns[c(available_preferred, remaining)]
}

# ==============================================================================
# PATTERN MATCHING
# ==============================================================================

#' Try to match a pattern against a value
#' @param value Character string to match
#' @param pattern Pattern definition list
#' @return Tibble with results or NULL if no match
try_pattern_match <- function(value, pattern) {
  if (!grepl(pattern$regex, value)) {
    return(NULL)
  }

  matches <- regmatches(value, regexec(pattern$regex, value))[[1]]
  if (length(matches) <= 1) {
    return(NULL)
  }

  # Extract captured groups (skip full match)
  captured_values <- matches[-1]

  # Clean percentage values
  for (i in seq_along(captured_values)) {
    if (length(pattern$stats) >= i && pattern$stats[i] == "pct") {
      captured_values[i] <- gsub("%$", "", captured_values[i])
    }
  }

  tibble(
    stat = captured_values,
    stat_name = pattern$stats,
    stat_label = pattern$labels
  )
}

#' Create result for unmatched values
#' @param stat_value Original value
#' @return Tibble with "other" classification
create_unmatched_result <- function(stat_value) {
  tibble(
    stat = stat_value,
    stat_name = "other",
    stat_label = "Other"
  )
}

#' Create result for missing values
#' @param stat_value Original value
#' @return Tibble with "missing" classification
create_missing_result <- function(stat_value) {
  tibble(
    stat = stat_value,
    stat_name = "missing",
    stat_label = "Missing"
  )
}

# ==============================================================================
# MAIN PARSING FUNCTION
# ==============================================================================

#' Parse statistical value with context awareness
#' @param stat_value Character string containing the statistic to parse
#' @param variable_context Variable name/label for context-based pattern
#' selection
#' @param patterns Optional list of patterns to use (defaults to built-in
#' patterns)
#' @return Tibble with parsed statistics
#' @examples
#' \dontrun{
#' parse_stat_value("24")
#' parse_stat_value("12 (30%)")
#' parse_stat_value("12.5 (2.1)", variable_context = "mean_score")
#' }
parse_stat_value <- function(
  stat_value,
  variable_context = NULL,
  patterns = NULL
) {
  # Input validation and cleaning
  stat_value <- trimws(as.character(stat_value))

  if (is.na(stat_value) || stat_value == "") {
    return(create_missing_result(stat_value))
  }

  # Get patterns - use defaults if not provided
  if (is.null(patterns)) {
    patterns <- get_default_patterns()
  }

  # Detect context and reorder patterns
  context_type <- detect_context_type(variable_context)
  patterns <- reorder_patterns_by_context(patterns, context_type)

  # Try each pattern (first match wins)
  for (pattern_name in names(patterns)) {
    result <- try_pattern_match(stat_value, patterns[[pattern_name]])
    if (!is.null(result)) {
      return(result)
    }
  }

  # No pattern matched
  create_unmatched_result(stat_value)
}
