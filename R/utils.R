#' @noRd
'%!in%' <- function(x, y) !('%in%'(x, y))

#' @noRd
prinf <- function(x) print(x, n = Inf)


#' Null coalescing operator
#' @param x First value
#' @param y Second value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
