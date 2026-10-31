# Validation helpers -----------------------------------------------------------

#' Validate that input is a numeric vector with no NAs
#'
#' @param x Object to validate.
#' @param name Name of the argument (used in error messages).
#'
#' @return Invisible `x` if valid.
#'
#' @noRd
validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    cli_abort("{.arg {name}} must be a numeric vector, not {.cls {class(x)}}.")
  }
  if (anyNA(x)) {
    cli_abort("{.arg {name}} must not contain NA values.")
  }
  invisible(x)
}

#' Validate that input is a single positive integer
#'
#' @param x Object to validate.
#' @param name Name of the argument (used in error messages).
#'
#' @return Invisible `x` if valid.
#'
#' @noRd
validate_positive_integer <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) ||
      x != as.integer(x) || x < 1L) {
    cli_abort("{.arg {name}} must be a single positive integer.")
  }
  invisible(x)
}

#' Validate that input is a single finite numeric value
#'
#' @param x Object to validate.
#' @param name Name of the argument (used in error messages).
#'
#' @return Invisible `x` if valid.
#'
#' @noRd
validate_scalar <- function(x, name) {

  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x)) {
    cli_abort("{.arg {name}} must be a single finite numeric value.")
  }
  invisible(x)
}

#' Validate a data.frame has required columns with appropriate types
#'
#' All columns except "date" and "item" must be numeric.
#'
#' @param data Object to validate.
#' @param required_cols Character vector of required column names.
#'
#' @return Invisible `data` if valid.
#'
#' @noRd
validate_data <- function(data, required_cols) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data.frame, not {.cls {class(data)}}.")
  }
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli_abort(
      "{.arg data} is missing required column{?s}: {.field {missing_cols}}."
    )
  }
  numeric_cols <- setdiff(required_cols, c("date", "item"))
  for (col in numeric_cols) {
    if (!is.numeric(data[[col]])) {
      cli_abort(
        "Column {.field {col}} must be numeric, not {.cls {class(data[[col]])}}."
      )
    }
  }
  invisible(data)
}

#' Format a proportion as a percentage string
#'
#' @param x Numeric value (proportion).
#'
#' @return Character string (e.g., 0.05 becomes "5%").
#'
#' @noRd
fmt_pct <- function(x) {
  paste0(round(x * 100, 2), "%")
}

#' Format a proportion as percentage points
#'
#' @param x Numeric value (proportion).
#'
#' @return Character string (e.g., 0.02 becomes "2 pp").
#'
#' @noRd
fmt_pp <- function(x) {
  paste0(round(x * 100, 2), " pp")
}

# Set the RNG seed for the duration of the calling function only, restoring the
# caller's .Random.seed on exit. ik_sample_data() needs a fixed seed so its
# demonstration data are reproducible, but it should not leave the caller's
# random stream reseeded: every later draw in the session would otherwise be
# silently reset to the same sequence.
#
# Touching globalenv() is unavoidable: .Random.seed lives there by
# definition. This restores the caller's state rather than adding to it.
.local_seed <- function(seed, frame = parent.frame()) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  restore <- if (had_seed) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    substitute(
      assign(".Random.seed", value, envir = globalenv()),
      list(value = old)
    )
  } else {
    # No stream existed before the call, so leave none behind.
    quote(suppressWarnings(rm(".Random.seed", envir = globalenv())))
  }
  do.call(on.exit, list(restore, add = TRUE, after = FALSE), envir = frame)
  set.seed(seed)
  invisible(NULL)
}
