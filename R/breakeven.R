#' Compute breakeven inflation rates
#'
#' Calculates breakeven inflation as the spread between nominal and real
#' (inflation-linked) bond yields. This provides a market-based measure of
#' inflation expectations.
#'
#' @param nominal_yield Numeric vector. Nominal bond yields.
#' @param real_yield Numeric vector. Real (inflation-linked) bond yields, same
#'   length as `nominal_yield`.
#' @param maturity Numeric vector or `NULL`. Bond maturities in years. If
#'   provided, must be the same length as `nominal_yield`.
#'
#' @return An S3 object of class `"ik_breakeven"` with elements:
#' \describe{
#'   \item{breakeven}{If `maturity` is provided, a data.frame with columns:
#'     maturity, nominal, real, breakeven. Otherwise, a numeric vector of
#'     breakeven rates.}
#'   \item{maturity}{The maturity vector (or `NULL`).}
#' }
#'
#' @export
#' @examples
#' # Breakeven term structure
#' be <- ik_breakeven(
#'   nominal_yield = c(4.2, 4.5, 4.8, 5.0),
#'   real_yield = c(1.8, 2.0, 2.3, 2.5),
#'   maturity = c(2, 5, 10, 30)
#' )
#' print(be)
#' plot(be)
#'
#' # Simple breakeven (no maturity)
#' be2 <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
#' print(be2)
ik_breakeven <- function(nominal_yield, real_yield, maturity = NULL) {
  validate_numeric_vector(nominal_yield, "nominal_yield")
  validate_numeric_vector(real_yield, "real_yield")

  if (length(nominal_yield) != length(real_yield)) {
    cli_abort(
      "{.arg nominal_yield} and {.arg real_yield} must have the same length."
    )
  }

  if (!is.null(maturity)) {
    validate_numeric_vector(maturity, "maturity")
    if (length(maturity) != length(nominal_yield)) {
      cli_abort(
        "{.arg maturity} must have the same length as {.arg nominal_yield}."
      )
    }
  }

  be_vals <- nominal_yield - real_yield

  if (!is.null(maturity)) {
    be_result <- data.frame(
      maturity = maturity,
      nominal = nominal_yield,
      real = real_yield,
      breakeven = be_vals,
      stringsAsFactors = FALSE
    )
  } else {
    be_result <- be_vals
  }

  structure(
    list(
      breakeven = be_result,
      maturity = maturity
    ),
    class = "ik_breakeven"
  )
}

#' @export
print.ik_breakeven <- function(x, ...) {
  cli_h1("Breakeven Inflation Rates")

  if (is.data.frame(x$breakeven)) {
    for (i in seq_len(nrow(x$breakeven))) {
      cli_bullets(c(
        "*" = "{x$breakeven$maturity[i]}Y: {round(x$breakeven$breakeven[i], 3)}% (nominal {round(x$breakeven$nominal[i], 3)}%, real {round(x$breakeven$real[i], 3)}%)"
      ))
    }
  } else {
    cli_bullets(c(
      "*" = "Breakeven rate{?s}: {round(x$breakeven, 3)}%"
    ))
  }
  invisible(x)
}

#' @export
plot.ik_breakeven <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if (is.null(x$maturity)) {
    cli_warn("No maturity dimension available for plotting.")
    return(invisible(x))
  }

  df <- x$breakeven
  ylim <- range(c(df$nominal, df$real, df$breakeven), na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  par(mar = c(4, 4, 3, 1))
  plot(
    df$maturity, df$nominal,
    type = "b",
    pch = 19,
    col = "#B2182B",
    lwd = 2,
    ylim = ylim,
    xlab = "Maturity (Years)",
    ylab = "Yield (%)",
    main = "Breakeven Inflation Term Structure",
    ...
  )
  lines(df$maturity, df$real, type = "b", pch = 17, col = "#2166AC", lwd = 2)
  lines(df$maturity, df$breakeven, type = "b", pch = 15, col = "#4DAF4A",
        lwd = 2)
  grid(col = "grey90")
  legend(
    "topleft",
    legend = c("Nominal", "Real", "Breakeven"),
    col = c("#B2182B", "#2166AC", "#4DAF4A"),
    pch = c(19, 17, 15),
    lwd = 2,
    bty = "n"
  )

  invisible(x)
}
