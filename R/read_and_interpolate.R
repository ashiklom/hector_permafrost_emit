#' Read raw emissions data file and interpolate to target years
#'
#' @param file Raw emissions data file
#' @param start_year Target start year for interpolation. If `NULL`
#'   (default), use smallest year in input data.
#' @param end_year Target end year for interpolation. If `NULL`
#'   (default), use largest year in input data.
#' @param scale Numeric factor by which to multiply values, e.g. to
#'   convert units (default = 1; i.e. no conversion).
#' @param ... Additional arguments to [readr::read_csv()].
#' @return `data.frame` containing interpolated data, with columns
#'   `year` and `value`.
#' @author Alexey Shiklomanov
#' @export
read_and_interpolate <- function(file,
                                 start_year = NULL,
                                 end_year = 2100,
                                 scale = 1,
                                 ...) {
  stopifnot(file.exists(file))

  dat <- readr::read_csv(file, col_names = FALSE, col_types = "dd", ...)
  colnames(dat) <- c("year", "value")

  ymin <- ceiling(min(dat[["year"]]))
  if (is.null(start_year)) start_year <- ymin
  if (start_year < ymin) {
    stop(
      "Start year ", start_year,
      " less than min available year ", ymin, "."
    )
  }

  ymax <- floor(max(dat[["year"]]))
  if (is.null(end_year)) end_year <- ymax
  if (end_year > ymax) {
    stop(
      "End year ", end_year,
      " greater than max available year ", ymax, "."
    )
  }

  years <- seq(start_year, end_year)
  values <- spline(x = dat[["year"]], y = dat[["value"]] * scale, xout = years)

  names(values) <- c("year", "value")

  tibble::as_tibble(values)
}
