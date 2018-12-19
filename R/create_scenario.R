#' Create a Hector emissions scenario
#'
#' @param cc `data.frame` of total carbon (C) emissions, with columns
#'   `year` and `value` (emissions in GtC/yr).
#' @param ch4_frac Fraction of carbon emitted as methane. Length must
#'   be either 1 or `nrow(cc)`.
#' @param co2 `data.frame` of CO2 emissions (same structure as `cc`)
#' @param ch4 `data.frame` of CH4 emissions (same structure as `cc`)
#' @param base_scenario_data `data.frame` containing baseline
#'   scenario, onto which CO2 and CH4 emissions will be added. If
#'   `NULL` (default), read from `base_scenario_file`
#' @param base_scenario Character describing base scenario (default = "RCP45").
#' @param base_scenario_file Path to base emissions file (default is
#'   `hector` package RCP 4.5 emissions file).  
#' @param ... Additional arguments to [readr::read_csv()] for reading
#'   scenario file
#' @return `data.frame` containing Hector emissions, with additional
#'   CO2 and CH4 emissions added to target years.
#' @author Alexey Shiklomanov
#' @export
create_scenario <- function(cc = NULL, ch4_frac = NULL,
                            co2 = NULL, ch4 = NULL,
                            base_scenario_data = NULL,
                            base_scenario = "RCP45",
                            base_scenario_file = system.file(
                              paste0("input/emissions/",
                                     base_scenario,
                                     "_emissions.csv"),
                              package = "hector"
                            ),
                            ...) {
  if (is.null(co2) && is.null(ch4)) {
    if (is.null(cc)) {
      stop(
        "Missing C, CO2, and CH4. ",
        "You must provide either C and CH4 fraction, or both CO2 and CH4."
      )
    }
    if (is.null(ch4_frac)) {
      stop("Provided C, but missing CH4 fraction.")
    }
    if (!(length(ch4_frac) %in% c(1, nrow(cc)))) {
      stop(
        "CH4 fraction must be length 1 or `nrow(cc)`. ",
        "Input is length ", length(ch4_frac), "."
      )
    }
    co2 <- ch4 <- cc
    co2[["value"]] <- cc[["value"]] * (1 - ch4_frac)
    ch4[["value"]] <- cc[["value"]] * ch4_frac
  }

  if (is.null(co2)) stop("Missing CO2 flux.")
  if (is.null(ch4)) stop("Missing CH4 flux.")

  if (is.null(base_scenario_data)) {
    if (!file.exists(base_scenario_file)) {
      stop("Unable to find base scenario file: ", base_scenario_file)
    }
    readr_args <- modifyList(list(skip = 3), list(...))
    base_scenario_data <- do.call(readr::read_csv, readr_args)
  }

  stopifnot(
    is.data.frame(base_scenario_data),
    "ffi_emissions" %in% colnames(base_scenario_data),
    "CH4_emissions" %in% colnames(base_scenario_data)
  )

  new_scenario <- base_scenario_data

  co2_years <- new_scenario[["Date"]] %in% co2[["year"]]
  new_scenario[co2_years, "ffi_emissions"] <-
    new_scenario[co2_years, "ffi_emissions"] + co2[["value"]]
  
  ch4_years <- new_scenario[["Date"]] %in% ch4[["year"]]
  new_scenario[ch4_years, "CH4_emissions"] <-
    new_scenario[ch4_years, "CH4_emissions"] + ch4[["value"]]

  new_scenario
}
