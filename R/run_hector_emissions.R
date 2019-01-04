#' Run Hector with custom emissions
#'
#' @param rcp Character describing RCP to use as baseline. Must be one
#'   of `"26", "45", "60", "85"`.
#' @param exo_emissions_df `data.frame` containing exogenous emissions
#'   data. Must contain column `Date` (year of emissions), and one (or
#'   both) of `exo_emissions` (exogenous CO2 emissions) or
#'   `exo_ch4_emissions` (for CH4).
#' @inheritParams hector::newcore
#' @inheritDotParams hector::newcore
#' @return Hector output, as `data.frame` (see [hector::fetchvars()]). 
#' @author Alexey Shiklomanov
#' @export
run_hector_emissions <- function(rcp, exo_emissions_df = NULL,
                                 suppresslogging = TRUE,
                                 ...) {
  stopifnot(rcp %in% c("26", "45", "60", "85"))
  inifile <- system.file(
    "input",
    paste0("hector_rcp", rcp, ".ini"),
    package = "hector"
  )
  stopifnot(file.exists(inifile))
  hc <- hector::newcore(inifile, suppresslogging = suppresslogging, ...)
  on.exit(hector::shutdown(hc))

  if (!is.null(exo_emissions_df)) {
    stopifnot(
      is.data.frame(exo_emissions_df),
      "Date" %in% colnames(exo_emissions_df)
    )
    exo_years <- exo_emissions_df[["Date"]]
    if ("exo_emissions" %in% colnames(exo_emissions_df)) {
      hector::setvar(
        hc,
        exo_years,
        "exo_emissions",
        exo_emissions_df[["exo_emissions"]],
        "Pg C/yr"
      )
    }
    if ("exo_ch4_emissions" %in% colnames(exo_emissions_df)) {
      hector::setvar(
        hc,
        exo_years,
        "exo_ch4_emissions",
        exo_emissions_df[["exo_ch4_emissions"]],
        "Tg CH4"
      )
    }
  }

  hector::run(hc)
  result <- hector::fetchvars(
    hc,
    seq(hector::startdate(hc), hector::enddate(hc))
  )
  result
}
