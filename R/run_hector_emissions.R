#' Run Hector for scenario, given as `data.frame`
#'
#' @inheritParams emissions2ini
#' @inheritDotParams emissions2ini
#' @return
#' @author Alexey Shiklomanov
#' @export
run_hector_emissions <- function(emissions, ...) {
  ini <- emissions2ini(emissions, ...)
  result <- hector::runscenario(ini)
  result
}

#' Create Hector INI file from emissions `data.frame`
#'
#' @param emissions `data.frame` containing Hector emissions data
#' @param base_ini Path to base INI file. Default is Hector RCP 4.5
#'   INI file.
#' @param replace_string Regular expression for emissions string to
#'   replace. Default is `"emissions/RCP45_emissions\\.csv"`.
#' @param replace_exo Regular expression for exogenous emissions. This
#'   will be deleted because, by default, we pass two values, not a
#'   CSV file. Default = `"exo_emissions.*=0"`
#' @param emissions_outfile Target output emissions CSV file. Default
#'   = `tempfile(fileext = ".csv")`.
#' @param ini_outfile Target output INI file. Default =
#'   `tempfile(fileext = ".ini")`.
#' @return Path to INI file
#' @author Alexey Shiklomanov
emissions2ini <- function(emissions,
                          base_ini = system.file(
                            "input/hector_rcp45.ini",
                            package = "hector"
                          ),
                          replace_string = "emissions/RCP45_emissions\\.csv",
                          replace_exo = "exo_emissions.*=0",
                          emissions_outfile = tempfile(fileext = ".csv"),
                          ini_outfile = tempfile(fileext = ".ini")) {

  stopifnot(file.exists(base_ini))
  ini_base <- readLines(base_ini)

  readr::write_csv(emissions, emissions_outfile)

  if (!any(grepl(replace_string, ini_base))) {
    stop(
      "Target replacement string '", replace_string, "' ",
      "not found in INI file '", base_ini, "'."
    )
  }
  ini_new <- gsub(replace_string, emissions_outfile, ini_base)

  # Replace exogenous emissions
  nexo <- grep(replace_exo, ini_new)
  stopifnot(length(nexo) >= 1)
  ini_new[head(nexo, 1)] <- paste0("exo_emissions=csv:", emissions_outfile)
  ini_new[tail(nexo, -1)] <- ""

  # Use full path, not relative path, for volcanic emissions
  ini_new <- gsub(
    "emissions/volcanic_RF\\.csv",
    system.file("input/emissions/volcanic_RF.csv", package = "hector"),
    ini_new
  )
  writeLines(ini_new, ini_outfile)

  ini_outfile
}
