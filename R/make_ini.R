#' Add exogenous CSV data to a Hector INI file
#'
#' @param exo_csv Path to exogenous emissions CSV file
#' @param ini_in Path to baseline Hector INI file
#' @param ini_out Target path for output INI file
#' @return `ini_out`, invisibly
#' @author Alexey Shiklomanov
#' @export
make_ini <- function(exo_csv, ini_in, ini_out) {
  stopifnot(file.exists(exo_csv), file.exists(ini_in))
  exo_csvn <- normalizePath(exo_csv, mustWork = TRUE)
  input <- readLines(ini_in)
  output <- input
  # First, remove any lines already containing exo_emissions
  output <- grep("exo_emissions", output, value = TRUE, invert = TRUE)
  # Then, insert the line at the beginning of the simpleNbox section
  nbox_start <- grep("\\[simpleNbox\\]", output)
  stopifnot(length(nbox_start) == 1)
  output <- append(
    output,
    paste0("exo_emissions = csv:", exo_csvn),
    nbox_start
  )
  # Do the same thing for exo_ch4_emissions
  ch4_start <- grep("\\[CH4\\]", output)
  stopifnot(length(ch4_start) == 1)
  output <- append(
    output,
    paste0("exo_ch4_emissions = csv:", exo_csvn),
    ch4_start
  )
  writeLines(output, ini_out)
  invisible(ini_out)
}
