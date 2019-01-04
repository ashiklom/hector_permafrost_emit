#' Process GCAM input XML files
#'
#' @param hector_ini Path to Hector INI file to use with scenario 
#' @param hector_xml_in Path to reference Hector XML file to modify
#' @param gcam_xml_in Path to reference GCAM XML file to modify
#' @param hector_xml_out Path to target Hector XML file
#' @param gcam_xml_out Path to target GCAM XML file
#' @return Named `character(2)` containing paths to Hector and GCAM
#'   output files, invisibly.
#' @author Alexey Shiklomanov
#' @export
make_xmls <- function(hector_ini,
                      hector_xml_in,
                      gcam_xml_in,
                      hector_xml_out,
                      gcam_xml_out) {
  stopifnot(
    file.exists(hector_ini),
    file.exists(hector_xml_in),
    file.exists(gcam_xml_in)
  )
  hector_ini <- normalizePath(hector_ini, mustWork = TRUE)
  hector_xml_out <- suppressWarnings(normalizePath(hector_xml_out))

  xmlh <- readLines(hector_xml_in)
  xmlh_out <- gsub(
    "<hector-ini-file>.*</hector-ini-file>",
    paste0("<hector-ini-file>", hector_ini, "</hector-ini-file>"),
    xmlh
  )
  writeLines(xmlh_out, hector_xml_out)

  xmlg <- readLines(gcam_xml_in)
  xmlg_out <- gsub(
    "<Value name *= *\"climate\">.*</Value>",
    paste0("<Value name = \"climate\">", hector_xml_out, "</Value>"),
    xmlg
  )
  writeLines(xmlg_out, gcam_xml_out)

  invisible(c(hector = hector_xml_out, gcam = gcam_xml_out))
}

if (FALSE) {
  hector_ini <- file.path(gcam_climate, "hector-gcam.ini")
  hector_xml_in <- file.path(gcam_root, "input", "gcamdata", "xml", "hector.xml")
  gcam_xml_in <- file.path(gcam_exe, "configuration_ref.xml")
  hector_xml_out <- hector_xml_in
}
