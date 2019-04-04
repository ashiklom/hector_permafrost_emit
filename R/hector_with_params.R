#' Run Hector with parameters set to specific values
#'
#' To specify units, set the `unit` attribute of the parameter. 
#'
#' @param ... Named list of parameter values (e.g. `beta = 0.3`).
#'   Names must match Hector parameter names. 
#' @param .dots A named list of arguments. Provides an alternative
#'   specification to `...`
#' @param rcp
#' @return 
#' @author Alexey Shiklomanov
#' @examples
#' hector_with_params(beta = 0.5, q10_rh = )
#' @export
hector_with_params <- function(..., .dots = list(), rcp = "45") {
  params <- modifyList(.dots, list(...))
  ini_file <- system.file(
    "input",
    paste0("hector_rcp", rcp, ".ini"),
    package = "hector"
  )
  core <- hector::newcore(
    ini_file,
    name = "sensitivity",
    suppresslogging = TRUE
  )
  purrr::iwalk(
    params,
    ~hector::setvar(core, NA, .y, .x, NA)
  )
  tryCatch({
    hector::run(core)
    hector::fetchvars(core, 2000:2100) %>%
      dplyr::mutate(!!!params)
  }, error = function(e) {
    message("Run failed. Returning NULL.")
    return(NULL)
  })
}

set_parameter <- function(core, name, value) {
  unit <- attr(value, "unit")
  if (is.null(unit)) unit <- NA
  tryCatch(
    hector::setvar(core, NA, name, value, unit),
    error = function(e) {
      stop(sprintf(paste0(
        "Hit the following error while ",
        "trying to set parameter `%s`:\n%s"
      ), name, conditionMessage(e)))
    }
  )
}
