#' Run Hector with parameters set to specific values
#'
#' To specify units, set the `unit` attribute of the parameter. 
#'
#' @param ... Named list of parameter values (e.g. `beta = 0.3`).
#'   Names must match Hector parameter names, or arguments to
#'   [split_biome()].
#' @param .dots A named list of arguments. Provides an alternative
#'   specification to `...`
#' @param rcp Representative carbon pathway (RCP) to use. One of
#'   `"26"`, `"45"`, `"60"`, `"85"`
#' @param core (Optional) An existing Hector core to modify.
#' @return Hector output, as a `data.frame`
#' @author Alexey Shiklomanov
#' @examples
#' hector_with_params(beta = 0.5, q10_rh = 1.8)
#' hector_with_params(
#'   global.beta = 0.6,
#'   permafrost.beta = 0.8,
#'   biome_name = "permafrost",
#'   frac_veg = 0.2
#' )
#' @export
hector_with_params <- function(..., .dots = list(), rcp = "45", core = NULL) {
  raw_params <- modifyList(.dots, list(...))
  all_names <- names(raw_params)
  sb_names_all <- names(formals(split_biome))
  sb_names <- intersect(all_names, sb_names_all)
  if (is.null(core)) {
    ini_file <- system.file(
      "input",
      paste0("hector_rcp", rcp, ".ini"),
      package = "hector"
    )
    if (length(sb_names) > 0) {
      sb_params <- raw_params[sb_names]
      if (is.null(sb_params[["biome_name"]])) biome_name <- "permafrost"
      core <- tryCatch(
        do.call(split_biome, sb_params),
        error = function(e) {
          message("Hit the following error while creating core:\n",
                  conditionMessage(e), ".\n",
                  "Returning `NULL`.")
          return(NULL)
        })
      if (is.null(core)) return(NULL)
    } else {
      core <- hector::newcore(
        ini_file,
        name = "sensitivity",
        suppresslogging = TRUE
      )
    }
  }
  param_names <- setdiff(all_names, sb_names_all)
  params <- raw_params[param_names]
  .iout <- tryCatch({
    purrr::iwalk(
      params,
      ~hector::setvar(core, NA, .y, .x, NA)
    )
  }, error = function(e) {
    message("Failed to set parameter value. ",
            "Probably hit this error during `reset`. ",
            "Returning `NULL`.")
    return(NULL)
  })
  if (is.null(.iout)) return(NULL)
  tryCatch({
    hector::run(core)
    hector::fetchvars(core, 2000:2100) %>%
      dplyr::mutate(!!!params)
  }, error = function(e) {
    message("Run failed. Returning `NULL`.\n",
            "Hit the following error:\n",
            conditionMessage(e))
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
