#' Create a Hector core with a split biome
#'
#' @param biome_name (Character) Name of new biome
#' @param frac_veg Fraction of global vegetation C in new biome.
#'   Default is 0.5.
#' @param frac_soil Fraction of global soil C in new biome. Default is `frac_veg`.
#' @param frac_detritus Fraction of global detritus C in new biome.
#'   Default is `frac_soil` (which in turn defaults to `frac_veg`)
#' @param frac_npp_flux0 Fraction if global initial NPP flux in new
#'   biome. Default is `frac_veg`.
#' @inheritParams hector_with_params
#' @return Hector core with biomes `"global"` and `biome_name`
#' @author Alexey Shiklomanov
#' @export
split_biome <- function(biome_name,
                        frac_veg = 0.5,
                        frac_soil = frac_veg,
                        frac_detritus = frac_soil,
                        frac_npp_flux0 = frac_veg,
                        rcp = "45") {
  ini_file <- system.file("input", paste0("hector_rcp", rcp, ".ini"),
                          package = "hector")
  ini <- hectortools::read_ini(ini_file)
  frac_tbl <- tibble::tribble(
    ~variable, ~frac,
    hector::VEGC(), frac_veg,
    hector::SOILC(), frac_soil,
    hector::DETRITUSC(), frac_detritus,
    hector::NPP_FLUX0(), frac_npp_flux0
  )
  inits <- ini[["simpleNbox"]][gsub("global\\.", "", frac_tbl[["variable"]])] %>%
    unlist() %>%
    tibble::enframe("variable", "value") %>%
    dplyr::mutate(variable = paste0("global.", variable))
  new_biome <- inits %>%
    dplyr::left_join(frac_tbl, by = "variable") %>%
    dplyr::mutate(variable = gsub("global", biome_name, variable),
                  value = value * frac) %>%
    dplyr::select(-frac)
  orig_biome <- inits %>%
    dplyr::left_join(frac_tbl, by = "variable") %>%
    dplyr::mutate(value = value * (1 - frac)) %>%
    dplyr::select(-frac)
  # Also, clone parameters
  param_names <- c(
    hector::BETA(),
    hector::Q10_RH(),
    hector::F_NPPD(),
    hector::F_NPPV(),
    hector::F_LITTERD(),
    hector::WARMINGFACTOR()
  )
  params <- ini[["simpleNbox"]][gsub("global\\.", "", param_names)] %>%
    setNames(param_names)
  if (is.null(params[["global.warmingfactor"]])) {
    params[["global.warmingfactor"]] <- 1
  }
  new_params <- setNames(params, gsub("global", biome_name, param_names))
  remove_values <- rep(list(NULL), length(param_names) + nrow(frac_tbl)) %>%
    setNames(gsub("global\\.", "", c(param_names, frac_tbl[["variable"]])))
  new_values <- list(simpleNbox = c(
    as.list(tibble::deframe(orig_biome)),
    as.list(tibble::deframe(new_biome)),
    params, new_params, remove_values
  ))
  new_ini <- modifyList(ini, new_values)
  hectortools::newcore_ini(new_ini)
}
