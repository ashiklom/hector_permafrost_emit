split_biome <- function(core, biome_name,
                        frac_veg = 0.5,
                        frac_soil = frac_veg,
                        frac_detritus = frac_soil,
                        frac_npp_flux0 = frac_veg) {
  frac_tbl <- tibble::tribble(
    ~variable, ~frac,
    hector::VEGC(), frac_veg,
    hector::SOILC(), frac_soil,
    hector::DETRITUSC(), frac_detritus,
    hector::NPP_FLUX0(), frac_npp_flux0
  )
  inits <- hector::fetchvars(core, NA, frac_tbl[["variable"]])
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
  params <- hector::fetchvars(core, NA, param_names)
  new_params <- params %>%
    dplyr::mutate(variable = gsub("global", biome_name, variable)) %>%
    dplyr::bind_rows(params)
  new_values <- dplyr::bind_rows(orig_biome, new_biome, new_params)
  set_values <- new_values %>%
    dplyr::select(dates = year, var = variable, value, unit = units) %>%
    purrr::pwalk(hector::setvar, core = core)
  invisible(hector::reset(core))
  new_values
}

if (FALSE) {

  ini <- system.file("input", "hector_rcp45.ini", package = "hector")
  core <- hector::newcore(ini, suppresslogging = FALSE)
  biome_name <- "boreal"
  frac_veg <- 0.5
  frac_soil <- frac_veg
  frac_detritus <- frac_soil
  frac_npp_flux0 <- frac_veg
  v <- split_biome(core, biome_name)
  ## hector::run(core)

}
