## Print the hector_with_params call args to console:
## writeLines(sprintf("%1$s = %1$s,", as.character(biome_params)))

## plan <- bind_plans(plan, drake_plan(
##   biome_draws_plot = GGally::ggpairs(draws),
##   biome_sims = target(
##     hector_with_params(
##       global.beta = global.beta,
##       global.q10_rh = global.q10_rh,
##       global.f_litterd = global.f_litterd,
##       permafrost.beta = permafrost.beta,
##       permafrost.q10_rh = permafrost.q10_rh,
##       permafrost.f_litterd = permafrost.f_litterd,
##       global.f_nppv = global.f_nppv,
##       global.f_nppd = global.f_nppd,
##       permafrost.f_nppv = permafrost.f_nppv,
##       permafrost.f_nppd = permafrost.f_nppd,
##       frac_veg = frac_veg,
##       frac_soil = frac_soil,
##       frac_detritus = frac_detritus,
##       biome_name = "permafrost"
##     ),
##     transform = map(!!!biome_draws)
##   ),
##   biome_all_sims = target(
##     as_tibble(bind_rows(biome_sims, .id = "id")),
##     transform = combine(biome_sims)
##   ),
##   biome_sims_out = write_csv(biome_all_sims, file_out("analysis/data/output/biome-sims.csv"))
## ))

plan <- bind_plans(plan, drake_plan(
  biome_all_sims = fst::read_fst(file_in(!!here::here(
    "analysis", "data", "output", "global-sims.csv"
  ))) %>% as_tibble()
))
