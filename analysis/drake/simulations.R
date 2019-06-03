ggpairs_density <- function(data, mapping, ..., low = "grey80", high = "red4") {
  ggplot(data = data, mapping = mapping) +
    geom_hex(...) +
    scale_fill_gradient(low = low, high = high)
}

plan <- bind_plans(plan, drake_plan(
  global_sims = file_in(!!here("analysis", "data",
                            "output", "global-sims.fst")) %>%
    fst::read_fst() %>%
    as_tibble(),
  biome_sims = file_in(!!here::here("analysis", "data",
                                    "output", "biome-sims.fst")) %>%
    fst::read_fst() %>%
    as_tibble(),
  draws = target(
    .sims %>%
      select(-(scenario:units)) %>%
      GGally::ggpairs(lower = list(continuous = ggpairs_density)) +
      theme_bw(),
    transform = map(.sims = c(global_sims, biome_sims))
  )
))

## plan <- bind_plans(plan, drake_plan(
##   draws_plot = GGally::ggpairs(draws),
##   sims = target(
##     hector_with_params(
##       beta = beta,
##       q10_rh = q10_rh,
##       f_nppv = f_nppv,
##       f_nppd = f_nppd,
##       f_litterd = f_litterd
##     ),
##     transform = map(!!!draws)
##   ),
##   all_sims = target(
##     as_tibble(bind_rows(sims, .id = "id")),
##     transform = combine(sims)
##   ),
##   all_sims_out = write_csv(all_sims, file_out("analysis/data/output/s1_sims.csv"))
## ))
