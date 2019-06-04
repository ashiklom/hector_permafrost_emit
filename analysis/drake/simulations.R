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
  ),
  draws_png = target(
    file_out(!!here("analysis", "figures", fname)) %>%
      ggplot2::ggsave(plot = draws, width = width, height = height),
    transform = map(draws, fname = c("global_draws.png", "biome_draws.png"),
                    width = c(7, 9), height = c(7, 9),
                    .id = draws)
  )
))
