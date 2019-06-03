global_sims_head <- fst::read_fst(here::here(
  "analysis", "data", "output", "global-sims.fst"
), to = 1)
global_params <- global_sims_head %>%
  select(-(scenario:units)) %>%
  colnames() %>%
  rlang::syms()

biome_sims_head <- here::here("analysis", "data", "output", "biome-sims.fst") %>%
  fst::read_fst(to = 1)
biome_params <- biome_sims_head %>%
  select(-(scenario:units)) %>%
  colnames() %>%
  rlang::syms()

param_types <- rlang::syms(c("global_params", "biome_params"))

biomes_params <- bind_rows(
  tibble(model = list(rlang::sym("global_sims")), params = global_params),
  tibble(model = list(rlang::sym("biome_sims")), params = biome_params)
)
