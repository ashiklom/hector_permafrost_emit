global_params_s <- c("beta", "q10_rh", "f_litterd", "f_nppv", "f_nppd")
global_params <- rlang::syms(global_params_s)

biome_params_s <- expand.grid(c("global", "permafrost"), global_params_s) %>%
  apply(1, paste, collapse = ".") %>%
  sort() %>%
  c("fveg_c", "fsoil_c", "fdetritus_c")
biome_params <- rlang::syms(biome_params_s)

# Similarly, get the list of biome parameter names.
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
