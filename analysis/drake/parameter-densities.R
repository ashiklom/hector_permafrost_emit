derived_dir <- fs::dir_create(here::here("analysis", "data", "derived-data"))
global_param_file <- fs::path(derived_dir, "parameter-draws.csv")
biome_param_file <- fs::path(derived_dir, "biome-parameter-draws.csv")

plan <- bind_plans(plan, drake_plan(
  global_inparams = read_csv(file_in(!!global_param_file)),
  biome_inparams = read_csv(file_in(!!biome_param_file)) %>%
    select(matches("warmingfactor|_c")),
  densities = target(
    purrr::map(.inparams, density, cut = 0) %>%
      purrr::map_dfr(~bind_cols(.x[c("x", "y")]), .id = "param"),
    transform = map(.inparams = c(global_inparams, biome_inparams))
  ),
  both_densities = target(bind_rows(densities), transform = combine(densities)),
  param_density_plot = both_densities %>%
    mutate(param = forcats::fct_inorder(param)) %>%
    group_by(param) %>%
    mutate(y = y / max(y)) %>%
    ungroup() %>%
    filter(!(param == "f_litterd" & x < 0.95)) %>%
    ggplot() +
    aes(x = x, y = y) +
    geom_area(fill = "gray85", color = "black") +
    lims(y = c(0, 1)) +
    labs(x = "Value", y = "Normalized probability density") +
    facet_wrap(vars(param), scales = "free_x") +
    theme(strip.background = element_blank()) +
    cowplot::theme_cowplot(),
  param_density_plot_file = ggsave(
    file_out(!!figfile("param-density.png")),
    param_density_plot,
    width = 10.3, height = 7.30
  )
))
