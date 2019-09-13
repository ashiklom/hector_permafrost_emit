plan <- bind_plans(plan, drake_plan(
  global_inparams = read_csv(file_in(!!here::here(
    "analysis", "data", "derived_data", "parameter-draws.csv"
  ))),
  biome_inparams = read_csv(file_in(!!here::here(
    "analysis", "data", "derived_data", "biome-parameter-draws.csv"
  ))) %>%
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
    geom_area() +
    lims(y = c(0, 1)) +
    labs(x = "Value", y = "Normalized probability density") +
    facet_wrap(vars(param), scales = "free_x") +
    cowplot::theme_cowplot(),
  param_density_plot_file = ggsave(
    file_out(!!here::here("analysis", "figures", "param-density.png")),
    param_density_plot,
    width = 10.3, height = 7.3
  )
))