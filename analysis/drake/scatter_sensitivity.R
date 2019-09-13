scatter_grid <- function(dat) {
  params <- setdiff(colnames(dat), c("id", "scenario", "year",
                                     "variable", "value", "units")) %>%
    rlang::syms()
  baseplot <- ggplot(dat) +
    aes(y = value) +
    facet_wrap(vars(variable), scales = "free_y") +
    ## geom_point() +
    geom_hex() +
    geom_smooth(method = "gam", color = "black") +
    ylab("") +
    theme_bw() +
    guides(fill = FALSE) +
    scale_fill_gradient(low = "grey80", high = "red4")
  plot_list <- lapply(params, function(X) baseplot + aes(x = !!X))
  do.call(plot_grid, plot_list)
}

plan <- bind_plans(plan, drake_plan(
  # Scatter plot of values in 2100
  lastyear = target(
    filter(.sims, year == 2100),
    transform = map(.sims = c(global_sims, biome_sims))
  ),
  scatter = target(
    scatter_grid(lastyear),
    transform = map(lastyear)
  ),
  scatter_png = target(
    ggsave(
      file_out(!!here::here("analysis", "figures", .fname)),
      scatter,
      width = .width, height = .height
    ),
    transform = map(scatter, .fname = c("global-scatter.png", "biome-scatter.png"),
                    .width = c(8, 10), .height = c(5, 8))
  ),
  sensitivity = target(
    lastyear %>%
      group_by(variable) %>%
      nest() %>%
      mutate(sens = purrr::map(
        data,
        sensitivity_analysis,
        xcols = as.character(.params),
        ycol = "value"
      )) %>%
      select(-data) %>%
      unnest(sens),
    transform = map(lastyear, .params = !!param_types)
  ),
  sensitivity_plot = target(
    sensitivity %>%
      gather(stat, value, cv:partial_var) %>%
      filter(stat != "pred_var") %>%
      ggplot() +
      aes(x = param, y = value) +
      geom_segment(aes(x = param, y = 0, xend = param, yend = value)) +
      geom_point() +
      coord_flip() +
      facet_grid(cols = vars(stat), rows = vars(variable), scales = "free_x") +
      theme_bw(),
    transform = map(sensitivity)
  )
))
