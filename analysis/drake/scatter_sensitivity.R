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
  sensitivity = target(
    lastyear %>%
      group_by(variable) %>%
      sensitivity_analysis(!!!.params) %>%
      tidy_sensitivity(),
    transform = map(lastyear, .params = !!param_types)
  ),
  sensitivity_plot = target(
    sensitivity %>%
      filter(
        parameter != "total",
        stat != "sens",
        stat != "var"
      ) %>%
      ggplot() +
      aes(x = parameter, y = value) +
      geom_segment(aes(x = parameter, y = 0, xend = parameter, yend = value)) +
      geom_point() +
      coord_flip() +
      facet_grid(cols = vars(stat), rows = vars(variable), scales = "free_x") +
      theme_bw(),
    transform = map(sensitivity)
  )
))
