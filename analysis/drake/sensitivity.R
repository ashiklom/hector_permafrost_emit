plan <- bind_plans(plan, drake_plan(
  sensitivity = lastyear %>%
    group_by(variable) %>%
    sensitivity_analysis(!!!params) %>%
    tidy_sensitivity(),
  sensitivity_plot = sensitivity %>%
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
  sensitivity_plot_png = save_plot(
    file_out(!!here::here("analysis", "figures", "sensitivity_plot.png")),
    sensitivity_plot
  )
))
