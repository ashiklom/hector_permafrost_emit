plan <- bind_plans(plan, drake_plan(
  # Scatter plot of values in 2100
  lastyear = filter(all_sims, year == 2100),
  scatter_common = ggplot(lastyear) +
    aes(y = value) +
    facet_wrap(vars(variable), scales = "free_y") +
    geom_point() +
    ylab("") +
    theme_bw(),
  scatter = target(
    scatter_common + aes(x = .x),
    transform = map(.x = !!params)
  ),
  scatter_both = target(
    plot_grid(scatter),
    transform = combine(scatter)
  ),
  scatter_both_png = save_plot(
    file_out(!!here::here("analysis", "figures", "scatter_both.png")),
    scatter_both
  )
))
