plan <- bind_plans(plan, drake_plan(
  ts_common = ggplot(all_sims) +
    aes(x = year, y = value, group = id) +
    geom_line() +
    facet_wrap(vars(variable), scales = "free_y") +
    scale_color_gradient(low = "grey", high = "red") +
    ylab("") +
    theme_bw() +
    theme(
      legend.position = "bottom"
    ),
  ts = target(
    ts_common + aes(color = .y),
    transform = map(.y = !!params)
  ),
  ts_both = target(
    plot_grid(ts),
    transform = combine(ts)
  ),
  ts_both_png = save_plot(
    file_out(!!here::here("analysis", "figures", "ts_both.png")),
    ts_both
  )
))
