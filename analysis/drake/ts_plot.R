plan <- bind_plans(plan, drake_plan(
  ts_summary = target(
    .sims %>%
      group_by(year, variable, units) %>%
      summarize(
        Mean = mean(value),
        Median = median(value),
        SD = sd(value),
        lo1 = quantile(value, 0.05),
        hi1 = quantile(value, 0.95),
        lo2 = quantile(value, 0.025),
        hi2 = quantile(value, 0.975)
      ),
    transform = map(.sims = c(global_sims, biome_sims))
  ),
  sims_summary = target(
    bind_rows(ts_summary, .id = "simulation_type") %>%
      mutate(simulation_type = forcats::fct_recode(
        simulation_type,
        "global" = "1",
        "biome" = "2"
      )),
    transform = combine(ts_summary)
  ),
  sims_summary_plot = ggplot(sims_summary) +
    aes(x = year, color = simulation_type) +
    geom_line(aes(y = Mean)) +
    ## geom_line(aes(y = lo2), linetype = "dashed") +
    ## geom_line(aes(y = hi2), linetype = "dashed") +
    geom_ribbon(aes(ymin = lo2, ymax = hi2, fill = simulation_type),
                alpha = 0.2, color = NA) +
    ## geom_ribbon(aes(ymin = lo1, ymax = hi1), alpha = 0.7) +
    facet_wrap(vars(variable), scales = "free_y") +
    labs(x = "Year", y = "Mean +/- 95% CI") +
    cowplot::theme_cowplot(),
  sims_summary_plot_png = ggsave(
    file_out(!!here::here("analysis", "figures", "ts-summary.png")),
    sims_summary_plot,
    width = 6.62, height = 4.66
  ),
  ts = target(
    ggplot(ts_summary) +
      aes(x = year) +
      geom_ribbon(aes(ymin = lo2, ymax = hi2, fill = "95% CI")) +
      geom_ribbon(aes(ymin = lo1, ymax = hi1, fill = "90% CI")) +
      scale_fill_manual(values = c("deepskyblue3", "deepskyblue1")) +
      geom_line(aes(y = Mean)) +
      facet_wrap(vars(variable), scales = "free_y") +
      ylab("") +
      theme_bw() +
      theme(
        legend.position = "bottom"
      ),
    transform = map(ts_summary)
  )
))
