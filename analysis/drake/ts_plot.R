plan <- bind_plans(plan, drake_plan(
  ts = target(
    .sims %>%
      group_by(year, variable, units) %>%
      summarize(Mean = mean(value),
                Median = median(value),
                SD = sd(value),
                lo1 = quantile(value, 0.05),
                hi1 = quantile(value, 0.95),
                lo2 = quantile(value, 0.025),
                hi2 = quantile(value, 0.975)) %>%
      ggplot() +
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
    transform = cross(.sims = c(global_sims, biome_sims))
  )
))
