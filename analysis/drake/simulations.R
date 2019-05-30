plan <- bind_plans(plan, drake_plan(
  draws_plot = GGally::ggpairs(draws),
  sims = target(
    hector_with_params(
      beta = beta,
      q10_rh = q10_rh,
      f_nppv = f_nppv,
      f_nppd = f_nppd,
      f_litterd = f_litterd
    ),
    transform = map(!!!draws)
  ),
  all_sims = target(
    as_tibble(bind_rows(sims, .id = "id")),
    transform = combine(sims)
  )
))
