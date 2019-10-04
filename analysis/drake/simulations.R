ggpairs_density <- function(data, mapping, ..., low = "grey80", high = "red4") {
  ggplot(data = data, mapping = mapping) +
    geom_hex(...) +
    scale_fill_gradient(low = low, high = high)
}

get_timestamp <- function(osf_id) {
  stopifnot(requireNamespace("osfr", quietly = TRUE))
  osfr::osf_retrieve_file(osf_id) %>%
    dplyr::pull(meta) %>%
    purrr::pluck(1, "attributes", "date_modified")
}

osf_url <- function(osf_id) file.path("https://osf.io/download", osf_id)

### OSF download simulations
sims_outdir <- fs::dir_create(here::here("analysis", "data", "output"))
global_sims_file <- fs::path(sims_outdir, "global-sims.fst")
global_sims_osf <- "z5nwa"
biome_sims_file <- fs::path(sims_outdir, "biome-sims.fst")
biome_sims_osf <- "rstg3"

plan <- bind_plans(plan, drake_plan(
  global_sims_dl = target(
    download.file(osf_url(global_sims_osf), file_out(!!global_sims_file)),
    trigger = trigger(change = get_timestamp(global_sims_osf))
  ),
  biome_sims_dl = target(
    download.file(osf_url(biome_sims_osf), file_out(!!biome_sims_file)),
    trigger = trigger(change = get_timestamp(biome_sims_osf))
  )
))

### Load simulations and draw raw results
plan <- bind_plans(plan, drake_plan(
  global_sims = file_in(!!global_sims_file) %>%
    fst::read_fst() %>%
    as_tibble(),
  biome_sims = file_in(!!biome_sims_file) %>%
    fst::read_fst() %>%
    as_tibble(),
  draws = target(
    .sims %>%
      select(-(scenario:units)) %>%
      distinct() %>%
      GGally::ggpairs(lower = list(continuous = ggpairs_density)) +
      theme_bw(),
    transform = map(.sims = c(global_sims, biome_sims))
  ),
  draws_png = target(
    file_out(!!here("analysis", "figures", fname)) %>%
      ggplot2::ggsave(plot = draws, width = width, height = height),
    transform = map(draws, fname = c("global_draws.png", "biome_draws.png"),
                    width = c(7, 9), height = c(7, 9),
                    .id = draws)
  )
))
