dig <- function(...) digest::digest(..., file = TRUE)

plan <- bind_plans(plan, drake_plan(
  figs_dir_t = list(
    dig(file_in(!!figfile("param-density.png"))),
    dig(file_in(!!figfile("ts-summary.png"))),
    dig(file_in(!!figfile("global-scatter.png"))),
    dig(file_in(!!figfile("biome-scatter.png"))),
    dig(file_in(!!figfile("sensitivity-biome.png"))),
    dig(file_in(!!figfile("global_draws.png"))),
    dig(file_in(!!figfile("biome_draws.png")))
  ),
  paper = target(
    rmarkdown::render(knitr_in(!!paper_file), .format),
    transform = map(.format = c("pdf_document", "html_document"))
  )
))
