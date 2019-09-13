plan <- bind_plans(plan, drake_plan(
  ## paper_md = rmarkdown::render(knitr_in(!!paper_file), "github_document"),
  paper_pdf = rmarkdown::render(knitr_in(!!paper_file), "pdf_document"),
  paper_html = rmarkdown::render(knitr_in(!!paper_file), "html_document")
))
