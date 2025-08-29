library(fields)
library(latex2exp)
library(kableExtra)
library(tidyverse)
library(knitr)
library(pdftools)
library(RanCh)

domain_names <- dimnames(RanCh::MMS_2019_counts)[[1]]
domain_filenames <- gsub(" ", "_", tolower(domain_names))

all_domains_data <- c()
for (i in seq_along(domain_names)) {
  outname <- sprintf("domain_%02d_%s.pdf", i, domain_filenames[i])
  rmarkdown::render(here("Rmarkdown", "domain_template.Rmd"),
                    output_file = here("documents", outname),
                    clean = TRUE,
                    params = list(i=i, domain_name = domain_names[i],
                                  domain_filename = domain_filenames[i],
                                  N = RanCh::MMS_2019_counts[i,,]))
}
