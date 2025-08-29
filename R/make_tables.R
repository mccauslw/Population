library(tidyverse)
library(knitr)
library(kableExtra)
library(here)
library(RanCh)

domain.names <- c('Prior', dimnames(RanCh::MMS_2019_counts)[[1]])

table <- read.table(here("data", "population.txt"))
row.names(table) <- domain.names
tbl <- as_tibble(table)
tbl$domain <- domain.names

table_vars <- c('domain', 'als0.05', 'als0.05e', 'als0.50', 'als0.50e', 'als0.95', 'als0.95e')
col.names <- c('Domain', 'quant', 'nse', 'quant', 'nse', 'quant', 'nse')
digits = c(0, 2, 3, 2, 3, 2, 3)
alpha_q_table <- kbl(tbl[,table_vars], booktab = TRUE, digits = digits,
                     col.names = col.names, format='latex') %>%
  add_header_above(c(' ', 'p=0.05' = 2, 'p=0.5' = 2, 'p=0.95' = 2))
writeLines(alpha_q_table, con=here("paper/tables", "alpha_q_table.tex"))

table_vars <- c('domain', 'las0.05', 'las0.05e', 'las0.25', 'las0.25e', 'las0.50', 'las0.50e')
col.names <- c('Domain', 'quant', 'nse', 'quant', 'nse', 'quant', 'nse')
digits = c(0, 3, 4, 3, 4, 4, 5)
lambda_q_table <- kbl(tbl[,table_vars], booktab = TRUE, digits = digits,
                     col.names = col.names, format='latex') %>%
  add_header_above(c(' ', 'p=0.05' = 2, 'p=0.25' = 2, 'p=0.50' = 2))
writeLines(lambda_q_table, con=here("paper/tables", "lambda_q_table.tex"))

table_vars <- c('domain', 'wstub', 'mstub', 'tiub', 'wstb3', 'mstb3', 'tib3')
col.names <- c('Domain', 'WST', 'MST', 'TI', 'WST', 'MST', 'TI')
digits = c(0, 2, 2, 2, 2, 2, 3)
bin_table <- kbl(tbl[2:33,table_vars], booktab = TRUE, digits = digits,
                 col.names = col.names, format='latex', escape = FALSE) %>%
  add_header_above(c(' ', 'Model $M_{\\\\mathrm{ind}}$' = 3, 'Model $M_3$' = 3), escape=FALSE)
writeLines(bin_table, con=here("paper/tables", "bin_table.tex"))

count_proportion_table(RanCh::MMS_2019_counts['Colours', , ],
                       here("paper/tables", "colours_table.tex"))
