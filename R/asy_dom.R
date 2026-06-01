library(tidyverse)
library(knitr)
library(kableExtra)
library(here)
library(RanCh)

latitude_domain <- RanCh::MMS_2019_counts["Latitude", , ]

u <- create_universe(5, letters[1:5])
singletons <- RanCh::u_const$singletons

alpha <- 1
M <- 100000
TCD_triples <- list(c(3, 1, 5),
                    c(3, 2, 5),
                    c(3, 4, 5),
                    c(2, 1, 4),
                    c(2, 3, 4),
                    c(2, 5, 4))
rows <- list()
city_names <- c("Warsaw", "London", "Vancouver", "Paris", "Seattle")

for (i in 1:length(TCD_triples)) {
  TCD <- TCD_triples[[i]]
  T <- TCD[1]
  C <- TCD[2]
  D <- TCD[3]

  A_b <- singletons[T] + singletons[C] # doubleton menu, target and competitor
  A_t <- A_b + singletons[D] # tripleton menu, target, competitor and decoy

  N_b <- latitude_domain[A_b, c(T, C)]
  N_t <- latitude_domain[A_t, c(T, C, D)]

  P_T_b_prior_draw <- rbeta(M, alpha, alpha)
  prior_Pr <- mean(pbeta(P_T_b_prior_draw, alpha, 2*alpha, lower.tail = FALSE))

  P_T_b_post_draw <- rbeta(M, alpha + N_b[1], alpha + N_b[2])
  post_Pr <- mean(pbeta(P_T_b_post_draw, alpha + N_t[[1]], 2*alpha + N_t[[2]] + N_t[[3]], lower.tail = FALSE))

  rows[[i]] <- data.frame(city_names[T], city_names[C], city_names[D],
                          sprintf("$(%d,%d)$", N_b[1], N_b[2]),
                          sprintf("$(%d,%d,%d)$", N_t[1], N_t[2], N_t[3]),
                          sprintf("%.3f", prior_Pr),
                          sprintf("%.3f", post_Pr),
                          sprintf("%.3f", post_Pr/prior_Pr))
  table <- do.call(rbind, rows)
  names(table) <- c("Target", "Competitor", "Decoy",
                    "$(N_T, N_C)$", "$(N_T, N_C, N_D)$",
                    "Prior prob.", "Post. prob.", "Bayes factor")
  asy_dom_table <- knitr::kable(
    table,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
  )
  writeLines(asy_dom_table, con=here("paper/tables", "asy_dom_table.tex"))
}
