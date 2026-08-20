library(tidyverse)
library(knitr)
library(kableExtra)
library(here)
library(RanCh)

latitude_domain <- RanCh::MMS_2019_counts["Latitude", , ]

u <- create_universe(5, letters[1:5])
singletons <- RanCh::u_const$singletons

alpha <- 1
M <- 2000000
TCD_triples <- list(c(3, 1, 5),
                    c(3, 2, 5),
                    c(3, 4, 5),
                    c(2, 1, 4),
                    c(2, 3, 4),
                    c(2, 5, 4))
rows <- list()
city_names <- c("Warsaw", "London", "Vancouver", "Paris", "Seattle")

max_std <- 0
set.seed(12345)
for (i in 1:length(TCD_triples)) {
  TCD <- TCD_triples[[i]]
  T <- TCD[1]
  C <- TCD[2]
  D <- TCD[3]

  A_b <- singletons[T] + singletons[C] # doubleton menu, target and competitor
  A_t <- A_b + singletons[D] # tripleton menu, target, competitor and decoy

  N_b <- latitude_domain[A_b, c(T, C)]
  N_t <- latitude_domain[A_t, c(T, C, D)]

  #P_T_b_prior_draw <- rbeta(M, alpha, alpha)
  #F_sample <- pbeta(P_T_b_prior_draw, alpha, 2*alpha, lower.tail = FALSE)
  #prior_Pr <- mean(F_sample)
  #std <- sqrt(var(F_sample)/M)
  #min_std <- min(std, min_std)
  prior_Pr <- 1/3

  P_T_b_post_draw <- rbeta(M, alpha + N_b[1], alpha + N_b[2])
  F_sample <- pbeta(P_T_b_post_draw,
                    alpha + N_t[[1]], 2*alpha + N_t[[2]] + N_t[[3]],
                    lower.tail = FALSE)
  post_Pr <- mean(F_sample)
  std <- sqrt(var(F_sample)/M)
  max_std <- max(std, min_std)

  rows[[i]] <- data.frame(city_names[T], city_names[C], city_names[D],
                          sprintf("$(%d,%d)$", N_b[1], N_b[2]),
                          sprintf("$(%d,%d,%d)$", N_t[1], N_t[2], N_t[3]),
                          sprintf("%.3f", post_Pr),
                          sprintf("%.3f", post_Pr/prior_Pr),
                          sprintf("%.3f", log(post_Pr/prior_Pr)))
}

tbl <- do.call(rbind, rows)
names(tbl) <- c("Target", "Competitor", "Decoy",
                "$(n_T^{(2)}, n_C^{(2)})$",
                "$(n_T^{(3)}, n_C^{(3)}, n_D^{(3)})$",
                "$\\Pr[P \\in \\Lambda|N = n_{\\mathrm{obs}}]$", "BF", "$\\ln \\mathrm{BF}$")
asy_dom_table <- knitr::kable(tbl, format = "latex",
                              booktabs = TRUE, escape = FALSE)

writeLines(asy_dom_table, con=here("paper/tables", "asy_dom_table.tex"))
