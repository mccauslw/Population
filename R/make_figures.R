library(here)
library(tidyverse)

load(file=here("data", "dma_tables.RData"))
dma_table = mutate(dma_table, model_ftr = factor(model))
dma_table$lnBF_ma_rel[!is.finite(dma_table$lnBF_ma_rel)] <- -5

# Figures for Bayes Factors related to binary choice axioms
dma_table %>%
	filter(axiom %in% c('none', 'wst', 'mst', 'sst'), model > 0, domain != 'Prior') %>%
	ggplot(aes(x=model_ftr, y=lnBF_ma_rel, group=axiom, shape=axiom)) +
		geom_point() + facet_wrap(nrow=4, ncol=8, vars(domain)) +
		labs(x='Encompassing model', y='Log Bayes factor') +
    ylim(-3.5, NA)
ggsave(here("paper/figures", "binary_BF.pdf"))

# Figures for Bayes Factors related to multiple choice axioms
dma_table %>%
	filter(axiom %in% c('none', 'reg', 'ru', 'mul'), model > 0, domain != 'Prior') %>%
	ggplot(aes(x=model_ftr, y=lnBF_ma_rel, group=axiom, shape=axiom)) +
		geom_point() + facet_wrap(nrow=4, ncol=8, vars(domain)) +
		labs(x='Encompassing model', y='log Bayes factor') +
    ylim(-2.5, NA)
ggsave(here("paper/figures", "multiple_BF.pdf"))

# Figure with prior densities of binary choice probabilitiies associated with
# different values of alpha
pdf(here("paper/figures", "bcp.pdf"), paper='special', width=12, height=6)
pr = seq(0.0, 1.0, by=0.0001)
alpha = c(0.5, 1, 2, 5, 10, 20)
lty_vals = c(1, 5, 2, 6, 4, 3)
plot(NULL, xlim=c(0,1), ylim=c(0,5), ylab="Density value", xlab="Binary choice probability")
for (i in 1:6)
  lines(pr, dbeta(pr, alpha[i]/2, alpha[i]/2), lty = lty_vals[i])
legend(x='topleft', legend = alpha, lty=lty_vals, title='Values of alpha')
dev.off()

# Figure with simplex plots for colours domain
pdf(here("paper/figures", "colours_figure.pdf"), paper='special', width=12.5, height=5)
P <- P_frequencies(RanCh::MMS_2019_counts['Colours', , ])
op = graphics::par(mfrow=c(2, 5), mar = c(0, 0, 0.0, 0.0))
n <- 5
tripletons <- u_const$tripletons[1:u_const$n_tripletons[n]]
triple_v_list <- u_const$subset_vectors[tripletons]
triple_v_list_sorted <- triple_v_list[order(sapply(triple_v_list,
                                                   paste, collapse=""))]
for (i_triple in 1:u_const$n_tripletons[5]) {
  P3 <- marginalize(P, triple_v_list_sorted[[i_triple]])
  bin_tern_MR_plot(P3)
}
graphics::par(op)
dev.off()
