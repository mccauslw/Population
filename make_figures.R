library(tidyverse)

load(file='dma_tables.RData')

dma_table = mutate(dma_table, model_ftr = factor(model))
dma_table$lnBF_ma_rel[!is.finite(dma_table$lnBF_ma_rel)] <- -5

dma_table %>%
	filter(axiom %in% c('none', 'wst', 'mst', 'sst'), model > 0, domain != 'Prior') %>%
	ggplot(aes(x=model_ftr, y=lnBF_ma_rel, group=axiom, shape=axiom)) +
		geom_point() + facet_wrap(nrow=4, ncol=8, vars(domain)) +
		labs(x='Encompassing model', y='Log Bayes factor') +
    ylim(-3.5, NA)

ggsave('Figures/binary_BF.pdf')

dma_table %>%
	filter(axiom %in% c('none', 'reg', 'ru', 'mul'), model > 0, domain != 'Prior') %>%
	ggplot(aes(x=model_ftr, y=lnBF_ma_rel, group=axiom, shape=axiom)) +
		geom_point() + facet_wrap(nrow=4, ncol=8, vars(domain)) +
		labs(x='Encompassing model', y='log Bayes factor') +
    ylim(-2.5, NA)

ggsave('Figures/multiple_BF.pdf')