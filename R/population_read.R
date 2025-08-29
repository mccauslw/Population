result.table = read.table('~/Dropbox/_Population/Tables/population_single_model.txt')

alpha.table = result.table[,c('alpha0.05est', 'alpha0.25est', 'alpha0.50est', 'alpha0.75est', 'alpha0.95est')]
lambda.table = result.table[,c('lambda0.05est', 'lambda0.25est', 'lambda0.50est', 'lambda0.75est', 'lambda0.95est')]
pr.table = result.table[,c('wst.pr', 'mst.pr', 'sst.pr', 'ti.pr', 'reg.pr', 'ru.pr', 'mul.pr')]
