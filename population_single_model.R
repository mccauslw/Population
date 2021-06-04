library(xtable)
library(mcmcse)
library(ggtern)

n.domains = 32

# Set up data frame for results
result.table = data.frame(id = 0:n.domains, row.names = 'id')

# Process posterior information
for (domain in 0:n.domains) {
	if (domain == 0) {
		# Process prior information (prior is domain 0)
		t = read.table('~/Results/RCM_population/population_single_model_priorsim')
	}
	else {
		# Process posterior information for given domain
		table.name = sprintf('~/Results/RCM_population/population_single_model_postsim_%i', domain)
		t = read.table(table.name)
	}
	t[, 'alpha'] = t[, 'a0'] + t[ ,'aA']
	t[, 'lambda'] = t[ ,'a0'] / t[ ,'alpha']
	for (vname in c('alpha', 'lambda')) {
		ase = mcse(t[, vname], method = "obm")
		result.table[domain+1, sprintf('%s.mean', vname)] = ase$est
		result.table[domain+1, sprintf('%s.mean.nse', vname)] = ase$se
	}
	
	for (q in c(0.05, 0.25, 0.5, 0.75, 0.95)) {
		for (vname in c('alpha', 'lambda')) {
			qse = mcse.q(t[ ,vname], q=q, method = "obm") # Returns est, se (estimated quantile and its standard error)
			est.name = sprintf('%s%.2fest', vname, q)
			se.name = sprintf('%s%.2fse', vname, q)
			result.table[domain+1, est.name] = qse$est
			result.table[domain+1, se.name] = qse$se
		}
	}

	for (axiom in c('wst', 'mst', 'sst', 'ti', 'reg', 'ru', 'mul')) {
		ase = mcse(t[, axiom], method = "obm") # Returns est, se (estimated mean and its standard error)
		pr.name = sprintf('%s.pr', axiom)
		pr.nse.name = sprintf('%s.pr.nse', axiom)
		lnBF.name = sprintf('%s.lnBF', axiom)
		lnBF.nse.name = sprintf('%s.lnBF.nse', axiom)
		result.table[domain+1, pr.name] = ase$est
		result.table[domain+1, pr.nse.name] = ase$se
		result.table[domain+1, lnBF.name] = log(ase$est) - log(result.table[1, pr.name])
		if (domain == 0) {
			result.table[1, lnBF.nse.name] = ase$se/ase$est
		}
		else{
			result.table[domain+1, lnBF.nse.name] = sqrt((ase$se/ase$est)^2 + result.table[1, lnBF.nse.name]^2)
		}
	}

	cat(sprintf('\nDomain %i', domain))
}
write.table(result.table, '~/Dropbox/_Population/Tables/population_single_model.txt')
