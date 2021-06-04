library(xtable)
library(mcmcse)
library(ggtern)

n.domains = 32
n.models = 3
B = 200
axiom.list = c('wst', 'mst', 'sst', 'ti', 'reg', 'ru', 'mul')

# Efficiency computations using overlapping batch mean method for weighted samples
Oefficiency <- function(w, x, B)
{
	wx = w*x
	n = mean(wx)
	d = mean(w)
	r = n/d
	N = length(w)
	const = B/((N-B)*(N-B+1))
	n_j = mean(wx[1:B])
	d_j = mean(w[1:B])
	var_n = (n_j - n)^2
	var_d = (d_j - d)^2
	cov_n_d = (n_j - n) * (d_j - d)
	for (j in seq(1, N-B)) {
		n_j = n_j + (wx[j+B] - wx[j])/B
		d_j = d_j + (w[j+B] - w[j])/B
		var_n = var_n + (n_j - n)^2
		var_d = var_d + (d_j - d)^2
		cov_n_d = cov_n_d + (n_j - n) * (d_j - d)
	}
	var_n = var_n * const
	var_d = var_d * const
	cov_n_d = cov_n_d * const
	var_r = (var_n - 2*r*cov_n_d + r^2*var_d) / (d^2)
	var_x = mean(w*(x-r)^2)/d
	rne = (var_x/N)/var_r
	list(r=r, var_n=var_n, var_d = var_d, cov_n_d = cov_n_d, var_r=var_r, var_x=var_x, rne=rne)
}

# Set up data frame for results
result.table = data.frame(id = 0:n.domains, row.names = 'id')

# Process prior and posterior information for single model run
for (sim_type in c('single', 'uniform')) {
	letter = substr(sim_type, 1, 1)
	for (domain in 0:n.domains) {
		if (domain == 0) {
			# Process prior information (prior is domain 0)
			table.name =
				sprintf('~/Results/RCM_population/population_%s_model_priorsim', sim_type)
			t = read.table(table.name)
		}
		else {
			# Process posterior information for given domain
			table.name = sprintf('~/Results/RCM_population/population_%s_model_postsim_%i',
								sim_type, domain)
			t = read.table(table.name)
		}
		t[, 'al'] = t[, 'a0'] + t[ ,'aA']
		t[, 'la'] = t[ ,'a0'] / t[ ,'al']
		for (vname in c('al', 'la')) {
			ase = mcse(t[, vname], method = "obm")
			result.table[domain+1, sprintf('%s%s', vname, letter)] = ase$est
			result.table[domain+1, sprintf('%s%se', vname, letter)] = ase$se
		}
	
		for (q in c(0.05, 0.25, 0.5, 0.75, 0.95)) {
			for (vname in c('al', 'la')) {
				qse = mcse.q(t[ ,vname], q=q, method = "obm") # Returns est, se (estimated quantile and its standard error)
				est.name = sprintf('%s%s%.2f', vname, letter, q)
				se.name = sprintf('%s%s%.2fe', vname, letter, q)
				result.table[domain+1, est.name] = qse$est
				result.table[domain+1, se.name] = qse$se
			}
		}

		for (axiom in axiom.list) {
			ase = mcse(t[, axiom], method = "obm") # Returns est, se (estimated mean and its standard error)
			pr.name = sprintf('%s%sP', axiom, letter)
			pr.nse.name = sprintf('%s%sPe', axiom, letter)
			lnBF.name = sprintf('%s%sb', axiom, letter)
			lnBF.nse.name = sprintf('%s%sbe', axiom, letter)
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
		cat(sprintf('\n%s model, domain %i', sim_type, domain))
	}
}

# Process prior and posterior information for prior robustness run
for (domain in 0:n.domains) {
	if (domain == 0) {
		t = read.table('~/Results/RCM_population/population_priorsim')
	}
	else {
		# Process posterior information for given domain
		table.name = sprintf('~/Results/RCM_population/population_postsim_%i', domain)
		t = read.table(table.name)
	}
	t[, 'al'] = t[, 'a0'] + t[,'aA']
	t[, 'la'] = t[ ,'a0'] / t[,'al']

	for (model in 1:n.models) {
		w.name = sprintf('w%i', model)
		BF.name = sprintf('B%i', model)
		al.name = sprintf('al%i', model)
		la.name = sprintf('la%i', model)
		BF.nse.name = sprintf('Be%i', model)
		al.nse.name = sprintf('ale%i', model)
		la.nse.name = sprintf('lae%i', model)
		
		w = exp(t[, w.name])
		mcresult = mcse(w, method = "obm") # Returns est, se (estimated mean, std error)
		result.table[domain+1, BF.name] = mcresult$est
		result.table[domain+1, BF.nse.name] = mcresult$se
		
		eff = Oefficiency(w, t[, 'al'], B)
		result.table[domain+1, al.name] = eff$r
		result.table[domain+1, al.nse.name] = sqrt(eff$var_r)

		eff = Oefficiency(w, t[, 'la'], B)
		result.table[domain+1, la.name] = eff$r
		result.table[domain+1, la.nse.name] = sqrt(eff$var_r)
		
		for (axiom in axiom.list) {
			axiom.P.name = sprintf('%sP%i', axiom, model)
			axiom.P.nse.name = sprintf('%sPe%i', axiom, model)
			axiom.BF.name = sprintf('%sb%i', axiom, model)
			axiom.BF.nse.name = sprintf('%sbe%i', axiom, model)
			
			eff = Oefficiency(w, t[, axiom], B)
			result.table[domain+1, axiom.P.name] = eff$r
			result.table[domain+1, axiom.P.nse.name] = sqrt(eff$var_r)
			if (domain==0) {
				result.table[1, axiom.BF.nse.name] = sqrt(eff$var_r)/eff$r
			}
			else{
				result.table[domain+1, axiom.BF.name] =
					log(eff$r) - log(result.table[1, axiom.P.name])
				result.table[domain+1, axiom.BF.nse.name] =
					sqrt(eff$var_r / eff$r^2 + result.table[1, axiom.BF.nse.name]^2)
			}
		}
	}

	cat(sprintf('\nRobust, domain %i', domain))
}

write.table(result.table, '~/Dropbox/_Population/Tables/population.txt')
