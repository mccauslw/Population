library(xtable)
library(mcmcse)
library(ggtern)

B = 200
n.domains = 32
n.models = 4

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
result.table = data.frame(id = 1:n.domains, row.names = 'id')

axiom.list = c('wst', 'mst', 'sst', 'ti', 'reg', 'ru', 'mul')

# Process prior information (prior is domain 0)
t = read.table('~/Results/RCM_population/population_priorsim')
log.pr.prob = matrix(0, nrow=length(axiom.list), ncol=n.models, dimnames=list(axiom.list))
log.pr.prob.nvar = matrix(0, nrow=length(axiom.list), ncol=n.models, dimnames=list(axiom.list))
for (model in 1:n.models) {
	w.name = sprintf('w%i', model)
	w = exp(t[,w.name])
	for (axiom in axiom.list) {
		eff = Oefficiency(w, t[, axiom], B)
		log.pr.prob[axiom, model] = log(eff$r)
		log.pr.prob.nvar[axiom, model] = eff$var_r / eff$r^2		
	}
}

# Process posterior information
for (domain in 1:n.domains) {
	# Process posterior information for given domain
	table.name = sprintf('~/Results/RCM_population/population_postsim_%i', domain)
	t = read.table(table.name)
	
	t[, 'alpha'] = t[, 'a0'] + t[,'aA']
	t[, 'lambda'] = t[ ,'a0'] / t[,'alpha']

	for (model in 1:n.models) {
		w.name = sprintf('w%i', model)
		BF.name = sprintf('BF%i', model)
		al.name = sprintf('al%i', model)
		la.name = sprintf('la%i', model)
		BF.nse.name = sprintf('BFnse%i', model)
		al.nse.name = sprintf('alnse%i', model)
		la.nse.name = sprintf('lanse%i', model)
		
		w = exp(t[, w.name])
		mcresult = mcse(w, method = "obm") # Returns est, se (estimated mean, std error)
		result.table[domain, BF.name] = mcresult$est
		result.table[domain, BF.nse.name] = mcresult$se
		
		eff = Oefficiency(w, t[, 'alpha'], B)
		result.table[domain, al.name] = eff$r
		result.table[domain, al.nse.name] = sqrt(eff$var_r)

		eff = Oefficiency(w, t[, 'lambda'], B)
		result.table[domain, la.name] = eff$r
		result.table[domain, la.nse.name] = sqrt(eff$var_r)
		
		for (axiom in axiom.list) {
			axiom.BF.name = sprintf('%slnBF%i', axiom, model)
			axiom.BF.nse.name = sprintf('%slnBFnse%i', axiom, model)
			
			eff = Oefficiency(w, t[, axiom], B)
			result.table[domain, axiom.BF.name] = log(eff$r) - log.pr.prob[axiom, model]
			result.table[domain, axiom.BF.nse.name] = sqrt(eff$var_r / eff$r^2 + log.pr.prob.nvar[axiom, model])
		}
	}
	
	cat(sprintf('\nDomain %i', domain))
}
write.table(result.table, 'Tables/population_robust.txt')
