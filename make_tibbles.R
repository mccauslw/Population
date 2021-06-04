library(tidyverse)
library(mcmcse)

domain_names = c(
 	'Male stars',           # 1
 	'Female stars',
 	'Films',
 	'Star pairs',
	'Pizzas',               # 5
	'Juices',
	'Colours',
	'Colour Pairs',
	'Events',
	'Radio formats',        # 10
	'Music',
	'Aborig. art',
	'Impress. art',
	'Sentences',
	'Travel',               # 15
	'Marijuana',
	'Latitude',
	'Dots',
	'Triangles',
	'Population',           # 20
	'Area',
	'Beer',
	'Cars',
	'Restaurants',
	'Itineraries I',      # 25
	'Payoffs',
	'Phone plans',
	'Hotel rooms',
	'Itineraries II',
	'Televisions',          # 30
	'Coffee',
	'Charity')

domain_names_extend = c('Prior', domain_names)
n_domains = length(domain_names)
n_domains_extend = length(domain_names_extend)

n_models = 3
n_models_extend = 4

axiom_names = c('none', 'wst', 'mst', 'sst', 'ti', 'reg', 'ru', 'mul')
n_axioms = length(axiom_names)

# Array for information that depends on domain, model and axiom
var_names = c('Pr', 'Pr_nse', 'lnBF_a', 'lnBF_a_nse',
			  'lnBF_ma', 'lnBF_ma_nse', 'lnBF_ma_rel', 'lnBF_ma_rel_nse')
n_vars = length(var_names)

dma_array <- array(NA,
	dim=c(n_domains_extend, n_models_extend, n_axioms, n_vars),
	dimnames=list(domain = domain_names_extend,
		          model = 0:n_models,
		          axiom = axiom_names,
		          var_name = var_names))

# Array for information that depends on domain and model
var_names = c('BF', 'BF_nse', 'lnBF', 'lnBF_nse', 'al', 'al_nse', 'la', 'la_nse')
n_vars = length(var_names)
dm_array <- array(NA,
	dim=c(n_domains_extend, n_models_extend, n_vars),
	dimnames=list(domain = domain_names_extend,
		          model = 0:n_models,
		          var_name = var_names))

#res.alla <- tibble(domain=rep(1:n.domains-1, each=n.models),
#	               model=rep(1:n.models-1, n.domains))

# Efficiency computations using overlapping batch mean method for weighted samples
Oefficiency <- function(w, x, B = 200)
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

# Process prior and posterior information for prior robustness runs
for (domain in 0:n_domains) {
	if (domain == 0) {
		t = read.table('~/Results/RCM_population/population_priorsim')
	}
	else {
		# Process posterior information for given domain
		table_name = sprintf('~/Results/RCM_population/population_postsim_%i', domain)
		t = read.table(table_name)
	}
	t[, 'al'] = t[, 'a0'] + t[,'aA']
	t[, 'la'] = t[ ,'a0'] / t[,'al']

	for (model in 0:n_models) {
		if (model==0) {
			w = rep(1, dim(t)[1])
		}
		else {
			w = exp(t[, sprintf('w%i', model)])
		}
		mcresult = mcse(w, method = "obm") # Returns est, se (estimated mean, std error)
		dm_array[domain+1, model+1, "BF"] <- mcresult$est
		dm_array[domain+1, model+1, "BF_nse"] <- mcresult$se
		dm_array[domain+1, model+1, "lnBF"] <- log(mcresult$est)
		dm_array[domain+1, model+1, "lnBF_nse"] <- mcresult$se / mcresult$est
		
		eff = Oefficiency(w, t[, 'al'])
		dm_array[domain+1, model+1, 'al'] = eff$r
		dm_array[domain+1, model+1, 'al_nse'] = sqrt(eff$var_r)

		eff = Oefficiency(w, t[, 'la'])
		dm_array[domain+1, model+1, 'la'] = eff$r
		dm_array[domain+1, model+1, 'la_nse'] = sqrt(eff$var_r)
		
		dma_array[domain+1, model+1, 'none', 'Pr'] = 1
		dma_array[domain+1, model+1, 'none', 'Pr_nse'] = 0
		dma_array[domain+1, model+1, 'none', 'lnBF_a'] = 0
		dma_array[domain+1, model+1, 'none', 'lnBF_a_nse'] = 0
		dma_array[domain+1, model+1, 'none', 'lnBF_ma'] = dm_array[domain+1, model+1, 'lnBF']
		dma_array[domain+1, model+1, 'none', 'lnBF_ma_nse'] = dm_array[domain+1, model+1, 'lnBF_nse']
	
		for (axiom in 2:n_axioms) {
			a_name = axiom_names[axiom]
			eff = Oefficiency(w, t[, a_name])
			dma_array[domain+1, model+1, axiom, 'Pr'] = eff$r
			dma_array[domain+1, model+1, axiom, 'Pr_nse'] = sqrt(eff$var_r)
			if (domain==0) {
				dma_array[1, model+1, axiom, 'lnBF_a_nse'] = sqrt(eff$var_r)/eff$r
			}
			else {
				lnBF_a = log(eff$r) - log(dma_array[1, model+1, axiom, 'Pr'])
				dma_array[domain+1, model+1, axiom, 'lnBF_a'] = lnBF_a
				lnBF_a_nse = sqrt(eff$var_r / eff$r^2 + dma_array[1, model+1, axiom, 'lnBF_a_nse']^2)
				dma_array[domain+1, model+1, axiom, 'lnBF_a_nse'] = lnBF_a_nse
				dma_array[domain+1, model+1, axiom, 'lnBF_ma'] =
					lnBF_a + dm_array[domain+1, model+1, 'lnBF']
				dma_array[domain+1, model+1, axiom, 'lnBF_ma_nse'] =
					sqrt(lnBF_a_nse^2 + dm_array[domain+1, model+1, 'lnBF_nse']^2)
			}
		}
	}
	for (model in 1:n_models) {
		for (axiom in 1:n_axioms) {
			dma_array[domain+1, model+1, axiom, 'lnBF_ma_rel'] =
				dma_array[domain+1, model+1, axiom, 'lnBF_ma'] -
				dma_array[domain+1, n_models_extend, 'none', 'lnBF_ma']
			dma_array[domain+1, model+1, axiom, 'lnBF_ma_rel_nse'] =
				sqrt(dma_array[domain+1, model+1, axiom, 'lnBF_ma_nse']^2 +
					 dma_array[domain+1, n_models_extend, 'none', 'lnBF_ma_nse']^2)
		}
	}

	cat(sprintf('\nRobust, domain %i', domain))
}

dma_table = as_tibble(melt(dma_array)) %>%
    pivot_wider(names_from = var_name, values_from = value)

dm_table = as_tibble(melt(dm_array)) %>%
	pivot_wider(names_from = var_name, values_from = value)

save(dma_array, dma_table, dm_array, dm_table, file='dma_tables.RData')
