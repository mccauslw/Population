library(bitops)
setwd("~/Git_repos/Population/Population_study_data")
t = read.csv("Experimental_Data_Final_n1042.csv")
n.lines = dim(t)[1]

# Names of the 32 choice domains in the experiment
domain.names = c(
	'Male stars',           # 1
	'Female stars',
	'Films',
	'Star pairs',
	'Pizzas',               # 5
	'Juices',
	'Colours',
	'Colour pairs',
	'Events',
	'Radio',                # 10
	'Music',
	'Aboriginal art',
	'Impressionist art',
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
	'Layovers',             # 25
	'Delayed choice',
	'Phone plans',
	'Hotel rooms',
	'Itineraries',
	'Televisions',          # 30
	'Coffee',
	'Charity')

# Vector of names without spaces, for file names
domain.names.nospace = gsub(' ', domain.names, fixed=TRUE, replacement='_')

# Construct the 3-D array all.choice to contain all choice counts.
# all.choice is indexed by (domain, choice set, object),
# where domain is the index of the domain (see order in domain.names),
# choice set is the binary set representation of the subset, with
#   0 the empty set and 32=11111b the master set of all five objects in the domain,
#   2^(i-1) the singleton set with object i, i=1,2,3,4,5
#   (Note: bitwise OR (AND) of representations of two sets gives the representation of the union (intersection, repectively).
# object the index (i=1,2,3,4,5) of the set.
n.domains = length(domain.names)
n.objects = 5
n.subsets = 2^n.objects
all.choice = array(0, c(n.domains, n.subsets, n.objects))
# Each line correponds to a choice trial in the experiment
for (line in 1:n.lines) {
  # The choice set is built up iteratively, starting at the empty set.
  set = 0;
  domain = t[line, 'domain']
  choice.pos = t[line, 'choice']
  for (pos in 1:5) {
    col.name = sprintf("obj%i", pos)
    col.value = t[line, col.name]
    ## If value in spreadsheet column pos isn't missing, it is the index of a choice object in the current choice set. In this case, add object to choice set.
    if (!is.na(col.value)) {
      set = set + bitShiftL(1, col.value-1)
    }
    ## If object in position pos has the same position as the chosen object, record object as the chosen object.
    if (pos==choice.pos) {
      choice = col.value
    }
  }
  ## Add one to the appropriate choice count given the information for this trial
  all.choice[domain, set, choice] = all.choice[domain, set, choice]+1
}
save(all.choice, file='~/Git_repos/Population/Population_study_data/all_choice.RData')
count.totals = rowSums(all.choice, dims=2)

# Generate C code
c.string.list = vector('list', n.domains)
for (domain in 1:n.domains) {
	domain.string.list = vector('list', n.subsets)
	domain.string.list[[1]] = "    {0, 0, 0, 0, 0}"
	for (set in 1:(n.subsets-1)) {
		cnt.string = paste(all.choice[domain, set,], collapse = ", ")
		domain.string.list[[set+1]] = sprintf('    {%s}', cnt.string)
	}
	domain.string = paste(domain.string.list, collapse = ",\n")
	c.string.list[[domain]] = sprintf("{ /* %s */\n%s }", domain.names[domain], domain.string)
}
c.string = paste(c.string.list, collapse = ",\n  ")
other.function = sprintf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s",
    'void set_population_data(Universe *univ, RCM *rcm, int domain)',
    '{',
    '    unsigned i, A;',
    '    for (A=0; A<univ->nSets; A++)',
    '        if (univ->cardinality[A] > 1) {',
    '            rcm->N_total[A] = 0;',
    '            for (i=0; i<univ->cardinality[A]; i++) {',
    '                unsigned x = univ->element[A][i];',
    '                rcm->N[A][x] = population_counts[domain][A][x];',
    '                rcm->N_total[A] += rcm->N[A][x];',
    '            }',
    '        }',
    '}')
file.contents = sprintf("%s\n%s\n\nstatic int population_counts[%i][%i][%i] =\n{ %s };\n\n%s\n",
    '#include "RCM.h"',
    '#include "RCM_population.h"', n.domains, n.subsets, n.objects, c.string, other.function)
cat(file.contents, file="RCM_population.c")

library(klaR)
source('~/Git_repos/Population/simplex.R')
triple.list = list(c(1, 2, 3), c(1, 2, 4), c(1, 2, 5), c(1, 3, 4), c(1, 3, 5),
                   c(1, 4, 5), c(2, 3, 4), c(2, 3, 5), c(2, 4, 5), c(3, 4, 5))
# Create simplex figures for each domain
for (domain in 1:n.domains) {
  # Set up array of simplex diagrams for this domain
  filename = sprintf("Simplexes/%s.pdf", domain.names.nospace[domain])
  pdf(filename, paper='special', width=9, height=4)
  op = par(mfrow=c(2, 5), mar = c(4, 4, 0.5, 0.5))
  for (triple in 1:10) {
    x = triple.list[[triple]][1]; xsing = bitShiftL(1, x-1)
    y = triple.list[[triple]][2]; ysing = bitShiftL(1, y-1)
    z = triple.list[[triple]][3]; zsing = bitShiftL(1, z-1)
    T = xsing + ysing + zsing
    xy = xsing + ysing; yz = ysing + zsing; xz = xsing + zsing
    pxy = all.choice[domain, xy, x] / (all.choice[domain, xy, x] + all.choice[domain, xy, y])
    pyz = all.choice[domain, yz, y] / (all.choice[domain, yz, y] + all.choice[domain, yz, z])
    pxz = all.choice[domain, xz, x] / (all.choice[domain, xz, x] + all.choice[domain, xz, z])
    nT = all.choice[domain, T, x] + all.choice[domain, T, y] + all.choice[domain, T, z]
    PTx = all.choice[domain, T, x] / nT
    PTy = all.choice[domain, T, y] / nT
    label=letters[triple.list[[triple]]]
    plot.axioms(pxy, pyz, pxz, PTx, PTy, label=label, binary.pch=1, ternary.pch=20)
  }
  dev.off()
}
par(op)
