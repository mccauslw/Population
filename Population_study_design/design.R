library(combinat)
library(Matrix)

# Dimension parameters of design (change values according to experimental design)
n.domains = 32    # Number of choice domains
n.objects = 5     # Number of objects in each master set
n.mult = 40       # Number of participants per choice subset

# Derived dimension parameter (don't change these values)
n.subsets = 2^n.objects-n.objects-1     # Number of non-degenerate choice subsets
n.participants = n.subsets * n.mult     # Total number of participants
# Thus there are n.groups * n.subsets participants in total

# Construct subsets for master set of size n.objects
# To get a vector of logicals to screen for subset i, use subset.structure[[i]]
# Only doubleton and bigger subsets appear in list subset.structure
# Order is not random
subset.structure = vector('list', 2^n.objects-n.objects-1)
lindex = 1
for (subset in 3:(2^n.objects-1)) {            # Exlcude empty set and 2 singletons
  if (subset!=4 & subset!=8 & subset!=16) { # Exclude other singletons
    subset.structure[[lindex]] = vector('logical', n.objects)
    for (object in 1:n.objects)
      subset.structure[[lindex]][[object]] = (bitwAnd(2^(object-1), subset) > 0)
    lindex = lindex + 1
  }
}

participant.domain.order = vector('list', n.participants)
for (participant in 1:n.participants) {
  participant.domain.order[[participant]] = sample(n.domains)
}

design.table = data.frame(participant = sort(rep.int(1:n.participants, n.domains)))
design.table$domain = unlist(participant.domain.order)
design.table$Obj1 = rep('-', n.participants * n.domains)
design.table$Obj2 = rep('-', n.participants * n.domains)
design.table$Obj3 = rep('-', n.participants * n.domains)
design.table$Obj4 = rep('-', n.participants * n.domains)
design.table$Obj5 = rep('-', n.participants * n.domains)
design.table$subset = rep('-', n.participants * n.domains)

objects = letters[1:n.objects]
for (t in 1:n.domains) {
  participant.permutation = sample(1:n.participants)
  for (i in 1:n.subsets) {
    subset.screen = subset.structure[[i]]    # e.g. (TRUE, TRUE, FALSE, FALSE, FALSE)
    subset.as.list = objects[subset.screen]  # e.g. ("a", "b")
    subset.size = length(subset.as.list)
    subset.permutations = permn(subset.as.list)[sample(factorial(subset.size))]
    n.mult.permutations = rep_len(subset.permutations, n.mult)
    for (n in 1:n.mult) {
      participant = participant.permutation[n + (i-1) * n.mult]
      inverse.participant.domain.order = invPerm(participant.domain.order[[participant]])
      trial = inverse.participant.domain.order[t]
      n.objects.in.trial = length(n.mult.permutations[[n]])
      design.table[trial + n.domains * (participant-1), 3:(3+subset.size-1)] = n.mult.permutations[[n]]
      design.table$subset[trial + n.domains * (participant-1)] = paste(subset.as.list, collapse='')
    }
  }
}

# Check 1: the following table should be n.domains x n.subsets with all elements
# equal to n.mult
check.table.1 = table(design.table$domain, design.table$subset)
# Check 2: the following table should be n.participants x n.domains, with all elements equal to 1
check.table.2 = table(design.table$participant, design.table$domain)

# Write out spreadsheet
write.csv(design.table, '~/Dropbox/_RCM/Experiment/design_table.csv')