# Population

Repository for research on discrete choice probabilities for population
data. Main paper is titled "A population choice experiment for testing
axioms of stochastic discrete choice", by William McCausland, A. A. J.
Marley and Clintin Davis-Stober.

### Notes on the contents of this repository

#### Main paper, figures, tables

-   Files `population.[tex,pdf]` are source and pdf for the main paper

-   Folder `Domain_tex/` gives R markdown (.Rmd, 2024) and LaTeX (.tex,
    2021) versions of various domain descriptions

-   Folder `Figures/` contains pdf files for figures of main paper

-   File `figures.R` creates `bcp.pdf` figure only

-   File `make_figures.R` creates figures `binary_BF.pdf` and
    `multiple_BF.pdf` using dma_tables.RData simulation summary data.

-   File `dma_tables.RData` contains R data with simulation summaries

#### Document with preliminary tables and graphics

-   Files population_tables_preliminary.[Rmd, pdf] are source and pdf
    for this document.

Title of this document is “Tables for my own interest, population experiment”.
First table gives prior specifications, with a0, b0, aA, bA and moments of alpha and lambda.
Prior specification 0 is an umbrella prior suitable for importance sampling.
There are graphs of implied densities of al0, alA and alpha and lambda.
After this, there are a lot of tables of posterior moments.

    - B1, Be1, B2, Be2, B3, Be3 wstuP, wstuPe, similar for mst, sst, ti (these are prior and posterior probabilities (with numerical standard errors) of axioms for “uniform” model)
    - wstsP, wstsPe, otherwise the same (these are prior and posterior probabilities of axioms for “simple” model)
    - regsP, regsPe, similar for ru, mul (prior and post probs, multi-choice axioms, simple model)
    - wstub, wstube, similar for mst, sst, ti (log bayes factors and errors, uniform model)
    - wstsb, wstsbe, similar for mst, sst, ti (same, simple model)
    - Mean and numerical standard error of alpha for models 1, 2, 3, simple and uniform model
    - Same for lambda
    - Quantiles for alpha, simple model
    - Quantiles for lambda, simple model
    - wstb1, wstb2, wstb3, wstub, wstube, wstsb, wstsbe (Bayes factors
for various models (for other axioms)
    - wstP1, wstP2, wstP3, wstuP, wstuPe, wstsP, wstsPe (Post probabilities for various models) (for other axioms)

#### R Code

- `make_tibbles.R` uses \~/Results/RCM_population tables to create tibble dma_array for dma_tables.RData

- `population_read.R` creates tables for alpha, lambda and axiom probabilities for the single model

- `population_robust.R` creates Tables/population_robust.txt (not there)

- `population_single_model.R` creates Tables/poplulation_single_model.txt

- `population.R` takes as input raw simulation files in \~/Results/RCM_population Writes
in \~/Dropbox/\_Population/Tables/population.txt`

- `Simplex.R` has functions for barycentric plots

#### Population_study_data folder

- Original data, feedback files, `RCM_population.c`, simplex figures

#### Population_study_design folder

Has old stuff related to creation of experiment, screenshots
