# Population

Repository for research materials (data and code, publically available documents) on discrete choice probabilities for population data.
Main paper is titled "A population choice experiment for testing
axioms of stochastic discrete choice", by William McCausland, A. A. J.
Marley and Clintin Davis-Stober.

### Notes on the contents of this repository

#### Main paper, figures, tables

-   `population.[tex,pdf]` are source and pdf for the main paper

-   Folder `Domain_tex/` gives R markdown (.Rmd, 2024) and LaTeX (.tex,
    2021) versions of various domain descriptions

-   Folder `Figures/` contains pdf files for figures of main paper

-   `figures.R` creates `bcp.pdf` figure only

-   `make_figures.R` creates figures `binary_BF.pdf` and
    `multiple_BF.pdf` using dma_tables.RData simulation summary data.

-   Folder `Tables/` contains .tex files for inclusion in main paper

-   `population_read.R` creates tables for alpha, lambda and axiom probabilities for the single model

-   `population_robust.R` creates Tables/population_robust.txt (not there)

-   `population_single_model.R` creates Tables/poplulation_single_model.txt

#### Document with preliminary tables and graphics

-   Files population_tables_preliminary.[Rmd, pdf] are source and pdf
    for this document.

Title of this document is “Tables for my own interest, population experiment”.
Prior specification 0 is an umbrella prior suitable for importance sampling.

Tables are

    - Prior specifications, with a0, b0, aA, bA and moments of alpha and lambda.
    - Figures showing implied densities of al0, alA and alpha and lambda.
    - B1, Be1, B2, Be2, B3, Be3
    - wstuP, wstuPe, similar for mst, sst, ti (post probs (with nse) for “uniform” model)
    - wstsP, wstsPe, etc. Same probabilities for “simple” model
    - regsP, regsPe, similar for ru, mul (post probs, multi-choice axioms, simple model)
    - wstub, wstube, similar for mst, sst, ti (log BFs and errors, uniform model)
    - wstsb, wstsbe, similar for mst, sst, ti (same, simple model)
    - Mean and numerical standard error of alpha for models 1, 2, 3, simple and uniform model
    - Same for lambda
    - Quantiles for alpha, simple model
    - Quantiles for lambda, simple model
    - wstb1, wstb2, wstb3, wstub, wstube, wstsb, wstsbe (BF for various models)
    - similar tables for other axioms
    - wstP1, wstP2, wstP3, wstuP, wstuPe, wstsP, wstsPe (Post probs for various models)
    - similar tables for other axioms

#### Other R Code

- `population.R` takes as input raw simulation files in \~/Results/RCM_population Writes
in \~/Dropbox/\_Population/Tables/population.txt`

- `dma_tables.RData` contains R data with simulation summaries

- `make_tibbles.R` uses \~/Results/RCM_population tables to create tibble dma_array for dma_tables.RData

- `Simplex.R` has functions for barycentric plots

#### Population_study_data folder

- Original data, feedback files, `RCM_population.c`, simplex figures

#### Population_study_design folder

- Screenshots
- Old stuff related to creation of experiment
