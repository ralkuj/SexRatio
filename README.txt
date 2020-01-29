This repository contains code related to the study 

"No genetic contribution to variation in human offspring sex ratio: A total population study of 4.7 million births"
Brendan P. Zietsch
Hasse Walum
Paul Lichtenstein
Karin J.H. Verweij
Ralf Kuja-Halkola

Published in 'Proceedings B' (Proceedings of the Royal Society B Biological Sciences)

Software versions used:
R version 3.6.1 (2019-07-05) -- "Action of the Toes"
Copyright (C) 2019 The R Foundation for Statistical Computing
Platform: x86_64-redhat-linux-gnu (64-bit)

SAS 9.4 TS Level 1M6
X64_10PRO platform


Files and short description:
Offspring_sexratio_Data.sas
- Data management in software SAS. Extract data from database, identify included individuals and their offspring, identify parental generation siblings, export full cohort and relative pair data.

Offspring_sexratio_Data_within_family.sas
- Data management in software SAS. Extract data from database, identify included individuals and their offspring, identify offpsring siblings, export relative pair data.

Offspring_sexratio_Cohort_descriptives.R
- Descritive information extracted, using software R.

Offspring_sexratio_Familial_aggregation_analyses.R
- Analyses in software R. Using parental sibling pairs to estimate familial aggregation of offspring sex.

Offspring_sexratio_Familial_aggregation_analyses_within_families.R
- Analyses in software R. Using offspring sibling pairs to estimate within-parent aggregation of offspring sex.

Offspring_sexratio_Analyses_tetrachoric_correlation.R
- In R. Estimate tetrachoric correlations between full sibling's offsrping's sex. Bootstrap intervals.

Offspring_sexratio_Analyses_h2_pedigreemm.R
- In R, using package pedigreemm, calculate heritability of offspring sex with different link fucnctions.

Offspring_sexratio_Plots.R
- Create plots of sex ratio in R.