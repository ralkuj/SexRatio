##############################################################################################
###
### File name:  Offspring_sexratio_Opening_results_h2_pedigreemm.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    SUmmarize results for heritability of offspring sex.
###
##############################################################################################


###################################
### 1.
### Estimate and CIs for main analysis born 1932-1973
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_20191028.Rdata')
vcs <- as.data.frame( print( VarCorr(fm1) , comp='Variance') )
vcs$vcov
100*vcs$vcov/sum(vcs$vcov)
# CIs
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_CI_20191028.Rdata')
CI1save
100*CI1save[[2]]/sum(vcs$vcov)
100*CI1save[[4]]/sum(vcs$vcov)

###################################
### 2.
### Estimate for born 1932-1973 using probit link
load('~/sexratio_probit_h2_animalmodel_indexBorn1932-1973_20191028.Rdata')
summary(fm2)
vcs <- as.data.frame( print( VarCorr(fm2) , comp='Variance') )
vcs$vcov
100*vcs$vcov/sum(c(vcs$vcov,1))


###################################
### 3.
### Estimate (and CIs) for women
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_WOMEN_20191029.Rdata')
summary(fm1)
vcs <- as.data.frame( print( VarCorr(fm1) , comp='Variance') )
vcs$vcov
100*vcs$vcov/sum(vcs$vcov)
# CI
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_WOMEN_CI_20191029.Rdata')
CI1save
100*CI1save[[2]]/sum(vcs$vcov)


###################################
### 4.
### Estimate (and CIs) for men
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_MEN_20191029.Rdata')
summary(fm1)
vcs <- as.data.frame( print( VarCorr(fm1) , comp='Variance') )
vcs$vcov
100*vcs$vcov/sum(vcs$vcov)
# CI
load('~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_MEN_CI_20191029.Rdata')
CI1save
CI1save[[2]]/sum(vcs$vcov)
100*CI1save[[2]]/sum(vcs$vcov)



###################################
### 5.
### Estimate born 1932-1973 using logit link
load('~/sexratio_logit_h2_animalmodel_indexBorn1932-1973_MEN_20191029.Rdata')
summary(fm1)
vcs <- as.data.frame( print( VarCorr(fm1) , comp='Variance') )
vcs$vcov
100*vcs$vcov/sum(c(vcs$vcov,pi^2/3))



#####################################################################################
#####################################################################################
############################### END OF FILE #########################################
#####################################################################################
#####################################################################################