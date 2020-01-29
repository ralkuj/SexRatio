##############################################################################################
###
### File name:  Offspring_sexratio_Familial_aggregation_analyses.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Estimate familial aggregation of offspring sex.
###
##############################################################################################

###############################################
### Create Rdata
#dat <- read.table( '~/Familiality_of_offspring_sex_cousinpairs_20190116.csv' , sep=',' , header=T )
#save.image( file='~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata' )
###############################################

###############################################
### Used libraries
library(drgee)
###############################################



##############################################################################################
##############################################################################################

###############################################
### Maternal half sibling analyses
### Read in data
load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')

### Remove twins in offspring generation (Choice, analyses were also performed without removing these)
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### Restrict data to half sibs only
dat <- dat[ dat$sibtype==1 , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','FODELSEDATUM2','nrbornsamedate','nrbornsamedate2','bnr','bnr2','LOPNRMOR'  ) ]

### GEE Analysis
fitHM <- summary(gee( konbarn ~ konbarn2 , data=dat , link='logit' , clusterid = 'LOPNRMOR' ) )$coef

### Analyses of first-born only
fitHMfirst <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$bnr==1&dat$bnr2==1 , ] ,  link='logit' , clusterid = 'LOPNRMOR' ) )$coef


### Main table of results - In Table 2
cbind(paste(round( exp(fitHM[2,1]),3),' (',round(exp( fitHM[2,1]-1.96*fitHM[2,2] ),3),',',round(exp( fitHM[2,1]+1.96*fitHM[2,2] ),3),'); p-value=', round( fitHM[2,4] , 3 ),'; N=', dim(dat)[1] , sep='') , 
  paste(round( exp(fitHMfirst[2,1]),3),' (',round(exp( fitHMfirst[2,1]-1.96*fitHMfirst[2,2] ),3),',',round(exp( fitHMfirst[2,1]+1.96*fitHMfirst[2,2] ),3),'); p-value=', round( fitHMfirst[2,4] , 3 ),'; N=', sum( dat$bnr==1&dat$bnr2==1 ) , sep='')
)

###############################################

###############################################
### Paternal half sibling analyses
### Read in data
load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')

### Restrict data to half sibs only
dat <- dat[ dat$sibtype==2 , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','FODELSEDATUM2','nrbornsamedate','nrbornsamedate2','bnr','bnr2' , 'LOPNRMOR' ) ]

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### GEE Analysis
fitHP <- summary(gee( konbarn ~ konbarn2 , data=dat , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef

### Analyses of first-born only
fitHPfirst <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$bnr==1&dat$bnr2==1 , ] ,  link='logit' , clusterid = 'LOPNRMOR' ) )$coef

### Main table of results - In Table 2
cbind(paste(round( exp(fitHP[2,1]),3),' (',round(exp( fitHP[2,1]-1.96*fitHP[2,2] ),3),',',round(exp( fitHP[2,1]+1.96*fitHP[2,2] ),3),'); p-value=', round( fitHP[2,4] , 3 ),'; N=', dim(dat)[1] , sep='') , 
  paste(round( exp(fitHPfirst[2,1]),3),' (',round(exp( fitHPfirst[2,1]-1.96*fitHPfirst[2,2] ),3),',',round(exp( fitHPfirst[2,1]+1.96*fitHPfirst[2,2] ),3),'); p-value=', round( fitHPfirst[2,4] , 3 ),'; N=', sum( dat$bnr==1&dat$bnr2==1 ) , sep='')
)

###############################################



###############################################
### Full sibling analyses
### Read in data
load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### Restrict data to full sibs only
dat <- dat[ dat$sibtype==0 , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','FODELSEDATUM2','nrbornsamedate','nrbornsamedate2','bnr','bnr2' , 'LOPNRMOR' ) ]

### GEE Analysis
fitF <- summary(gee( konbarn ~ konbarn2 , data=dat , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef


### Analyses of first-born only
dat <- dat[ order(dat$LOPNRMOR) , ]
fitFfirst <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$bnr==1&dat$bnr2==1 , ] , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef

### Categorized by birth-period
yrsL   <- c(193200,194700,196100)
yrsH   <- c(200000,198500,197300)

outNUMF <- matrix(NA,length(yrsL),length(yrsH) )
outESTF <- matrix(NA,length(yrsL),length(yrsH) )
outSEF  <- matrix(NA,length(yrsL),length(yrsH) )
outZF   <- matrix(NA,length(yrsL),length(yrsH) )
outPF   <- matrix(NA,length(yrsL),length(yrsH) )
for(i in 1:dim(outESTF)[1]){
  for(j in 1:dim(outESTF)[2]){
    outNUMF[i,j] <- sum(dat$FODELSEDATUM>yrsL[i] & dat$FODELSEDATUM<yrsH[j] & dat$FODELSEDATUM2>yrsL[i] & dat$FODELSEDATUM2<yrsH[j])/2
    fittemp <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$FODELSEDATUM>yrsL[i] & dat$FODELSEDATUM<yrsH[j] & dat$FODELSEDATUM2>yrsL[i] & dat$FODELSEDATUM2<yrsH[j]  , ] , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef
    outESTF[i,j] <- fittemp[2,1]
    outSEF[i,j]  <- fittemp[2,2]
    outZF[i,j]   <- fittemp[2,3]
    outPF[i,j]   <- fittemp[2,4]
    print( paste(i,j,outNUMF[i,j]) )
  }
}
rownames(outNUMF) <- rownames(outESTF) <- rownames(outSEF) <- rownames(outZF) <- rownames(outPF) <- paste('From',substr(yrsL,1,4))
colnames(outNUMF) <- colnames(outESTF) <- colnames(outSEF) <- colnames(outZF) <- colnames(outPF) <- paste('To',substr(yrsH,1,4))

nams <- rep(NA,9)
ns <- rep(NA,9)
ors <- rep(NA,9)
ps <- rep(NA,9)
l<-1
for(i in 1:3){for(j in 1:3){
  nams[l] <- paste(rownames(outNUMF)[i],colnames(outNUMF)[j])
  ns[l] <- outNUMF[i,j]
  ors[l] <- paste( round(exp(outESTF[i,j]),3) , ' (' , round(exp(outESTF[i,j]-qnorm(.975)*outSEF[i,j]),3) , ',' , round(exp(outESTF[i,j]+qnorm(.975)*outSEF[i,j]),3) , ')' , sep='' )
  ps[l] <- round( outPF[i,j] , 3 )
  l <- l+1
} }
# For Table - In Table 4
cbind(nams,ns,ors,ps)
  

### Categorized by birth-period, first-born only
yrsL   <- c(193200,194700,196100)
yrsH   <- c(200000,198500,197300)


outNUMF1 <- matrix(NA,length(yrsL),length(yrsH) )
outESTF1 <- matrix(NA,length(yrsL),length(yrsH) )
outSEF1  <- matrix(NA,length(yrsL),length(yrsH) )
outZF1   <- matrix(NA,length(yrsL),length(yrsH) )
outPF1   <- matrix(NA,length(yrsL),length(yrsH) )
for(i in 1:dim(outESTF1)[1]){
  for(j in 1:dim(outESTF1)[2]){
    outNUMF1[i,j] <- sum(dat$FODELSEDATUM>yrsL[i] & dat$FODELSEDATUM<yrsH[j] & dat$FODELSEDATUM2>yrsL[i] & dat$FODELSEDATUM2<yrsH[j] & dat$bnr==1 & dat$bnr2==1 )
    fittemp <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$FODELSEDATUM>yrsL[i] & dat$FODELSEDATUM<yrsH[j] & dat$FODELSEDATUM2>yrsL[i] & dat$FODELSEDATUM2<yrsH[j] & dat$bnr==1 & dat$bnr2==1  , ] , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef
    outESTF1[i,j] <- fittemp[2,1]
    outSEF1[i,j]  <- fittemp[2,2]
    outZF1[i,j]   <- fittemp[2,3]
    outPF1[i,j]   <- fittemp[2,4]
    print( paste(i,j,'Nrs',outNUMF1[i,j]) )
  }
}
rownames(outNUMF1) <- rownames(outESTF1) <- rownames(outSEF1) <- rownames(outZF1) <- rownames(outPF1) <- paste('From',substr(yrsL,1,4))
colnames(outNUMF1) <- colnames(outESTF1) <- colnames(outSEF1) <- colnames(outZF1) <- colnames(outPF1) <- paste('To',substr(yrsH,1,4))


nams1 <- rep(NA,9)
ns1 <- rep(NA,9)
ors1 <- rep(NA,9)
ps1 <- rep(NA,9)
l<-1
for(i in 1:3){for(j in 1:3){
  nams1[l] <- paste(rownames(outNUMF1)[i],colnames(outNUMF1)[j])
  ns1[l] <- outNUMF1[i,j]
  ors1[l] <- paste( round(exp(outESTF1[i,j]),3) , ' (' , round(exp(outESTF1[i,j]-qnorm(.975)*outSEF1[i,j]),3) , ',' , round(exp(outESTF1[i,j]+qnorm(.975)*outSEF1[i,j]),3) , ')' , sep='' )
  ps1[l] <- round( outPF1[i,j] , 3 )
  l <- l+1
} }
# For Table  - In Table 4
cbind(nams1,ns1,ors1,ps1)

### Main table of results - In Table 2
cbind(paste(round( exp(fitF[2,1]),3),' (',round(exp( fitF[2,1]-1.96*fitF[2,2] ),3),',',round(exp( fitF[2,1]+1.96*fitF[2,2] ),3),'); p-value=', round( fitF[2,4] , 3 ),'; N=', dim(dat)[1] , sep='') , 
  paste(round( exp(fitFfirst[2,1]),3),' (',round(exp( fitFfirst[2,1]-1.96*fitF[2,2] ),3),',',round(exp( fitFfirst[2,1]+1.96*fitFfirst[2,2] ),3),'); p-value=', round( fitFfirst[2,4] , 3 ),'; N=', sum( dat$bnr==1&dat$bnr2==1 ) , sep='')
)

###############################################

##############################################################################################
##############################################################################################









##############################################################################################
##############################################################################################
### Analyses females

###############################################
### Full sibling analyses
### Read in data
load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]
### Only females, and sisters
dat <- dat[ dat$KON==1 & dat$KON2==1 , ]

### Restrict data to full sibs only
dat <- dat[ dat$sibtype==0 , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','FODELSEDATUM2','nrbornsamedate','nrbornsamedate2','bnr','bnr2' , 'LOPNRMOR' ) ]

### Analysis
fitFfemale <- summary(gee( konbarn ~ konbarn2 , data=dat  , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef

### Analyses of first-born only
fitFfirstfemale <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$bnr==1&dat$bnr2==1 , ]  , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef

### Main table of results - In Table 2
cbind(paste(round( exp(fitFfemale[2,1]),3),' (',round(exp( fitFfemale[2,1]-1.96*fitFfemale[2,2] ),3),',',round(exp( fitFfemale[2,1]+1.96*fitFfemale[2,2] ),3),'); p-value=', round( fitFfemale[2,4] , 3 ),'; N=', dim(dat)[1] , sep='') , 
  paste(round( exp(fitFfirstfemale[2,1]),3),' (',round(exp( fitFfirstfemale[2,1]-1.96*fitFfirstfemale[2,2] ),3),',',round(exp( fitFfirstfemale[2,1]+1.96*fitFfirstfemale[2,2] ),3),'); p-value=', round( fitFfirstfemale[2,4] , 3 ),'; N=', sum( dat$bnr==1&dat$bnr2==1 ) , sep='')
)
###############################################

##############################################################################################
##############################################################################################







##############################################################################################
##############################################################################################
### Analyses males

###############################################
### Full sibling analyses
### Read in data
load(' /Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]
### Only males, and brothers
dat <- dat[ dat$KON==0 & dat$KON2==0 , ]

### Restrict data to full sibs only
dat <- dat[ dat$sibtype==0 , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','FODELSEDATUM2','nrbornsamedate','nrbornsamedate2','bnr','bnr2' , 'LOPNRMOR' ) ]

### Analysis
fitFmale <- summary(gee( konbarn ~ konbarn2 , data=dat  , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef

### Analyses of first-born only
fitFfirstmale <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$bnr==1&dat$bnr2==1 , ]  , link = 'logit' , clusterid = 'LOPNRMOR' ) )$coef


### Main table of results - In Table 2
cbind(paste(round( exp(fitFmale[2,1]),3),' (',round(exp( fitFmale[2,1]-1.96*fitFmale[2,2] ),3),',',round(exp( fitFmale[2,1]+1.96*fitFmale[2,2] ),3),'); p-value=', round( fitFmale[2,4] , 3 ),'; N=', dim(dat)[1] , sep='') , 
  paste(round( exp(fitFfirstmale[2,1]),3),' (',round(exp( fitFfirstmale[2,1]-1.96*fitFfirstmale[2,2] ),3),',',round(exp( fitFfirstmale[2,1]+1.96*fitFfirstmale[2,2] ),3),'); p-value=', round( fitFfirstmale[2,4] , 3 ),'; N=', sum( dat$bnr==1&dat$bnr2==1 ) , sep='')
)
###############################################



##############################################################################################
##############################################################################################
################################# END OF FILE ################################################
##############################################################################################
##############################################################################################