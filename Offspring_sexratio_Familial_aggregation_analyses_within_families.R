##############################################################################################
###
### File name:  Offspring_sexratio_Familial_aggregation_analyses_within_families.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Estimate familial aggregation of offspring sex within parents (offspring are siblings).
###
##############################################################################################

###############################################
### Create Rdata
#dat <- read.table( '~/Familiality_of_offspring_sex_siblingpairs_within_family_20190116.csv' , sep=',' , header=T )
#save.image( file='~/Familiality_of_offspring_sex_siblingpairs_within_family_20190116.Rdata' )
###############################################

###############################################
### Used libraries
library(drgee)
###############################################


###############################################
### Conditioning on final family size

### Read in data
load('~/Familiality_of_offspring_sex_siblingpairs_within_family_20190116.Rdata')

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### Don't restrict data
dat <- dat[ , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','dobbarn','dobbarn2','nrbornsamedate','nrbornsamedate2','bnr','bnr2','maxbnr','maxbnr2','LOPNR'  ) ]

### GEE analysis (not used, combines all)
# summary(gee( konbarn ~ konbarn2 , data=dat , link='logit' , clusterid = 'LOPNR' ) )$coef

### Categorized by family size
outNUMF <- rep(NA,9)
outESTF <- rep(NA,9)
outSEF  <- rep(NA,9)
outZF   <- rep(NA,9)
outPF   <- rep(NA,9)
i<-2
for(i in 2:10){
  if(i<=10){outNUMF[i-1] <- dim(dat[dat$maxbnr==i,])[1]}
  #if(i==10){outNUMF[i-1] <- dim(dat[dat$maxbnr>=i,])[1]}
  fittemp <- summary( gee( konbarn ~ konbarn2 , data=dat[ dat$maxbnr==i,] , link='logit' , clusterid = 'LOPNR' ) )$coef
  outESTF[i-1] <- fittemp[2,1]
  outSEF[i-1]  <- fittemp[2,2]
  outZF[i-1]   <- fittemp[2,3]
  outPF[i-1]   <- fittemp[2,4]
  print( paste(i,outNUMF[i-1]) )
}


### For table:  - In Table 3
cbind( 2:10 , outNUMF/2 , paste(round(exp(outESTF),3),' (',round(exp(outESTF-qnorm(.975)*outSEF),3),',',round(exp(outESTF+qnorm(.975)*outSEF),3),')',sep='') , round(outPF,3) )

###############################################





##############################################################################################
### Conditioning on attained family size rather than final

### Read in data
load('~/Familiality_of_offspring_sex_siblingpairs_within_family_20190116.Rdata')

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### No restriction of data 
dat <- dat[ , c('konbarn','konbarn2','KON','KON2','FODELSEDATUM','dobbarn','dobbarn2','nrbornsamedate','nrbornsamedate2','bnr','bnr2','maxbnr','maxbnr2','LOPNR'  ) ]

### GEE-analysis
fitF <- summary(gee( konbarn ~ konbarn2 , data=dat[dat$bnr==2&dat$bnr2==1,] , link='logit' , clusterid = 'LOPNR' ) )$coef

out <- c(dim(dat[dat$bnr==2&dat$bnr2==1,])[1],paste(round( exp(fitF[2,1]),3),' (',round(exp( fitF[2,1]-1.96*fitF[2,2] ),3),',',round(exp( fitF[2,1]+1.96*fitF[2,2] ),3),')',sep=''), round( fitF[2,4] , 3 ) )
for(i in 3:10){
  fitF <- summary(gee( konbarn ~ konbarn2 , data=dat[dat$bnr==i&dat$bnr2<i,] , link='logit' , clusterid = 'LOPNR' ) )$coef
  out <- rbind( out , c(dim(dat[dat$bnr==i&dat$bnr2<i,])[1],paste(round( exp(fitF[2,1]),3),' (',round(exp( fitF[2,1]-1.96*fitF[2,2] ),3),',',round(exp( fitF[2,1]+1.96*fitF[2,2] ),3),')',sep=''), round( fitF[2,4] , 3 ) ) )
  print(i)
}
cbind(2:10,out)


##############################################################################################




##############################################################################################
##############################################################################################
################################# END OF FILE ################################################
##############################################################################################
##############################################################################################