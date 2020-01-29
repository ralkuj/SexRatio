##############################################################################################
###
### File name:  Offspring_sexratio_Analyses_h2_pedigreemm.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Estimate heritability of offspring sex.
###
##############################################################################################

### Needed library
library(pedigreemm)



#########################################################################################
###
### 1: All data, index generation born before 1973. Identity link and profile likelihood interval
### 2: As above, but probit link (no interval)
###
###

### To run on all data (excluding twin births)
# Individuals born latest 1973 (to have finished their reproductive career)

### Data
dat <- read.table( '~/Familiality_of_offspring_sex_cohort_20190116.csv',header=T,sep=',')

# Exclude twin births parents!
temp <- dat[ dat$nrbornsamedate!=1 , ]
dat <- dat[ !(dat$LOPNR%in%temp$LOPNR) & !(dat$LOPNRFAR%in%temp$LOPNRFAR) & !(dat$LOPNRMOR%in%temp$LOPNRMOR) , ]

### Shorter time-span for index generation!
# Individuals born latest 1973 (to have finished their reproductive career)
tempid <- dat[ dat$FODELSEDATUM<197300 ,]$LOPNR
dat <- dat[ dat$LOPNR%in%tempid , ]

### Set up data for pedigree (founders need to be ordered first)
# Unique parents
uMOR <- unique(dat$LOPNRMOR)
uFAR <- unique(dat$LOPNRFAR)

# Identify listed as mother/father as well as index?
xLOPNR <- dat[ dat$bnr==1 , ]$LOPNR[ dat[ dat$bnr==1 , ]$LOPNR %in% c(uFAR,uMOR) ]

# Make parents vector with those without any parents
uParents <- c(uFAR,uMOR)
uParents <- uParents[ !( uParents %in% xLOPNR ) ]

### Checks
# Any parent-only id in offspring-only?
sum( uParents %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR )
# Any parent-only id in offspring-who-are-parents?
sum( uParents %in% xLOPNR )
# Any offspring-who-are-parents id in offspring-only?
sum( xLOPNR %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR  )
# Any offspring-who-are-parents id in parent-only
sum( xLOPNR %in% uParents )
# Any offspring-who-are-parents who ?
sum(xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR))

# Order these first
xLOPNR2 <- xLOPNR[ xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR)]
xLOPNR3 <- xLOPNR[ !(xLOPNR%in%xLOPNR2) ]

### Check that this suffices
sum(xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR))
chng <- xLOPNR2[xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR)]
length( chng )
# Fix so they appear first with no parents
xLOPNR2v2 <- xLOPNR2[!(xLOPNR2%in%chng)]

# Set up pedigree for analysis
p1 <- pedigree( 
  sire=c(rep(NA,length(uParents)+length(chng)) , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRFAR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRFAR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRFAR ) ,
  dam= c(rep(NA,length(uParents)+length(chng)) ,dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRMOR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRMOR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRMOR ) , 
  label= c(uParents,chng,xLOPNR2v2,xLOPNR3,dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR) , ]$LOPNR) )


# Run linear model
fm1 <- pedigreemm( konbarn ~ 1+(1|LOPNR) , data=dat , pedigree = list(LOPNR=p1) )
summary(fm1)
#save( fm1 , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_20191028.Rdata' )

## Get profile likelihood intervals
CI1 <- profile( fm1 , which='.sig01' , prof.scale="varcov" )
CI1save <- lapply( c(.90,.95,.975,.99,.999,.9998) , FUN=function(x){ confint( CI1 ,level=x) } )
CI1save
#save( CI1save , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_CI_20191028.Rdata' )

# Run liability-model
fm2 <- pedigreemm( konbarn ~ 1+(1|LOPNR) , data=dat , pedigree = list(LOPNR=p1) , family=binomial(link='probit'))
summary(fm2)
# Profile likelihood interval is broken. Thus no intervals are calculated.
#save( fm2 , file='~/sexratio_probit_h2_animalmodel_indexBorn1932-1973_20191028.Rdata' )





#########################################################################################
###
### 3: Females index generation born before 1973. Identity link (and profile likelihood interval)
###
###

### Data
dat <- read.table( '~/Familiality_of_offspring_sex_cohort_20190116.csv',header=T,sep=',')

# Exclude twin births parents!
temp <- dat[ dat$nrbornsamedate!=1 , ]
dat <- dat[ !(dat$LOPNR%in%temp$LOPNR) & !(dat$LOPNRFAR%in%temp$LOPNRFAR) & !(dat$LOPNRMOR%in%temp$LOPNRMOR) , ]

### Only mums.
table(dat$KON)
dat <- dat[ dat$KON==1 , ]

# INdividuals born latest 1973 (to have finished their reproductive career)
tempid <- dat[ dat$FODELSEDATUM<197300 ,]$LOPNR
dat <- dat[ dat$LOPNR%in%tempid , ]

### Set up data for pedigree (founders need to be ordered first)
# Unique parents
uMOR <- unique(dat$LOPNRMOR)
uFAR <- unique(dat$LOPNRFAR)

# Identify listed as mother/father as well as index?
xLOPNR <- dat[ dat$bnr==1 , ]$LOPNR[ dat[ dat$bnr==1 , ]$LOPNR %in% c(uFAR,uMOR) ]

# Make parents vector with those without any parents
uParents <- c(uFAR,uMOR)
uParents <- uParents[ !( uParents %in% xLOPNR ) ]

### Checks
# Any parent-only id in offspring-only?
sum( uParents %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR )
# Any parent-only id in offspring-who-are-parents?
sum( uParents %in% xLOPNR )
# Any offspring-who-are-parents id in offspring-only?
sum( xLOPNR %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR  )
# Any offspring-who-are-parents id in parent-only
sum( xLOPNR %in% uParents )
# Any offspring-who-are-parents who ?
sum(xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR))

# Order these first
xLOPNR2 <- xLOPNR[ xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR)]
xLOPNR3 <- xLOPNR[ !(xLOPNR%in%xLOPNR2) ]

### Check that this suffices
# REMOVE THESE!!! - Doesn't work, how to solve?
sum(xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR))
chng <- xLOPNR2[xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR)]
length( chng )
# Fix so they appear first with no parents
xLOPNR2v2 <- xLOPNR2[!(xLOPNR2%in%chng)]

# Set up pedigree for analysis
p1 <- pedigree( 
  sire=c(rep(NA,length(uParents)+length(chng)) , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRFAR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRFAR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRFAR ) ,
  dam= c(rep(NA,length(uParents)+length(chng)) ,dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRMOR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRMOR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRMOR ) , 
  label= c(uParents,chng,xLOPNR2v2,xLOPNR3,dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR) , ]$LOPNR) )


# Run linear model
fm1 <- pedigreemm( konbarn ~ 1+(1|LOPNR) , data=dat , pedigree = list(LOPNR=p1) )
summary(fm1)
#save( fm1 , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_WOMEN_20191029.Rdata' )

## Get profile likelihood intervals
CI1 <- profile( fm1 , which='.sig01' , prof.scale="varcov" )
CI1save <- lapply( c(.90,.95,.975,.99,.999,.9998) , FUN=function(x){ confint( CI1 ,level=x) } )
#save( CI1save , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_WOMEN_CI_20191029.Rdata' )




#########################################################################################
###
### 4: Male index generation born before 1973. Identity link (and profile likelihood interval)
###
###

### Data
dat <- read.table( '~/Familiality_of_offspring_sex_cohort_20190116.csv',header=T,sep=',')

# Exclude twin births parents!
temp <- dat[ dat$nrbornsamedate!=1 , ]
dat <- dat[ !(dat$LOPNR%in%temp$LOPNR) & !(dat$LOPNRFAR%in%temp$LOPNRFAR) & !(dat$LOPNRMOR%in%temp$LOPNRMOR) , ]

### Only mums.
table(dat$KON)
dat <- dat[ dat$KON==0 , ]

# INdividuals born latest 1973 (to have finished their reproductive career)
tempid <- dat[ dat$FODELSEDATUM<197300 ,]$LOPNR
dat <- dat[ dat$LOPNR%in%tempid , ]

### Set up data for pedigree (founders need to be ordered first)
# Unique parents
uMOR <- unique(dat$LOPNRMOR)
uFAR <- unique(dat$LOPNRFAR)

# Identify listed as mother/father as well as index?
xLOPNR <- dat[ dat$bnr==1 , ]$LOPNR[ dat[ dat$bnr==1 , ]$LOPNR %in% c(uFAR,uMOR) ]

# Make parents vector with those without any parents
uParents <- c(uFAR,uMOR)
uParents <- uParents[ !( uParents %in% xLOPNR ) ]

### Checks
# Any parent-only id in offspring-only?
sum( uParents %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR )
# Any parent-only id in offspring-who-are-parents?
sum( uParents %in% xLOPNR )
# Any offspring-who-are-parents id in offspring-only?
sum( xLOPNR %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR  )
# Any offspring-who-are-parents id in parent-only
sum( xLOPNR %in% uParents )
# Any offspring-who-are-parents who ?
sum(xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR))

# Order these first
xLOPNR2 <- xLOPNR[ xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR)]
xLOPNR3 <- xLOPNR[ !(xLOPNR%in%xLOPNR2) ]

### Check that this suffices
# REMOVE THESE!!! - Doesn't work, how to solve?
sum(xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR))
chng <- xLOPNR2[xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR)]
length( chng )
# Fix so they appear first with no parents
xLOPNR2v2 <- xLOPNR2[!(xLOPNR2%in%chng)]

# Set up pedigree for analysis
p1 <- pedigree( 
  sire=c(rep(NA,length(uParents)+length(chng)) , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRFAR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRFAR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRFAR ) ,
  dam= c(rep(NA,length(uParents)+length(chng)) ,dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRMOR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRMOR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRMOR ) , 
  label= c(uParents,chng,xLOPNR2v2,xLOPNR3,dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR) , ]$LOPNR) )


# Run linear model
fm1 <- pedigreemm( konbarn ~ 1+(1|LOPNR) , data=dat , pedigree = list(LOPNR=p1) )
summary(fm1)
#save( fm1 , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_MEN_20191029.Rdata' )


## Get profile likelihood intervals
CI1 <- profile( fm1 , which='.sig01' , prof.scale="varcov" )
CI1save <- lapply( c(.90,.95,.975,.99,.999,.9998) , FUN=function(x){ confint( CI1 ,level=x) } )
CI1save
#save( CI1save , file='~/sexratio_identity_h2_animalmodel_indexBorn1932-1973_MEN_CI_20191029.Rdata' )




#########################################################################################
###
### 5: All data, index generation born before 1973. Logit link 
###
###

### To run on all data (excluding twin births)
# INdividuals born latest 1973 (to have finished their reproductive career)

### Data
dat <- read.table( '~/Familiality_of_offspring_sex_cohort_20190116.csv',header=T,sep=',')

# Exclude twin births parents!
temp <- dat[ dat$nrbornsamedate!=1 , ]
dat <- dat[ !(dat$LOPNR%in%temp$LOPNR) & !(dat$LOPNRFAR%in%temp$LOPNRFAR) & !(dat$LOPNRMOR%in%temp$LOPNRMOR) , ]

### Shorter time-span for index generation!
# Individuals born latest 1973 (to have finished their reproductive career)
tempid <- dat[ dat$FODELSEDATUM<197300 ,]$LOPNR
dat <- dat[ dat$LOPNR%in%tempid , ]

### Set up data for pedigree (founders need to be ordered first)
# Unique parents
uMOR <- unique(dat$LOPNRMOR)
uFAR <- unique(dat$LOPNRFAR)

# Identify listed as mother/father as well as index
xLOPNR <- dat[ dat$bnr==1 , ]$LOPNR[ dat[ dat$bnr==1 , ]$LOPNR %in% c(uFAR,uMOR) ]

# Make parents vector with those without any parents
uParents <- c(uFAR,uMOR)
uParents <- uParents[ !( uParents %in% xLOPNR ) ]

### Checks
# Any parent-only id in offspring-only?
sum( uParents %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR )
# Any parent-only id in offspring-who-are-parents?
sum( uParents %in% xLOPNR )
# Any offspring-who-are-parents id in offspring-only?
sum( xLOPNR %in% dat[ !(dat$LOPNR%in%xLOPNR) , ]$LOPNR  )
# Any offspring-who-are-parents id in parent-only
sum( xLOPNR %in% uParents )
# Any offspring-who-are-parents who ?
sum(xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR))

# Order these first
xLOPNR2 <- xLOPNR[ xLOPNR %in% c(dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR) , ]$LOPNRMOR)]
xLOPNR3 <- xLOPNR[ !(xLOPNR%in%xLOPNR2) ]

### Check that this suffices (nope, not in total data)
sum(xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR))
chng <- xLOPNR2[xLOPNR2 %in% c(dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRFAR,dat[ (dat$LOPNR%in%xLOPNR2) , ]$LOPNRMOR)]
length( chng )
# Fix so they appear first with no parents
xLOPNR2v2 <- xLOPNR2[!(xLOPNR2%in%chng)]

p1 <- pedigree( 
  sire=c(rep(NA,length(uParents)+length(chng)) , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRFAR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRFAR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRFAR ) ,
  dam= c(rep(NA,length(uParents)+length(chng)) ,dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR2v2), ]$LOPNRMOR , dat[ dat$bnr==1 & (dat$LOPNR%in%xLOPNR3), ]$LOPNRMOR ,  dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR), ]$LOPNRMOR ) , 
  label= c(uParents,chng,xLOPNR2v2,xLOPNR3,dat[ dat$bnr==1 & !(dat$LOPNR%in%xLOPNR) , ]$LOPNR) )

# Run logit model
fm1 <- pedigreemm( konbarn ~ 1+(1|LOPNR) , data=dat , pedigree = list(LOPNR=p1), family=binomial(link='logit') )
summary(fm1)
#save( fm1 , file='~/sexratio_logit_h2_animalmodel_indexBorn1932-1973_20200123.Rdata' )

# Profile likelihood interval is broken. Thus no intervals are calculated.



###################################################################################################
###################################################################################################
###################################### END OF FILE ################################################
###################################################################################################
###################################################################################################