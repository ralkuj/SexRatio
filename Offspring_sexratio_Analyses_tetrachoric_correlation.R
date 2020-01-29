##############################################################################################
###
### File name:  Offspring_sexratio_Analyses_tetrachoric_correlation.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Estimate tetrachoric correlation of offspring sex.
###
##############################################################################################


###############################################
### Used libraries
library(parallel)
library(polycor)
library(sampling)

###############################################


#######################################################################
### All pairs
#load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')
dat <- dat[ order(dat$LOPNRMOR) , ]

### Keep one pair so pairs aren't repeated in opposite order
dat <- dat[ dat$LOPNRBARN < dat$LOPNRBARN2 , ]

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

### Restrict data to full sibs only
dat <- dat[ dat$sibtype==0 , c('konbarn','konbarn2','LOPNRMOR' ) ]

# Create function that performs non-parametric bootstrapped analyses
loopFun <- function( nrs , datasamp='Family' ){
loopl<-0
returnthing <- rep(NA,nrs)
while( loopl<nrs ){
    if( datasamp=='Family' ){
      datFtemp <-  getdata( dat , cluster( dat , clustername = 'LOPNRMOR' , size = length(unique(dat$LOPNRMOR)) ,method = 'srswr' ) )
    }
    if( datasamp=='None' ){
      datFtemp  <- dat
    }
    tmptab <- table( datFtemp[,1] , datFtemp[,2] )
    tmptab <- (tmptab+t(tmptab))/2
    returnthing[loopl+1] <- polychor(tmptab)
    loopl <- loopl+1
    }
  return(returnthing)
}
# Observed correlation
loopFun( 1 , 'None' )

# Bootstrap CIs
n <- 200 # nr repeats
N <- 10 # nr cores

vec <- rep( n , N )
results <- mclapply( vec , loopFun , datasamp='Family' , mc.cores=N )

# Look at results
results
quantile( unlist( results ) , c(0,.005,.01,.025,.5,.975,.99,.995,1) )
# h^2 is twice observed full sibling correlation
2*quantile( unlist( results ) , c(.95,.975,.99,.995,1) )

#save(  list='results' , file=paste('Correlations_',n*N,'repeats_20190119.Rdata',sep='') )
#load(  file='~/Correlations_2000repeats_20190119.Rdata' )

#######################################################################




#######################################################################
### Only first-born
load('~/Familiality_of_offspring_sex_cousinpairs_20190116.Rdata')
dat <- dat[ order(dat$LOPNRMOR) , ]

### Keep one pair
dat <- dat[ dat$LOPNRBARN < dat$LOPNRBARN2 , ]

### Remove twins in offspring generation
dat <- dat[ dat$nrbornsamedate==1 & dat$nrbornsamedate2==1 , ]

# Only first born offspring
datFirst <- dat[ dat$bnr==1 & dat$bnr2==1 , ]

### Restrict data to full sibs only
datFirst <- datFirst[ datFirst$sibtype==0 , c('konbarn','konbarn2','LOPNRMOR' ) ]

# Create function that performs non-parametric bootstrapped analyses
loopFunFirst <- function( nrs , datasamp='Family' ){
loopl<-0
returnthing <- rep(NA,nrs)
while( loopl<nrs ){
    if( datasamp=='Family' ){
      datFtemp <-  getdata( datFirst , cluster( datFirst , clustername = 'LOPNRMOR' , size = length(unique(datFirst$LOPNRMOR)) ,method = 'srswr' ) )
    }
    if( datasamp=='None' ){
      datFtemp  <- datFirst
    }
    tmptab <- table( datFtemp[,1] , datFtemp[,2] )
    tmptab <- (tmptab+t(tmptab))/2
    returnthing[loopl+1] <- polychor(tmptab)
    loopl <- loopl+1
    }
  return(returnthing)
}
# Observed correlation
loopFunFirst( 1 , 'None' )

# Bootstrap CIs
n <- 200 # nr repeats
N <- 10 # nr cores

vec <- rep( n , N )
results <- mclapply( vec , loopFunFirst , datasamp='Family' , mc.cores=N )

### Results
results
quantile( unlist( results ) , c(0,.005,.01,.025,.5,.975,.99,.995,1) )
2*quantile( unlist( results ) , c(.975,.99,.995,1) )

#save(  list='results' , file=paste('Correlations_firstborn_',n*N,'repeats_20190119.Rdata',sep='') )
#load(  file=' /Correlations_firstborn_2000repeats_20190119.Rdata' )

#######################################################################


##############################################################################################
##############################################################################################
################################# END OF FILE ################################################
##############################################################################################
##############################################################################################