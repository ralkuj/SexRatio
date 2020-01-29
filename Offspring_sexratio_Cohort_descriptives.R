##############################################################################################
###
### File name:  Offspring_sexratio_Cohort_descriptives.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Estimate familial aggregation of offspring sex.
###
##############################################################################################
###############################################
### Create Rdata
#dat <- read.table( '~/Familiality_of_offspring_sex_cohort_20190116.csv' , sep=',' , header=T )
#save.image( file='~/Familiality_of_offspring_sex_cohort_20190116.Rdata' )
###############################################


###############################################
### Descriptive
# Load data
load( file='~/Familiality_of_offspring_sex_cohort_20190116.Rdata' )
###############################################


###############################################
### Create descriptive table index generation
### Dataset summarized over index generation individuals
# Sex
datsex <- tapply( dat$KON , dat$LOPNR , mean , simplify = T)
datsex <- data.frame( ID=as.numeric( names(datsex) ) , KON=as.vector(datsex) )

# Birth date
datfod <- tapply( dat$FODELSEDATUM , dat$LOPNR , mean , simplify = T)
datfod <- data.frame( ID=as.numeric( names(datfod) ) , byear=as.numeric(as.vector(substr(datfod,1,4))) )

# Number offspring
datbnr <- tapply( dat$bnr , dat$LOPNR , max , simplify = T)
datbnr <- data.frame( ID=as.numeric( names(datbnr) ) , bnr=as.vector(datbnr) )

# Had twins/triplets
dattwin <- tapply( dat$nrbornsamedate , dat$LOPNR , max , simplify = T)
dattwin <- data.frame( ID=as.numeric( names(dattwin) ) , twin=as.vector(dattwin) )

# Merge
datindex <- merge( datsex , datfod , by='ID' )
datindex <- merge( datindex , datbnr , by='ID' )
datindex <- merge( datindex , dattwin , by='ID' )

### Create table 
# Sex
out <- c( dim(datindex)[1] , table(datindex$KON) )
# Byears
out <- rbind( out,
  cbind( table( cut(datindex$byear,c(1931,seq(1940,2000,by=10))) ),   table( cut(datindex$byear,c(1931,seq(1940,2000,by=10))), datindex$KON ) 
    )
)
# Nroffspr
out <- rbind( out,
  cbind( table( cut(datindex$bnr,c(0:5,18)) ),   table( cut(datindex$bnr,c(0:5,18)), datindex$KON ) 
    )
)
# Ever twin
out <- rbind( out,
  cbind( table( datindex$twin>1) ,   table( datindex$twin>1, datindex$KON ) 
    )
)
# % of all
outpercent <- 100*out[1,]/out[1,1]
outpercent <- rbind( outpercent ,
  100*cbind( out[-1,1]/out[1,1] , out[-1,2]/out[1,2] , out[-1,3]/out[1,3] )
)

### For Table 1
cbind( paste( out[,1] , ' (' , sprintf('%.1f',round(outpercent[,1],1)) , ')' , sep='' ),
  paste( out[,2] , ' (' , sprintf('%.1f',round(outpercent[,2],1)) , ')' , sep='' ),
  paste( out[,3] , ' (' , sprintf('%.1f',round(outpercent[,3],1)) , ')' , sep='' )
)

# P-vals
binom.test(out[1,2],out[1,1])
chisq.test( out[2:8,2:3] )
chisq.test( out[9:14,2:3] )
chisq.test( out[15:16,2:3] )



###############################################

###############################################
### Create descriptive table offspring generation

### Need to limit the data to one obs per lopnrbarn
konbarn <- tapply( dat$konbarn , dat$LOPNRBARN , mean , simplify = T)
dat$byearbarn <- as.numeric( substr(dat$dobbarn,1,4) )
byearbarn <- tapply( as.numeric( substr(dat$dobbarn,1,4) ) , dat$LOPNRBARN , mean , simplify = T)
bnr <- tapply( dat$bnr , dat$LOPNRBARN , max , simplify = T)
twin <- tapply( dat$nrbornsamedate , dat$LOPNRBARN , max , simplify = T)

# Sex
outoff <- c( dim(konbarn)[1] , table(konbarn) )
# Byears
outoff <- rbind( outoff,
  cbind( table( cut(byearbarn,c(1940,seq(1960,2000,by=10),2014)) ),   table( cut(byearbarn,c(1940,seq(1960,2000,by=10),2014)), konbarn )
    )
)
# Nroffspr
outoff <- rbind( outoff,
  cbind( table( cut(bnr,c(0:5,18)) ),   table( cut(bnr,c(0:5,18)), konbarn ) 
    )
)
# Ever twin
outoff <- rbind( outoff,
  cbind( table( twin>1) ,   table( twin>1, konbarn ) 
    )
)

# % of all
outoffpercent <- 100*outoff[1,]/outoff[1,1]
outoffpercent <- rbind( outoffpercent ,
  100*cbind( outoff[-1,1]/outoff[1,1] , outoff[-1,2]/outoff[1,2] , outoff[-1,3]/outoff[1,3] )
)

### For Table 1
cbind( paste( outoff[,1] , ' (' , sprintf('%.1f',round(outoffpercent[,1],1)) , ')' , sep='' ),
  paste( outoff[,2] , ' (' , sprintf('%.1f',round(outoffpercent[,2],1)) , ')' , sep='' ),
  paste( outoff[,3] , ' (' , sprintf('%.1f',round(outoffpercent[,3],1)) , ')' , sep='' )
)

# P-vals
binom.test(outoff[1,2],outoff[1,1])
chisq.test( outoff[2:7,2:3] )
chisq.test( outoff[8:13,2:3] )
chisq.test( outoff[14:15,2:3] )
  
##############################################################################################
##############################################################################################
################################# END OF FILE ################################################
##############################################################################################
##############################################################################################
