##############################################################################################
###
### File name:  Offspring_sexratio_Plots.R
### Author:     Ralf Kuja-Halkola
### Created:    2020-01-29
### Purpose:    Create plots of sex ratio.
###
##############################################################################################


###############################################
### Used libraries
library(drgee)
###############################################


###############################################
### Conditioning on final family size

### Read in data
load( file='~/Familiality_of_offspring_sex_cohort_20190116.Rdata' )
dat <- dat[ dat$nrbornsamedate==1 , ]

###############################################
### Plot offspring sex ratio per year,
dat$byearbarn <- as.numeric( substr( dat$dobbarn , 1 , 4 ) )
datBarn <- cbind( tapply( 1-dat$konbarn , dat$LOPNRBARN , FUN='mean' ) ,  tapply( dat$byearbarn , dat$LOPNRBARN , FUN='mean' ) , tapply( dat$LOPNRMOR , dat$LOPNRBARN , FUN='mean' ) )
datBarn <- data.frame( konbarn=datBarn[,1] , byearbarn=datBarn[,2] , LOPNRMOR=datBarn[,3])

### GEE
yrs <- 1951:2013
obsBarn <- data.frame(summary(gee( konbarn~1 , data=datBarn[ datBarn$byearbarn==yrs[1] , ] , link='logit' ))$coef,year=yrs[1])
for(i in yrs[-1]){
  temp <- data.frame(summary(gee( konbarn~1 , data=datBarn[ datBarn$byearbarn==i , ] , link='logit' ))$coef,year=i)
  obsBarn <- rbind( obsBarn , temp )
  print(i)
}

# Overall mean and CI
datBarn <- datBarn[ order(datBarn$LOPNRMOR) , ]
overallmean <- summary(gee( konbarn~1 , data=datBarn , link='logit' , clusterid='LOPNRMOR' ))$coef

ests <- 1/(1+exp(-obsBarn[,1]))
lbound <- 1/(1+exp(-(obsBarn[,1]-1.96*obsBarn[,2])))
ubound <- 1/(1+exp(-(obsBarn[,1]+1.96*obsBarn[,2])))
overall <- c(1/(1+exp(-overallmean[,1])), 1/(1+exp(-(overallmean[,1]-1.96*overallmean[,2]))) ,  1/(1+exp(-(overallmean[,1]+1.96*overallmean[,2]))) )


###############################################





###############################################
### Plot parental sex-ratio per year
dat$byear <- as.numeric( substr( dat$FODELSEDATUM , 1 , 4 ) )
datParent <- data.frame( OSR= tapply( 1-dat$konbarn , dat$LOPNR , FUN='mean' ) ,  byear=tapply( dat$byear , dat$LOPNR , FUN='mean' ) ,  LOPNRMOR=tapply( dat$LOPNRMOR , dat$LOPNR , FUN='mean' ) )

# Do gee calculations
fitoverall <- gee( OSR ~ 1 , data=datParent , link='identity' , clusterid='LOPNRMOR' )
plotall <- summary(fitoverall)$coef[,1:2]
fit <- gee( OSR ~ 0+as.factor(byear) , data=datParent , link='identity' , clusterid='LOPNRMOR' )
plotsak <- cbind( 1932:1999 , summary(fit)$coef[ , 1:2 ] )
rownames(plotsak) <- NULL



# PLot
estsParent <- plotsak[,2]
lboundParent <- plotsak[,2]-1.96*plotsak[,3]
uboundParent <- plotsak[,2]+1.96*plotsak[,3]
overallParent <- c(plotall[1],plotall[1]-1.96*plotall[2],plotall[1]+1.96*plotall[2])

yrsParent <- 1932:1992

###############################################





###############################################
### Combined plot

### Aligning birth years
pdf('~/sexratio_parents_and_offspring_align_birthyear_20190912.pdf',height=12.5,width=13)
par( mfrow=c(2,1) )
par( family='mono' )
par( mar=c(7,6,6,3) )
plot( NULL , xlim=range(c(yrs,yrsParent)), bty='l' , xlab='Parent birth year' , ylab='Proportion males' , ylim=c(.50,.535) , main='Offspring sex ratio by birth year' , yaxp=c(.50,.53,3 ))
polygon( c(1931,1993,1993,1931) , c(overallParent[2],overallParent[2],overallParent[3],overallParent[3]) , col=rgb(0,0,0,alpha=.05) , border=NA )
lines( c(1931,1993) , c(overallParent[1],overallParent[1]) , lty=2 , col='grey' )

for(i in 1:length(yrsParent) ){ lines( rep(yrsParent[i],2) , c(lboundParent[i]+.0002,uboundParent[i]-.0002) , lty=2 , lwd=.7 , col='grey' ) }
points( yrsParent , lboundParent[1:length(yrsParent)] , pch='-' , col=1 ,cex=1.0 , lwd=1 )
points( yrsParent , uboundParent[1:length(yrsParent)] , pch='-' , col=1 ,cex=1.0 , lwd=1 )

points( yrsParent , estsParent[1:length(yrsParent)] , pch='-' , col=1 ,cex=1.7 , lwd=3 )


plot( NULL , xlim=range(c(yrs,yrsParent)), bty='l' , xlab='Offspring birth year' , ylab='Proportion males' , ylim=c(.495,.536) , main='Sex ratio by birth year' )
polygon( c(1950,2014,2014,1950) , c(overall[2],overall[2],overall[3],overall[3]) , col=rgb(0,0,0,alpha=.05) , border=NA )
lines( c(1950,2014) , c(overall[1],overall[1]) , lty=2 , col='grey' )

for(i in 1:length(yrs) ){ lines( rep(yrs[i],2) , c(lbound[i]+.0002,ubound[i]-.0002) , lty=2 , lwd=.7 , col='grey' ) }
points( yrs , lbound , pch='-' , col=1 ,cex=1.0 , lwd=1 )
points( yrs , ubound , pch='-' , col=1 ,cex=1.0 , lwd=1 )

points( yrs , ests , pch='-' , col=1 ,cex=1.7 , lwd=3 )


dev.off()




###############################################


##############################################################################################
##############################################################################################
################################# END OF FILE ################################################
##############################################################################################
##############################################################################################