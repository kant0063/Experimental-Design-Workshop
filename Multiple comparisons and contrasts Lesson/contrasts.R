
####Contrasts
##simple linear model
library(daewr)
bread
mod0<-lm(height~time, data=bread)
summary(mod0)

###contrasts
library (gmodels)
fit.contrast(mod0,"time", c(1,-1,0))

library(daewr)
sugarbeet
mod4<-aov(yield ~ treat, data = sugarbeet)
con    <-matrix(c(1,-1/3,-1/3,-1/3,0,1,-1,0,0,0,1,-1),4,3)
L   <-t(con)
rownames(L)<-c("-fertilizer effect","-plowed vs. broadcast",
               "-Januaray vs. April")

#use this matrix to do the preplanned contrasts
options(digits =3)
library(gmodels)
fit.contrast(mod4,"treat",L)

##contrasts can also be used to interpret quantitative levels
##looking for linear or various polynomial interactions

contrasts(bread$time)<-contr.poly(3)
#The resulting contrast matrix below has coefficients for the linear and quadratic contrasts.
contrasts(bread$time)
mod3<-aov(height~time, data=bread)
summary.lm(mod3)
