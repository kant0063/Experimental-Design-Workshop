####Power Analysis
library(pwr)

#You must provide an effect size on the expected scale. 
#Doing otherwise will produce wrong sample size and power calculations.

#Lets look at a t-test
pwr.t.test( d = 10 , sig.level = 0.001 , power = 0.9, type = "paired")


##lets look at anova with a moderate effect 
#get a value for a medium effect
cohen.ES(test = "anov", size = "medium")
#We have four groups, how many individuals will we need per group (i.e. replicates)
pwr.anova.test(k = 4, f = 10/8, sig.level = 0.05, power = 0.8)

#when we ask about the total number of individuals this becomes a very large number
#But what if we have a good idea of the the between group variation and within group variation
#we can use that instead
pwr.anova.test(k = 4, f = 1.7, sig.level = 0.05, power = 0.8)


# Plot sample size curves for detecting differences in an ANOVA

# range of effect size
f <- seq(0.1,0.5,0.01)
nf <- length(f)
# power values
p <- seq(.5,.9,.1)
np <- length(p)
# obtain sample sizes
samsize <- array(numeric(nf*np), dim=c(nf,np))
for (i in 1:np){
  for (j in 1:nf){
    result <- pwr.anova.test(n = NULL, k=4, f = f[j],
                             sig.level = .05, power = p[i])
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(f)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect size (f)",
     ylab="Sample Size per treatment (n)" )
# add power curves
for (i in 1:np){
  lines(f, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,col="grey89")
title("Sample Size Estimation for ANOVA")
legend("topright", title="Power", as.character(p),
       fill=colors)



####other power libraries
library(pwr2)
library(pwrAB) 
library(pwrFDR) 
library(PwrGSD) 
library(pwrRasch)
