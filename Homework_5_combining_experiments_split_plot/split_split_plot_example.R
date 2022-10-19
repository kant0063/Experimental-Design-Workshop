###split-split plot

# Statistical procedures for agricultural research, pag 143
# Grain Yields of Three Rice Varieties Grown under 
#Three Management practices and Five Nitrogen levels; in a
#split-split-plot design with nitrogen as main-plot, 
#management practice as subplot, and variety as sub-subplot 
#factores, with three replications.
library(agricolae)
library(agridat)
library(plot3D)

ssp<-read.csv("C:/Users/Michael/Google Drive/Teaching Experimental design/SSPED_example.csv")


str(ssp)

model<-with(ssp,ssp.plot(block,nitro,management,gen,yield))
summary(model)
gla<-model$gl.a; 
glb<-model$gl.b; 
glc<-model$gl.c
Ea<-model$Ea; 
Eb<-model$Eb; 
Ec<-model$Ec

par(mfrow=c(1,3),cex=0.6)
out1<-with(ssp,LSD.test(yield,nitro,gla,Ea,console=TRUE))
out2<-with(ssp,LSD.test(yield,management,glb,Eb,console=TRUE))
out3<-with(ssp,LSD.test(yield,gen,glc,Ec,console=TRUE))
bar.group(out1$groups, ylim = c(0,9), xlab="Nitrogen", ylab="Yield")
bar.group(out2$groups,ylim = c(0,9),xlab="Management")
bar.group(out3$groups,ylim = c(0,9),xlab="Variety")


# with aov
ssp_aov<-aov(yield ~ nitro*management*gen + Error(block/nitro/management),
         data=ssp)
summary(ssp_aov)

ssp_aov_assumption<-aov(yield ~ block + nitro + block/nitro + management + management*nitro +management*nitro/block
                        +gen + gen*management + gen*nitro +gen*management*nitro, data=ssp)

summary(ssp_aov_assumption)
par(mfrow=c(2,2))
plot(ssp_aov_assumption)


##interaction plots
library(phia)
ssp_mean<-interactionMeans(ssp_aov_assumption)
plot(ssp_mean)
interaction.plot(ssp$nitro,ssp$management, ssp$yield)
interaction.plot(ssp$nitro,ssp$gen,ssp$yield)
interaction.plot(ssp$management, ssp$gen, ssp$yield)

####3d-plots
library(scatterplot3d)
scatterplot3d(ssp$nitro, ssp$management, ssp$yield)
scatterplot3d(ssp$nitro, ssp$gen, ssp$yield)
scatterplot3d(ssp$gen, ssp$management, ssp$yield)

