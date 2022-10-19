#Generic split plot

###Split Plot

SPD<-read.csv("camel_cheese.csv")

names(SPD)
head(SPD)

library(lattice)

dotplot(Response~Split.Plot|Whole_Plot, SPD, group=Block, las=2,
        main=" Variable Measured", ylab="Response")

#####Get summary by Treatment
#Sumerize the four yield by treatment, here we are exploring the mean
library (dplyr)
library(raster)
comb_stats <- 
  SPD %>% 
  group_by(Whole_Plot, Split.Plot) %>% # <- remember to group by the important factor
  summarise(Means = mean(response), SE = sd(response)/sqrt(n()), 
            CV = cv(response))
comb_stats


##########incorrect model
SPED_M1<-aov(Response~Whole_Plot*Split.Plot*factor(Block), data =SPD)
summary(SPED_M1)


##Correct model
SPED_M2<-aov(Response~Whole_Plot+Error(factor(Block)/Whole_Plot)+
               Split.Plot + Split.Plot*Whole_Plot, data = SPD)

summary(SPED_M2)
#check assumptions need the model specified
assum<-aov(Response~factor(Block)*Whole_Plot+Split.Plot + Split.Plot*Whole_Plot, data = SPD)
plot(assum)

##if you want to do f-tests by hand
#pf(q, df1, df2)

#Ftest whole plot
whole_plot_p<- 1-pf(,,)
#Ftest sub plot
split_plot_p<-1-pf(,,)
#Ftest interaction
interaction_p<- 1-pf(,,)

##mean seperation
library(agricolae)

names(SPED_M2)
Treat_diff<-LSD.test(SPED_M1, 'Whole_Plot', p.adj="fdr")

bar.group(Treat_diff$groups,ylim=c(0,90), density=0, border="black", 
          main = "LSD of Whole Plot Treatment Differences",
          ylab="Yield", xlab="Treatment")
Split_Treat_diff<-LSD.test(SPED_M1, "Split.Plot", p.adj="fdr")
bar.group(Split_Treat_diff$groups,ylim=c(0,90),density=0, border="black", 
          main = "LSD of Split plot differences",
          ylab="Yield", xlab="Method")

###Can also be done by default
model<-with(SPD,sp.plot(factor(Block),Whole_Plot, Split.Plot, Yield))

