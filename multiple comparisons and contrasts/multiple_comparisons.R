###multiple testing

library(agricolae) #library that has useful multiple comparisons functions

###let play with CRD model
out<- aov(weight~group, data =PlantGrowth) # plant growth is a dataset present in agricolae library
##get the anova table
summary(out)
#different multiple comparisons with functions built into the agriocoale library
#tukey
tukey.test <- TukeyHSD(out) #without a grouping factor all pairwise comparisons reported
tukey.test
plot(tukey.test) # a plot of the CI developed for treatment means

tukey.test2 <- HSD.test(out, trt = 'group') # with a grouping factor treatment groups are compared
tukey.test2

#Fishers Least significant difference
LSD.test <- LSD.test(out, "group", alpha = 0.05, p.adj= "holm")
LSD.test
bar.group(LSD.test$groups, ylim = c(0,8))

#studnet-newman-keuls (SNK) test
SNK_out <- SNK.test(out, trt = 'group')
SNK_out
bar.group(SNK_out$groups,ylim = c(0,8))

##Duncan multiple range test
duncan_out<-duncan.test(out, trt = 'group')
duncan_out
bar.group(duncan_out$groups,ylim = c(0,8))

##Dunnett test
library(multcomp)# addtional package that contains multiple comparisons funcitons
summary(glht(out, linfct = mcp(group = "Dunnett")))


