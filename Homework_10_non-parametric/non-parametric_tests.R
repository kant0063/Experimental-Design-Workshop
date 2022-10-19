###non-parametric anova
#non-parametric t-test
x<-c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y<-c(0.878, 0.647, 0.598, 2.050, 1.060, 1.290, 1.060, 3.140, 1.290)  
wilcox.test(y, x, paired=TRUE, alternative="less")
wilcox.test(y, x, paired=TRUE, conf.int=TRUE)

##if there are ties you should use the exact test
library(exactRankTests)
wilcox.exact(x, y, paired = TRUE,
             alternative = "greater")


####non-parametric one-way anova Kruskal Wallis Test One Way Anova by Ranks 

non_anova<-read.csv("G:/My Drive/Teaching Experimental design/Lectures/Non-Parametic Tests/kruskal_wallis.csv", header=TRUE) 
str(non_anova)

out<-kruskal.test(FST~class, data=non_anova) # where y1 is numeric and A is a factor
print(out)

###non-parametric anova Friedman Test
non_anova_2<-read.csv("G:/My Drive/Teaching Experimental design/Lectures/Non-Parametic Tests/Non_parametric_anova.csv", header=TRUE) 

str(non_anova_2)
non_anova_2$player<-as.factor(non_anova_2$player)

out <- friedman.test(time ~ method | player, data=non_anova_2)
print(out)

library(SuppDists)
pFriedman(out$statistic, nlevels(non_anova_2$method), nlevels(non_anova_2$player), lower.tail = FALSE)

