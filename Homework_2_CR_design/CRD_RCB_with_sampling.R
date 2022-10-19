###CRD and RCBD with subsampling

CR<-read.csv("G:/My Drive/Teaching Experimental design/Lectures/Sampling/CRD_with_sampling.csv")
##explore data
names(CR)
###Get some summary Statistics
summary(CR)

#####do the anova
sub_CR<- aov(Response ~ Trt, data=CR)

sub_CR<- aov(Response ~ Trt + Sub%in%Trt, data=CR)

#check the assumptions
plot(sub_CR)
##normality looks a little suspect
shapiro.test(sub_CR$residuals)
leveneTest(Response ~ Trt, data=CR)
bartlett.test(Response ~ Trt, data=CR)
#these data are not normal, transform
library(MASS)
library(car)
boxCox(sub_CR)
#lambda about -.5, this means the transformation should be the square root of the repciprical
#transformation
trans_response_CR<-sqrt(CR$Response^-1)
sub_transform_CR<- aov(trans_response_CR ~ Trt + Sub/Trt, data=CR)
#test_assumptions
plot(sub_transform_CR)
#still doesn't look normal
shapiro.test(sub_transform_CR$residuals)
#barley passes the the normality assumption
####look at the anova table
summary(sub_transform_CR)
#this isn't the correct error term
#Lets slightly rewrite the model
sub_transform_CR_correct<- aov(sqrt(Response^-1) ~ Trt + Sub/Trt, data=CR)
####look at the anova table 
plot (sub_transform_CR_correct)

#multiple comparison
TukeyHSD(sub_transform_CR, which = 'Trt') #use the anova model, tell the model which compariosn to make


########randomized complete block with sampling
RCBD<-read.csv("C:/Users/Michael/Google Drive/Teaching Experimental design/RCBD_sub_sampling.csv")
##explore data
names(RCBD)
###Get some summary Statistics
summary(RCBD)
###do the anova
RCBD_out <- aov(Response ~ factor(Block)+Trt+Trt/factor(Sample), data=RCBD)
###look at assumption
plot(RCBD_out)
###looks like there isn't normality
shapiro.test(RCBD_out$residuals)
#not-normal-transform
#transform
RCBD_out_sqrt <- aov(sqrt(Response) ~ factor(Block)+ Trt+ factor(Block)*Trt/factor(Sample), data=RCBD)
####look at anova
summary(RCBD_out_sqrt)

###calculate F-test
.4849/ 0.0661
#7.335 with 6 and 42 df
pv<-pf(7.34,6,42)
2*min(pv,1-pv)
#p=4.16*10^-5

library (dplyr)
factorial_stats <- 
  RCBD %>% 
  group_by(Trt) %>% # <- remember to group by the important factor
  summarise(Means = mean(Response), SE = sd(Response)/sqrt(n()))
factorial_stats

###bar-plots
library(ggplot2)
ggplot(factorial_stats, 
       aes(x = Trt, y = Means, fill = Trt,
           ymin = Means - SE, ymax = Means + SE)) +
  # this adds the mean
  geom_bar(stat = "identity", position = position_dodge()) +
  # this adds the error bars
  geom_errorbar(position = position_dodge(0.9), width=.2) +
  # controlling the appearance
  xlab("Population") + ylab("Yield (g dry weight)")

