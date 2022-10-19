#repeated measures
library(psych)
library(ggplot2)
library(nlme)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)

reap_meas<-read.csv("G:/My Drive/Teaching Experimental design/Lectures/Repeated Measures/repeated_measures.csv")

with(reap_meas,plot(Month,Calories.per.day,  col=Instruction))

#identify autocorrelation
model_cor <- lme(Calories.per.day ~ Instruction + Month + Instruction*Month, 
              random = ~1|Student,
              data=reap_meas)
#get correlation for different lags
ACF(model_cor)

#create the correlation model than use the gls function to run the model
model <- gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
            correlation = corAR1(form = ~ Month | Student,
            value = 0.4287), 
            data=reap_meas,
            method="REML") #autocorrelation structure must be modled

model <- gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
             correlation = corAR1(form = ~ 1 | Month,
                                  value = 0.4287), 
             data=reap_meas,
             method="REML") #autocorrelation structure must be modled



#results of the model
library(car)
anova(model)

#create fixed effects model, i.e. no correlation structure
model.fixed <- gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
                  data=reap_meas, method="REML")
#look at the significance of the random effects
anova(model,model.fixed)
#use the nagelkerke function to calculate the R-square of the model
model.null <- lme(Calories.per.day ~ 1, random = ~1|Student, data = reap_meas)
nagelkerke(model,  model.null)

#do multiple comparisons

marginal <- lsmeans(model, ~ Instruction:Month) #create the ls means comparisons your intersted in

cld(marginal, alpha   = 0.05, Letters = letters, ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

#Create an interaction plot
#Create a new object to plot
Sum <- groupwiseMean(Calories.per.day ~ Instruction + Month,
                    data   = reap_meas,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = TRUE)

pd <- position_dodge(.2)

ggplot(Sum, aes(x =    Month,
                y =    Mean,
                color = Instruction)) +
geom_errorbar(aes(ymin=Percentile.lower,ymax=Percentile.upper), width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean calories per day")



