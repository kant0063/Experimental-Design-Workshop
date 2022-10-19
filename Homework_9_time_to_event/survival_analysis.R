#survival analysis

library(survival)
library(dplyr)
library(OIsurv) # Aumatically loads KMsurv
library(ranger)
library(ggplot2)

data(ovarian)

#futime:	survival or censoring time
#fustat:	censoring status
#age:	in years
#resid.ds:	residual disease present (1=no,2=yes)
#rx:	treatment group
#ecog.ps:	ECOG performance status (1 is better, see reference)


S1<-with(ovarian, Surv(futime,fustat))

#The confBands() function from the OIsurv package 
#estimates Hall-Wellner confidence bands. 

cb <- confBands(S1, type = "hall")

#The formula y_bmt ~ 1 instructs the survfit() function to fit 
#a model with intercept only, and produces the Kaplan-Meier estimate.

fit  <- survfit(Surv(futime, fustat) ~1, data=ovarian)
summary(fit)

#plots the survival function
plot(fit,xlab="t",ylab=expression(hat(S)*"(t)"),main = 'Kaplan Meyer Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(50, 0.25, legend = c('K-M survival estimate', 'pointwise intervals', 
                            'Hall-Werner conf bands'), lty = 1:3)

#compare the use of the prescription
fit  <- survfit(Surv(futime, fustat) ~ rx, data=ovarian)
summary(fit)

coxaml <- coxph(Surv(futime, fustat) ~ rx, data=ovarian) #or do cox regression with treatment as predicto
summary(coxaml)
#plots the survival function
plot(fit,xlab="t",ylab=expression(hat(S)*"(t)"),main = 'Kaplan Meyer Plot')



##create a hazard function
# Fit Cox Model
form <- formula(S1 ~ age + rx +resid.ds +ecog.ps) #create a fomula based on 
cox_fit <- coxph(form,data = ovarian)
summary(cox_fit)

cox_fit_ov <- survfit(cox_fit)
plot(cox_fit_ov)

#hypothesis test
# Note this is very conservative!
cox.zph(cox_fit)

#testing for proportional hazards
a <- cox.zph(cox_fit)
par(mfrow = c(1, 1))
plot(a[1], main = "Age")
plot(a[2], main = "Rx")
plot(a[3], main = "Resid.ds")
plot(a[4], main = "Ecog.ds")

#plot by stratified levels of treatment

form <- formula(S1 ~ age + rx +resid.ds +ecog.ps) #create a fomula based on 
cox_fit_2 <- coxph(form,data = ovarian)
summary(cox_fit_2)

#HR=1: no effect
#HR<1: reduction in hazard
#HR>1: Increase in Hazard
#probability = (hazard ratio) / (1 + hazard ratio).
#get percent increase in hazard
exp(cox_fit_2$coefficients)/ (1+ exp(cox_fit_2$coefficients)) # this percent increase in risk


cox.zph(cox_fit_2)

plot(survfit(cox_fit_2), col = 1:2)
legend(800,1, legend = c('No Drug', 'Drug'), 
       lty = 1,  col = 1:2, title = "Treatment")


##random forest model
# ranger model
# ranger model
r_fit_ov <- ranger(form, data = ovarian, importance = "permutation", seed = 1234)

# Average the survival models
death_times <- r_fit_ov$unique.death.times
surv_prob <- data.frame(r_fit_ov$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit_ov$unique.death.times,r_fit_ov$survival[1,], type = "l", 
     ylim = c(0,1),
     col = "red",xlab = "death times", ylab = "survival",main = "Patient Survival Curves")

for(n in c(2:26)){
  lines(r_fit_ov$unique.death.times, r_fit_ov$survival[n,], type = "l", col = "red")
}
lines(death_times, avg_prob, lwd = 2)
legend(100, 0.15, legend = c('Average - black'))


#plot comparison of all three methods
# Set up for ggplot
km <- rep("KM", length(fit$time))
km_df <- data.frame(fit$time,fit$surv,km)
names(km_df) <- c("Time","Surv","Model")

cox <- rep("Cox",length(cox_fit_ov$time))
cox_df <- data.frame(cox_fit_ov$time,cox_fit_ov$surv,cox)
names(cox_df) <- c("Time","Surv","Model")

rf <- rep("RF",length(r_fit_ov$unique.death.times))
rf_df <- data.frame(r_fit_ov$unique.death.times,avg_prob,rf)
names(rf_df) <- c("Time","Surv","Model")
plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line() + ggtitle("Comparison of Survival Curves")

##########
gehan<-read.csv("C:/Users/Michael/Google Drive/Teaching Experimental Design/gehan_surv.csv", header=TRUE) 
str(gehan)


fit  <- survfit(Surv(time,failure)~1, data=gehan)
summary(fit)
S2<-with(gehan,Surv(time, failure))
cb1 <- confBands(S2, type = "hall")

plot(fit,xlab="t",ylab=expression(hat(S)*"(t)"),main = 'Kaplan Meyer Plot with confidence bands')
lines(cb1, col = "red",lty = 3)
legend(16, 1, legend = c('K-M survival estimate', 'pointwise intervals', 
                            'Hall-Werner conf bands'), lty = 1:3)


##############analyze with the surivival package 
# Load required packages
library(survival)
library(survminer)
library(dplyr)

gehan<-read.csv("G:/My Drive/Teaching Experimental design/Lectures/Survival Analysis/gehan_surv.csv", header=TRUE) 
head(gehan)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(gehan$time,gehan$failure)

fit1 <- survfit(surv_object ~ treatment, data = gehan)
summary(fit1)

#logrank test see if treatments are different
ggsurvplot(fit1, data = gehan, pval = TRUE)
