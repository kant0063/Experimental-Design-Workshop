#Random effects Politness example
#load libraries needed for the analysis
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(lattice)
library(raster)
library(effects)


#read in data file
politeness<-read.csv("G:/My Drive/Teaching Experimental design/assignments/politness.csv" ) #read in data

names(politeness) #names of columns
head(politeness) #first six lines
tail(politeness) #last six lines

#initial visualizations
#look at boxplots of scenario on the x axis and frequency on the y-axis
qplot(scenario, frequency, facets = . ~ subject, # quick plot wrapper to use ggplot functions with standard plot appearance
      colour = subject, geom = "boxplot", data = politeness)
#look at boxplots of atttitude on the x axis and frequency on the y-axis
qplot(attitude, frequency, facets = . ~ subject, 
      colour = subject, geom = "boxplot", data = politeness)

##subset dataframe
list_pol <- split(politeness, politeness$attitude) #create a list object of the dataset split on attitude
inf_freq<-data.frame(list_pol$inf$frequency) #create a new dataframe where only a column informal frequencies are present
pol_freq<-data.frame(list_pol$pol$frequency) #create a new dataframe where only a column formal frequencies are present
new_cor<-cbind(inf_freq,pol_freq) #combine the two new dataframe into a single dataframe 

##create a graph of the within subjects correlation of frequncy by condition
ggplot(data=new_cor, aes(x=list_pol.inf.frequency, y=list_pol.pol.frequency)) + #create an xyplot of the within correlation
  geom_smooth(method='lm') + #create line fit
  geom_point() #add data points

####create mixed effects model
#first model we decide which variables are fixed and which are random
#random intercepts for each participant, i.e. assume an intercept that's different for each subject
politeness.model <- lmer(frequency ~ attitude + (1|subject) + 
                           (1|scenario), data=politeness)
summary(politeness.model) #look at the significance each treatment in a regression context
anova(politeness.model) #look at the anova of the fixed effects
rand(politeness.model) # look at the anova for the random effects

#graph to at the look addtional fixed effects that were not include in the intial model like gender
#sumerise by gender and attitude
freq_bycond <- na.omit(politeness) %>% # remove the missing data from the dataframe politeness
  group_by(gender, attitude) %>% #group by gender and attitude
  summarise(mean_freq = mean(frequency), se_freq = sd(frequency)/sqrt(n()), coef_var = cv(frequency)) #calculate the mean , se and cv
freq_bycond
#plot the summary statistics
ggplot(freq_bycond, aes(x=attitude, y=mean_freq, 
                     colour=gender, group=gender)) +
  geom_line(size=2) + geom_point(size=5, shape=21, fill="white")

##create a new model with an additional fixed effect of gender
politeness.model_2 <- lmer(frequency ~ attitude +
                             gender + (1 |subject) + (1 | scenario), data=politeness)
summary(politeness.model_2)
anova(politeness.model_2)
rand(politeness.model_2)

##summarize data by subject
freq_bysubj <- na.omit(politeness) %>% #remove missing observations from dataframe politness
  group_by(gender,attitude, subject) %>% # summarize by lots of different grouping factors 
  summarise(mean_freq_sub = mean(frequency),
            se_freq_sub = sd(frequency)/sqrt(n()),
            coef_var = cv(frequency)) #calculate mean, se, cv
freq_bysubj
##plot change in frequcy by subject
ggplot(freq_bysubj, aes(x=attitude, y=mean_freq_sub, 
                        colour=subject, group=subject)) +
  geom_line(size=2)+
  geom_point(size=4)


##add in an effect of a random slope
politeness.model_3 <- lmer(frequency ~ attitude +
                             gender + (1+ attitude | subject) + #
                             (1|scenario), data=politeness)
summary(politeness.model_3)
anova(politeness.model_3)
rand(politeness.model_3)

###summerise data scenario and attitude 
freq_byscenario <- na.omit(politeness) %>% # remove missing data from politeness
  group_by(scenario, attitude) %>% # group data
  summarise(mean_freq = mean(frequency), se= (sd(frequency)/n()), coef_var=cv(frequency))
freq_byscenario

#create a graph of slopes of scenario
ggplot(freq_byscenario, aes(x=attitude, y=mean_freq, 
                  color=factor(scenario), group=scenario)) +
                  geom_line() + geom_point(shape=21, fill="white")

###random slope for scenarios
politeness.model_4 <- lmer(frequency ~ attitude +
                            gender + (1 |subject) +
                            (1+attitude|scenario),
                          data=politeness)

summary(politeness.model_4)
anova(politeness.model_4)
rand(politeness.model_4)

#create model with a random effects interaction
politeness.model_5 <- lmer(frequency ~ attitude+gender + (1 |subject) +
                               (1 | attitude:scenario),
                           data=politeness)

summary(politeness.model_5)
anova(politeness.model_5)
rand(politeness.model_5)

###create an interaction plot
e<-allEffects(politeness.model_5)
#lattice like plot on fixed effects
plot(e)

# allEffects() returns a list, but for our model (where everything can interact with everything), there's only one element
e1 <- e[[1]]
e.df <- as.data.frame(e1)
ggplot(e.df,aes(x=attitude,y=fit,ymin=lower,ymax=upper)) + 
  geom_pointrange(position=position_dodge(width=.1)) + 
  xlab("Attitude") + ylab("Frequency") + ggtitle("Effect of Attitude and Frequency")

###create a model with nested random effects
politeness.model_6 <- lmer(frequency ~ attitude+gender + (1 |subject) +
                             (1 | scenario/attitude),
                           data=politeness)

summary(politeness.model_6)
anova(politeness.model_6)
rand(politeness.model_6)


###compare different models
anova(politeness.model_2,politeness.model_3, politeness.model_4)

anova(politeness.model_3,politeness.model_5, politeness.model_6)

#assumptions
#random error
plot(na.omit(politeness$frequency), resid(politeness.model_4))
abline(h=0)

#equal variance by doing a levene test by hand
M4_Res<- residuals(politeness.model_2) #extracts the residuals and places them in a vector
Abs_M4_Res <-abs(M4_Res) #creates a vector with the absolute value of the residuals
M4_Res2 <- Abs_M4_Res^2 #squares the absolute values of the residuals to provide the more robust estimate
napf<-na.omit(politeness$frequency) #remove missing data from politeness
Levene_Model <- lm(M4_Res2 ~ napf) #ANOVA of the squared residuals
anova(Levene_Model) #displays the results

#create plot of residuals
plot(politeness.model_4)
#create qqplot
qqmath(politeness.model_4, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
