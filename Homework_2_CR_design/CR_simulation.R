#simulate some fake data

##Lets create some fake data
#create values for four treatments for factor 1
#create values for two treatments for factor 2
#simulate data
value<-rnorm(15, c(10,15,20,25,30),3)
#create factor 1
trt<-rep(c("trt1","trt2","trt3","trt4", "trt5"),3)
#create dataframe
CR<-data.frame(value,trt)

##explore data

names(CR)

###Get some summary Statistics

summary(CR)

#####Get summary by Treatment
#Sumerize the four yield by treatment, here we are exploring the mean
library (dplyr)
library(raster)
comb_stats <- 
  CR %>% 
  group_by(trt) %>% # <- remember to group by the important factor
  summarise(Means = mean(value), SE = sd(value)/sqrt(n()), 
            CV = cv(value))
comb_stats

#####Identify the hypothesis
##Ho = no difference between treatments
##Ha = there is a differnce between treatments

##Create a linear model using aov function
##Step one 
##make sure each variable has the correct form (numeric or factor)
str(CR)

##Create the model, we want to see yield differences explained by replication and treatment

CR_out<- aov(value ~ trt, data=CR)

##Lets sumarize our model

summary(CR_out)

#Do we meet the assumptions of our model
par(mfrow=c(2,2))
plot(CR_out)

par(mfrow=c(1,1))

###So we know that there are significant differences between treatments, but not which treatment provided is best

##Lets do a multiple comparison
##Least Significant Difference, this is a test that asks what is the minimum difference between treatments that is significant

###This is a library that will calculate an LSD for you
library(agricolae)

###Lests use the LSD.test function
##you must tell the function what model you want to use
##You must the tell the function what you want to seperate in our case the treatment
#console=True means that I want the output printed on the screen

Treat_diff<-LSD.test(CR_out, "trt", console=TRUE)

###Its nice to have the table print out but lets create a graphic
###To do this we need to use the bar.group function
###we give this function the "groups" paramater from our LSD test
###the other paramaters are standard graphical paramaters
#y-lim gives the size of the y axix
#density asks if you want the bars shaded
#Border asks what color you want the boarder of the boxes to be
#main asks what you want the title of the graph to be
#xlab is the x-axis label
#ylab is the y-asix label

bar.group(Treat_diff$groups,ylim=c(0,40),density=0, border="black", 
          main = "LSD of Treatment Differences",
          ylab="Yield", xlab="Treatment")




