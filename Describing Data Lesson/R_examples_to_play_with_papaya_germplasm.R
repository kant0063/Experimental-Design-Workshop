####Examples to play with

#Get data
####Read in file
###Need to know the directory where your data live
Phen<-read.csv("C:/Users/Michael/Downloads/Papaya_germplasm_collection.csv",header= TRUE) #filepath

gh<-Phen$FRUIT.DIAMETER[2:17]

gh2<-gh*3

plot(density(gh2), ylim=c(0,0.2), xlim=c(0,60))
lines(density(gh), col=4)

abline(v=13,col='red')
abline(h=11,col='orange')


rownames(Phen) <- Phen[, 1] ## set rownames
Phen <- Phen[, -1]          ## remove the first variable

####what are the column headings
names(Phen) #names of columns
str(Phen) #what type of variable is each column, numeric, factor, integer 
dim(Phen) #what are the dimensions of the dataset

###Get summary statistics
phen_sum<-summary(Phen)

###subset the data to only numeric variables
Phen_num <- Phen[, -c(14:27)]         
str(Phen_num)

##impute missing data
library(missMDA) #contains imputation function
completed_phen_num         <-imputePCA(Phen_num) #imputation function
pap_phen_num_scale <- scale(completed_phen_num$completeObs) #take out the imputed data


#look at all the numerical variables standardized
boxplot(pap_phen_num_scale, 
        ylab="Standarized value", main="Numeric Papaya data", #ylab changes y axis label, main changes label for the title
        col = terrain.colors(13),xaxt = "n")  

# x axis with ticks for each box but without labels
axis(1,1:13, labels = FALSE)

##Create labels
labels <- colnames(Phen_num) # make a vector of column names called labels

# Plot x labs at default x position
text(x =  seq_along(labels), y = par("usr")[3]-1 , #par("usr") [3] gets the range of the  y axes in the plot.
     srt = 35, adj = 1, #str is the angle of text, adj should the txt be on the x or y axis 
     labels = labels, xpd = TRUE, cex= 0.5) #labels adds labels, xpd TRUE limit plotting to the figure and device region, cex is font size


###summerize the data
library(dplyr) # contains funciton to summarise the data
library(raster) #contains cv function
library(plotrix) #contains standard error funciton

##summarise a single numeric variable from the dataset
summarise(Phen_num, avg = mean(FRUIT.DIAMETER), 
          min = min(FRUIT.DIAMETER),
          max = max(FRUIT.DIAMETER),
          stan_dev = sd(FRUIT.DIAMETER),
          coefficient_var = cv(FRUIT.DIAMETER),
          stan_err = std.error(FRUIT.DIAMETER))


#lets summarise everything but it prints it out in strange format
P_sum<-summarise_all(Phen_num, funs(mean, min,max,sd,cv,std.error),na.rm=TRUE)

#get dimension of the dataframe
dim(P_sum)

#we now want to create a dataframe that is readable
library(tidyr) #reshape the data
library(tidyselect) #package that contians functions that help reshape data

P_stats_tidy <- P_sum %>% gather(stat, val) %>% #%>% is a piping opporator to take the output of one funciton and apply another function to it 
  separate(stat, into = c("vaiable", "stat"), sep = "_") %>% #seperate the values by the 
  spread(stat, val) 

#write this dataframe to a new file
write.csv(P_stats_tidy, "C:/Users/Michael/Downloads/Papaya_sum.csv") #directory where new file will be sent

#####confidence interval
library(gmodels) #contains function to do confidence intervals
fr<-Phen$FRUIT.DIAMETER
CI_reg_95<-ci(Phen$FRUIT.DIAMETER,0.99)
CI_reg_95
CI_7_95<-ci(((Phen$FRUIT.DIAMETER)*7),0.95)
CI_10_95  <-ci(((Phen$FRUIT.DIAMETER)+10),0.95)


#plot a CI one way
library(psych) #contains error.bar function that plots confidence limits
error.bars(Phen_num, xlab="", na.rm=TRUE, xaxt = "n") 

##Create labels
labels <- colnames(Phen_num)

# Plot x labs at default x position
text(x =  seq_along(labels), y = par("usr")[3]-15 , srt = 35, adj = 1,
     labels = labels, xpd = TRUE, cex= 0.5)

###another way to create a graphic though inelegant
library(ggplot2) #contains plotting funcitons
library(Rmisc) #contains summary functions

fd <- summarySE(data=Phen_num,  measurevar = "FRUIT.DIAMETER") 

# Use 95% confidence intervals instead of SEM
ggplot(fd, aes(x="name", y=FRUIT.DIAMETER)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=FRUIT.DIAMETER-ci, ymax=FRUIT.DIAMETER+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))                                            

###Create graphic another way
P_s_t<-P_stats_tidy[-9,]


ggplot(P_s_t, aes(x=vaiable, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=(mean-(1.96*std.error)), ymax=(mean+(1.96*std.error))),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=5))

###Create a graphic another way

plot((density((Phen$FRUIT.DIAMETER)*7)),ylim=c(0,0.1))
abline(v=c(70.8,84.1))

lines((density((Phen$FRUIT.DIAMETER)+10)), col="blue" )
abline(v=c(20.1,22.01),col="blue")

lines((density((Phen$FRUIT.DIAMETER))), col="red" )
abline(v=c(10.11,12.01),col="red")

###look up critical value of t distribution
qt(0.975, 3) # the output of this value is used for constructing the confidence interval or finding the p-value


#look up probability of critical value

pt(2.5, 3) #find the pvalue wih a known critical value

#look up critical value of normal distibution

qnorm(0.975) # the output of this value is used for constructing the confidence interval or finding the p-value


#look up probability of critical value

pnorm(5) #find the pvalue wih a known critical value

#Create a randomization for an experiment

library(agricolae)
treatment = letters[1:4] # the new variable treatment will consist of the leters a,b,c,d
design.rcbd(trt=treatment, r=4) #create a randomized complete block design with trt designation treatment with four replications
