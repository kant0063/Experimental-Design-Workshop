#simulating a two factor factorial
#following code assumes 10 replicates per group (30 total). 
#Factor1 has two levels. Factor2 has three levels.
Response<-rnorm(30,c(100,200,300),sd=10)
#Factor1<-rep(c("level1","level2"),each=3,5)
#Factor2<-rep(c("level1","level2", "level3"),each=1,10)

Factor1<-rep(c("temp1","temp2"),each=3,5)
Factor2<-rep(c("pressure1","pressure2", "pressure3"),each=1,10)


AllData<-data.frame(Response,Factor1,Factor2)


ANOVAresults<-aov(Response~Factor1*Factor2, AllData)
summary(ANOVAresults)
model.tables(ANOVAresults,"means")




Response<-rbinom(50,1,0.5)
