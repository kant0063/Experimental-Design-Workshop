#   Script to perform Principal Components Analysis (PCA)

Data <- read.table("file path to data", header=TRUE)
Data <- read.csv("file path to data")

#Make sure the data is correct 
names(Data)
#Make sure data is correct
head(Data)
length(Data[1,])

#Remove labels from data inorder to be able to do the PCA
#here the first two columns contain information about the data but is not the data
Data1 <- Data[, -(1:2)]

#add rownames
rownames(Data1) <- Data[, 1] ## set rownames

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
scaled_data<-scale(Data1)
fit <- prcomp(scaled_data, scale=TRUE)
summary(fit) # print variance accounted for
plot(fit, type="lines") # scree plot
biplot(fit)

#extract first two principle components and place in another object for ploting
comp <- data.frame(fit$x[,1:2])

#plot PC1 x PC2 colored by country
plot(comp, pch=19) # it would be advisable to color this by some factor

#Make Legend
legend("bottomleft", title="",
  cex=0.75,  pch=16,
  col=c(""),legend=c(""),
  ncol=6)

# make plot of PC1 x PC2 x PC3
comp <- data.frame(fit$x[,1:3])
plot(comp, pch=19, col="by a factor")

# make plot of PC1 x PC2 x PC3 x PC4 x PC5
comp <- data.frame(fit3$x[,1:5])
plot(comp, pch=19)

#verify PCA with new R package for PCA
library(FactoMineR) #library for conducting PCA
library(factoextra) # library for plotting PCA
library(missMDA) #for imputing if you have missing data

impute_sd<-imputePCA(Data)
scaled_impute_sd<- scale(impute_sd$completeObs)
rownames(scaled_impute_sd)<-Data[,1]
#conduct the Principle component analysis
result <- PCA(scaled_impute_sd, scale.unit=TRUE, ncp=5, graph=T)
#make a plot of first two PC axics
plot.PCA(result, axes=c(1, 2), choix="ind")
#description of PC 
dimdesc(result)

#cluster the individuals by principle component scores
res.hpc2 <- HCPC(result, graph =FALSE)
#what is in the HCPC object

#data.clust: The original data with a supplementary column called class containing the partition.
#desc.var: The variables describing clusters
#desc.ind: The more typical individuals of each cluster
#desc.axes: The axes describing clusters
#res.hcpc$desc.var$quanti, display quantitative variables that describe the most each cluster

#graph  cluster denogram
fviz_dend(res.hpc2, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)
#great convex hulls
fviz_cluster(res.hpc2,
             repel = T,            # Avoid label overlapping
             geom= "point", #plot only points
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
#get variables
get_variables <- HCPC(result)
pca_data <- get_variables$desc.var
#description of PC axis
res.hpc2$desc.axes
#description of clusters
res.hpc2$desc.ind


write.csv(
  pca_data,
  "file path to output vaiable clusters",
  sep="\t",
  col.names=FALSE)

#create a pot with convex hulls around clusters by hand
#this allows you to creat hulls around any factor set
#getting the convex hull of each unique point set
library(ggplot2)


#getting the convex hull of each unique point set

df <- data.frame(result$ind$coord[,1:5])

#get cluster assignment
df$cluster<-as.factor(res.hpc2$data.clust$clust)

str(df)

find_hull <- function(df) df[chull(df$Dim.1, df$Dim.2), ]

hulls2 <- ddply(df, "cluster", find_hull)

#Clustering based on Three Groups makes a lot of sense
plot_of_polygons <- ggplot(data = df, aes(x = Dim.1, y = Dim.2, 
                                          colour=cluster, fill = cluster)) +
  geom_point() + 
  geom_polygon(data = hulls2, alpha = 0.5) +
  labs(x = "PC1", y = "PC2")

plot_of_polygons 

