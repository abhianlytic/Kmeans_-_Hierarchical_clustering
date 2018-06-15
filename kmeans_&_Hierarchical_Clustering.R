#install.packages('rattle')
data(wine, package='rattle.data') #inbuilt data in rattle package
head(wine)


wine.stand <- scale(wine[-1])  # To standarize the variables
View(wine.stand)
# K-Means #flat clustering where we decide number of clustering
k.means.fit <- kmeans(wine.stand, 3) # k = 3
attributes(k.means.fit)


#centroid
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size

sum(apply(wine.stand,2,var)) #calculating variance column wise & 
#then summing it up.
(nrow(wine.stand)-1)#total number of observation-1
(nrow(wine.stand)-1)*sum(apply(wine.stand,2,var)) #within sum of
#variance in the data.
#now we will turn the code in loop to find wss for different 
#clusters created using Kmeans.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 
#Our whole purpose here to reduce wss, and as we see after 3 or 4
# clusters we are not able to reduce wss much, so we can go ahead
# with 3 or 4.

library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
#good job with 3 clusters, as the we are able to make a good
#seperation between different clusters.

table(wine[,1],k.means.fit$cluster)


#Hierarchical clustering #soft clustering, as the clusters get
#decided automatically.
d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.

H.fit <- hclust(d, method="ward")

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red") 

table(wine[,1],groups)
#here we can see that hierarchical is doing good job in creating
# a good clusters.

clusplot(wine.stand, groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
#though this plot does have more overlapping than the kmeans
#clustering but this method is able to distiguish the class well.