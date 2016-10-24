################################# DAMI Programming Assignment 2: Clustering ##################################

## IMPORTANT! In this document, when you see [FIXME], that is where you should fill in your corresponding R code! 

## If would like to implement every part by yourself, in your submissions, you just comment out the provided codes, and write down your own codes in the corresponding place instead!


###### Part 1: Euclidean Distance ######

# First create a 2*2 matrix A, where each row represent an observation point
A <- matrix(c(1.7, 5, 4, 72), 2, 2, byrow = T) 
A
# Then calculate the Euclidean distance between the two observation points
eu.distance <- function(x,y) sqrt(sum((x-y)^2))
d <- eu.distance(A[1,],A[2,]) # 3 points
print(d)




###### Part 2: Generate a Fake Dataset ######

set.seed(5)
x <- matrix(rnorm(300), 150, 2) # The dataset contains 150 observations, each of which is described by a two-element vector
xmean <- matrix(rnorm(8), 4, 2) # Generate a deviation matrix
which <- sample(1:4, 150, replace = T) # Randomly sample of number 1-4 for 150 times with replacement
x <- x + xmean[which, ] # Add the deviation to dataset in four randomly formed groups
plot(x, col = which, pch = 19) # Plot the distribution of the observation points


###### Part 3: K-Means Clustering ######


# Slightly complicated solution #
x.cluster <- cbind(x, which) # Add the randomly sampled 1-4 (a new column) to the dataset as the initial clusters (in this case, we specify 4 clusters)
while(TRUE){
  centroid <- c() # Create an empty vector to put the centroids
  for(g in 1:4){ # In this for loop, the position of each centroid is calculated - mean of observations assigned to each centroid
    centroid <- c(centroid, mean(x.cluster[x.cluster[, 3] == g, 1]), mean(x.cluster[x.cluster[, 3] == g, 2]))
  }
  centroid <- matrix(centroid, 4, 2, byrow = T) # Reform the vector into a matrix where each row is one centroid
  distance <- c()
  for(i in 1:nrow(x)){ # In this for loop, each observation is reassigned its closest centroid
    for(j in 1:4){ # First to calculate the Euclidean distance between each observation and each centroid
      dis <- eu.distance(x[i,],centroid[j,]) # 3 points
      distance <- c(distance, dis)
    }
  }
  distance <- matrix(distance, 150, 4, byrow = T) # Create the matrix where each row is an observation and each column is its distance to the four centroids
  centroid.label <- apply(distance, 1, which.min) # Choose the centroid with the shortest distance
  if(all(centroid.label == x.cluster[, 3])){ # If the centroid is not changing any more for each observation, stop the iteration
    km.clusters <- centroid.label
    centroid.matrix <- centroid
    break
  }else{ # Otherwise, assign the new centroids to the dataset, and continue the iteration
    x.cluster[, 3] <- centroid.label

    # Test code to check the evolution of clusters
    #plot(x, col = centroid.label, pch = 19)
    #points(centroid, pch = 19, col = 6, cex = 2)

  }  
}
# 4 points
plot(x, col = km.clusters, pch = 19) # Plot the clustered observation points 
points(centroid.matrix, pch = 19, col = 6, cex = 2) # Add centroids to the plot

# Easy Solution #
km.out <- kmeans(x, 4)
km.out
plot(x, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)

# 4 points

# Try 3 clusters with kmeans function and plot the clusters

km.out <- kmeans(x, 3)
km.out
plot(x, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)

# Try 5 clusters with kmeans function and plot the clusters

km.out <- kmeans(x, 5)
km.out
plot(x, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)


# Plot original data and clustered data together #
par(mfcol = c(1, 2)) # create plotting structure of two plots in a row
plot(x, col = which, pch = 19) # plot observations with randomly assigned clusters
plot(x, col = centroid.label, pch = 19) # plot observations after clustering
dev.off() # quit the plotting structure settings


###### Part 4: K-medoids clustering ######

# install package "cluster"
library(cluster)
# generate a dissimilarity matrix for all the data points in data x, using the Euclidean distance. Hint: use the dist() function is most straight forward.
dismat <- dist(x, method = "euclidean") # 2 points
# run the pam() function with 3 clusters using the dismat generated above
med <- pam(dismat,3,diss=T) # 3 points
# extract the clusters for all data points and the medoids from the outputs of med.
clusters <- med$clustering # 1 point
medoids <- x[med$medoids,]  # 1 point
# plot the clustered data points, where color is decided by the clusters, and add the resulted medoids 
plot(x, col = clusters, pch = 19)   # 1 point
points(medoids, pch=19, col=6, cex=2)  # 1 point


###### Part 5: Hierarchical Clustering ######

# Calculate Inter-Observation Distances #
## If there are n observations, we need to calculate n*(n-1)/2 unique inter-observation distances. 
## It is because the distance between the same observation is zero, and distance between A and B is the same with the distance between B and A.  

# Euclidean distance function 1:
ed1 <- c()
for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(i < j){
      ed <- eu.distance(x[i,],x[j,]) # 3 points
      ed1 <- c(ed1, ed)
    }
  }
}

# Euclidean distance function 2:
ed2 <- dist(x, method = "euclidean") # the default method

# Plot Cluster Dendrogram #
# First take out a subset of the original observations. In this case, we randomly select 20 observations for the purpose of clear demonstration in the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance

# Function for hierarchical clustering
hc.complete <- hclust(y, method = "complete")
# Plot the dendrogram
plot(hc.complete)

# 4 points

# Try single with hclust function and plot the dendrogram

hc.single <- hclust(y,method="single")
plot(hc.single)

# Try average with hclust function and plot the dendrogram

hc.average <- hclust(y,method="average")
plot(hc.average)

########################################### END ##############################################