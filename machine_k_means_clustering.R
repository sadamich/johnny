https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Clustering-Example/index.html

### k means Clustering


library(ggplot2)

plot_clusters <- function(data, clusters) {
  ggplot(data, aes(x = x, y = y, color = factor(clusters))) +
    geom_point(size = 3) +
    labs(title = "k-means clustering",
         x = "Feature 1",
         y = "Feature 2") +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) 
}


set.seed(0)
data <- data.frame(x = rnorm(35), y = rnorm(35))
plot_clusters(data, rep(1, nrow(data)))


library(stats)

k <- 4  # Number of clusters
kmeans_result <- kmeans(data, centers = k)

plot_clusters(data, kmeans_result$cluster)


plot_clusters(data, rep(1, nrow(data)))

k <- 4

k_means_clustering <- function(data, k) {
  }


k_means_clustering <- function(data, k) {
  
  # select k centroids from the initial points
  set.seed(1)
  centroids <- data[sample(nrow(data), k), ] 
  
  return(centroids)
}


### Plot ###
centroids <- k_means_clustering(data, k)
plot_clusters(data, rep(1, nrow(data))) +
  geom_point(data = centroids, aes(x = centroids[, 1], y = centroids[, 2]), size = 6, color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), fill = NA, shape = 9) 


k_means_clustering <- function(data, k) {
  
  # select k centroids from the initial points
  set.seed(1)
  centroids <- data[sample(nrow(data), k), ] # shuffle and select k points
  
  # compute distances (Euclidean norm)
  distances <- matrix(0, nrow = nrow(data), ncol = nrow(centroids))
  for (i in 1:nrow(data)) {
    for (j in 1:nrow(centroids)) {
      distances[i, j] <- sqrt(sum((data[i, ] - centroids[j, ])^2)) # Euclidean norm
    }
  }
  
  # assign data to centroids
  cluster_assignments <- apply(distances, 1, which.min)
  
  return(list(clusters = cluster_assignments, centroids = centroids))
}


### Plot ###
result <- k_means_clustering(data, 4)
plot_clusters(data, result$clusters) +
  geom_point(data = result$centroids, aes(x = x, y = y), size = 6, color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), fill = NA, shape = 9)


k_means_clustering <- function(data, k) {
  
  # select k centroids from the initial points
  set.seed(1)
  centroids <- data[sample(nrow(data), k), ] # shuffle and select k points
  
  # compute distances (Euclidean norm)
  distances <- matrix(0, nrow = nrow(data), ncol = nrow(centroids))
  for (i in 1:nrow(data)) {
    for (j in 1:nrow(centroids)) {
      distances[i, j] <- sqrt(sum((data[i, ] - centroids[j, ])^2))
    }
  }
  # assign data to centroids
  cluster_assignments <- apply(distances, 1, which.min)
  
  # update centroids based on mean of the assigned data points
    new_centroids <- data.frame(matrix(0, nrow = k, ncol = ncol(data)))
    colnames(new_centroids) <- colnames(data)
    for (i in 1:k) {
      cluster_points <- data[cluster_assignments == i, ]
      if (nrow(cluster_points) > 0) {
        new_centroids[i, ] <- colMeans(cluster_points)
      }
    }
  return(list(clusters = cluster_assignments, centroids = new_centroids))
}

### Plot ###
result <- k_means_clustering(data, 4)
plot_clusters(data, result$clusters) +
  geom_point(data = result$centroids, aes(x = x, y = y), size = 6, color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), fill = NA, shape = 9) 
### Plot ###
result <- k_means_clustering(data, 4)
plot_clusters(data, result$clusters) +
  geom_point(data = result$centroids, aes(x = x, y = y), size = 6, color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), fill = NA, shape = 9) 




