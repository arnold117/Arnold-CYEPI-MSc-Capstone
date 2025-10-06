# medoid function
find_medoid <- function(points) {
  if (nrow(points) == 1) {
    return(as.numeric(points))
  }

  # Calculate distance matrix
  dist_matrix <- as.matrix(dist(points, method = "euclidean"))

  # For each point, calculate the sum of distances to all other points
  total_distances <- rowSums(dist_matrix)

  # Return the point with minimum total distance
  return(as.numeric(points[which.min(total_distances), ]))
}