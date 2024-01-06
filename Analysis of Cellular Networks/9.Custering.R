# Load igraph library
library(igraph)

# Create Zachary's Karate Club graph
za = graph('Zachary')

# Function to find clusters in a graph based on specified diameter
qt_graph = function(g, Diam) {
  # For graphs, we use the diameter of the subgraph induced by the nodes in the cluster
  points = 1:length(V(g))
  allclusters = list()
  
  while (length(points) > 1) {
    cluster = c()
    
    # Iterate over points that are not in a cluster yet
    for (each_point in points) {
      each_point_cluster = c(each_point)
      Flag = TRUE
      
      # Generate clusters using each point as the center, stopping if the diameter exceeds the specified value
      while (Flag == TRUE & length(each_point_cluster) != length(points)) {
        diameter = c()
        
        for (candidate in setdiff(points, each_point_cluster)) {
          test = c(candidate, each_point_cluster)
          
          # Check diameter
          diameter = c(diameter, max(as.vector(shortest.paths(induced.subgraph(g, test)))))
          names(diameter)[length(diameter)] = candidate
        }
        
        print(min(diameter))
        
        if (min(diameter) > Diam) {
          Flag = FALSE
        } else {
          each_point_cluster = c(each_point_cluster, names(diameter)[which.min(diameter)])
        }
      }
      
      # Check if the cluster is larger than the saved cluster; if so, update the saved cluster
      if (length(cluster) < length(each_point_cluster)) {
        cluster = as.numeric(each_point_cluster)
      }
    }
    
    allclusters = append(allclusters, list(cluster))
    points = setdiff(points, cluster)
  }
  
  if (length(points) == 1) {
    allclusters = append(allclusters, list(points))
  }
  
  return(allclusters)
}

# Function to plot clusters in a graph
plot_clg = function(g, cl) {
  for (i in 1:length(cl)) {
    V(g)$color[cl[[i]]] = i
  }
  
  plot(g)
}

# Find clusters with a diameter of 2 in Zachary's Karate Club graph
cl = qt_graph(za, 2)

# Plot the clusters
plot_clg(za, cl)
