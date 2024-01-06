library(igraph)

# Create an Atlas graph and Zachary's Karate Club graph
at = graph_from_atlas(500)
za = graph('Zachary')

# Function to create a line graph from a given graph
line_graph <- function(g) {
  ec <- length(E(g))
  adj <- matrix(0, ec, ec)
  node_ends <- ends(g, E(g))
  
  # Build adjacency matrix for the line graph
  for (i in 1:(ec - 1)) {
    for (j in (i + 1):ec) {
      s <- intersect(node_ends[i,], node_ends[j,])
      if (length(s) > 0) {
        adj[i, j] <- adj[j, i] <- 1
      }
    }
  }
  
  # Create the line graph
  l <- graph_from_adjacency_matrix(adj, mode = "undirected")
  
  # Add edge ends as vertex attributes
  V(l)$n_ends = vector(mode = "list", length = nrow(node_ends))
  for (i in 1:nrow(node_ends)) {
    V(l)$n_ends[[i]] = node_ends[i,]
  }
  
  return(l)
}

# Function for QT clustering
qt_graph = function(g, Diam) {
  points = 1:length(V(g))
  allclusters = list()
  
  while (length(points) > 1) {
    cluster = c()
    
    # Iterate over points that are not in a cluster yet
    for (each_point in points) {
      each_point_cluster = c(each_point)
      Flag = TRUE
      
      while (Flag == TRUE & length(each_point_cluster) != length(points)) {
        diameter = c()
        
        for (candidate in setdiff(points, each_point_cluster)) {
          test = c(candidate, each_point_cluster)
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
      
      # Check if the cluster is larger than the saved cluster, if so, update the saved cluster
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

# Function to recover clustering from the line graph clustering
recover_lcl = function(l, cl) {
  cl_vect = character(length(V(l)))
  
  # Convert a clustering of a line graph to the original graph
  for (i in 1:length(V(l))) {
    cl_vect[i] = LETTERS[which(sapply(cl, function(clust, x) {return(x %in% clust)}, x = i))]
  }
  
  # Recover old graph from nodes edge ends attribute
  g = graph(as.vector(unlist(V(l)$n_ends)), directed = FALSE)
  
  # Convert to matrix
  n_ends = do.call(rbind, V(l)$n_ends)
  
  # Add a label according to line graph cluster membership
  V(g)$label = character(length(V(g)))
  
  for (i in 1:length(V(g))) {
    V(g)$label[i] = paste(unique(cl_vect[rowSums(n_ends == i) > 0]), sep = '', collapse = '')
  }
  
  return(g)
}

# Function to perform fuzzy clustering using the line graph of the original graph in QT clustering
line_qt_graph = function(g, Diam) {
  l = line_graph(g)
  cl = qt_graph(l, Diam)
  cg = recover_lcl(l, cl)
  return(cg)
}

# Apply the line_qt_graph function to Zachary's Karate Club graph
zacl = line_qt_graph(za, 1)
zacl
