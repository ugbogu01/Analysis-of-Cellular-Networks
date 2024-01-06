library(igraph)
library(dequer)

# Task 1: Find triangles in a graph
get_triangles = function(g) {
  # Function that returns a matrix where each row 
  # corresponds to the nodes forming a triangle (3-cycle) in the graph g
  
  # Get vertex index/name sorted by degree
  Vtx <- order(degree(g), decreasing = TRUE)
  # Add the position in the ordered vertex list as an attribute
  V(g)$idx[Vtx] <- 1:length(V(g))
  # Initialize a list of admissible neighbors
  Adm <- vector(mode = "list", length = length(Vtx))
  # Initialize a list of triangles by vertex name
  triang <- matrix(0, nrow = 0, ncol = 3)
  # Going through the vertices by decreasing degree
  for (i in Vtx) {
    # For each neighboring vertex
    for (j in neighbors(g, i)) {
      # If vertex i is lower in the sorted vertex list
      if (V(g)$idx[i] < V(g)$idx[j]) {
        # Report any shared admissible node between i and j as a triangle
        for (k in intersect(Adm[[i]], Adm[[j]])) {
          triang <- rbind(triang, c(i, j, k))
        }
        # Add i to the admissible nodes of j
        Adm[[j]] <- c(Adm[[j]], i)
      }
    }
  }
  return(triang)
}

# Task 2: Count the number of 3-paths in a graph
get_npaths3 <- function(g) {
  # Get the adjacency matrix
  adj_m <- as_adjacency_matrix(g)
  # Take it to the power of 2
  # adj_m^2[i,j] gives you all walks of length 2 between i,j
  am_2 <- adj_m %*% adj_m
  # In the case of walks of length two, we only have to exclude the
  # diagonal to get all possible paths, since the only node that can be repeated
  # in walks of length 2 is the starting node
  # Summing over the upper left triangle without the diagonal:
  n_path3 <- 0
  for (i in 2:length(V(g))) {
    for (j in 1:(i-1)) {
      n_path3 <- n_path3 + am_2[i, j]
    }
  }
  return(n_path3)
}

# Task 3: Calculate transitivity of a graph
get_transitivity <- function(g) {
  trgls <- get_triangles(g)
  npaths3 <- get_npaths3(g)
  return(3 * nrow(trgls) / npaths3)
}

# Test on different graphs
get_transitivity(graph_from_atlas(500))
get_transitivity(sample_gnp(10, 0.3))
