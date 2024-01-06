# Load igraph package
library(igraph)

# Create an undirected graph
at <- graph_from_atlas(75)
# Add an edge to the graph for fun
at <- add_edges(at, c(2, 2))

# Function that returns the degrees of all nodes in an undirected graph simultaneously
# INPUT:
# - igraph gr: the graph to calculate degrees on all vertices
# OUTPUT:
# - named numeric: A named vector giving the degree of each node
#   (Column sums of the adjacency matrix)
my_degree <- function(gr) {
  # Get the adjacency matrix
  gr_am <- as.matrix(as_adjacency_matrix(gr))
  # Double the diagonal (loops increase degree by 2)
  diag(gr_am) <- 2 * diag(gr_am)
  # Add vertex names to the matrix
  colnames(gr_am) <- V(gr)
  # Calculate node degree
  # Since this is undirected, we don't have to worry about negative entries in the adjacency matrix
  return(colSums(gr_am))
}

# Test the my_degree function
my_degree(at)
# Compare with the degree from the igraph package
degree(at)

# Function that returns all vertices connected to v
# INPUT:
# - igraph gr: the graph to analyze
# - v: the name of the vertex for which neighbors should be extracted
# OUTPUT:
# - An array of vertices which are the direct neighbors of v
my_neighbors <- function(gr, v) {
  # Get the adjacency matrix
  gr_am <- as.matrix(as_adjacency_matrix(gr))
  return(V(gr)[as.logical(gr_am[V(gr) %in% v])])
}

# Test the my_neighbors function
my_neighbors(at, 2)
# Compare with the igraph implementation
neighbors(at, 2)

# Function to recursively extract the n-neighborhood of a given vertex
# INPUT:
# - v: an array of vertex names of the vertices for which neighbors should be extracted
# - igraph gr: the graph to analyze
# - numeric n: the depth of the neighborhood to extract
# OUTPUT:
# A array of vertices in the n-neighborhood of the vertex
my_neighborhood <- function(v, gr, n) {
  if (n == 0) {
    # If depth == 0, return the input vertices
    return(v)
  } else {
    # Otherwise, call my_neighbors again with the unique neighbors of x and n-1
    return(my_neighborhood(unique(c(v, unlist(sapply(v, function(x) my_neighbors(gr, x))))), gr, n - 1))
  }
}

# To extract the 2nd neighborhood
my_neighborhood(4, at, 2)
# Compare with the igraph implementation
ego(at, 2, 4)

# Function to find the most similar pair of nodes in a graph
# If multiple node pairs have maximum similarity, it returns the first one in the order of edges
# INPUT:
# - igraph gr: graph to find the most similar pair of nodes in
# OUTPUT:
# A vector containing the two most similar nodes
my_simpair <- function(gr) {
  # Function to calculate similarity between two nodes
  nodesim <- function(v1, v2, gr) {
    v1_nbh <- my_neighbors(gr, v1)
    v2_nbh <- my_neighbors(gr, v2)
    # For the case that both nodes do not have neighbors, the similarity is set to 0
    if (length(v1_nbh) == 0 && length(v2_nbh) == 0) {
      return(0)
    } else {
      return(length(intersect(v1_nbh, v2_nbh)) / length(union(v1_nbh, v2_nbh)))
    }
  }
  
  # Populate the upper triangle of the similarity matrix
  sim_mat <- matrix(0, length(V(gr)), length(V(gr)))
  for (i in 1:length(V(gr))) {
    for (j in 1:i) {
      sim_mat[i, j] <- nodesim(V(gr)[i], V(gr)[j], gr)
    }
  }
  
  # Set diagonal to 0 to avoid trivial solution
  diag(sim_mat) <- 0
  # Take the first maximum similarity pair
  max_idx <- which(sim_mat == max(sim_mat), arr.ind = TRUE)[1, ]
  return(c(V(gr)[max_idx[1]], V(gr)[max_idx[2]]))
}

# Test the my_simpair function
my_simpair(at)
