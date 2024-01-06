library(dequer)
library(igraph)

# Create an undirected graph from the atlas with 500 nodes
at <- graph_from_atlas(500)

# Task 1: Write a function that uses BFS to return the shortest path to each node from node v
visualize_ep <- function(g, edges_vector, p_length) {
  ei <- get.edge.ids(g, edges_vector)
  E(g)$color <- "black"
  E(g)[ei]$color <- "orange"
  V(g)$color <- p_length + 1
  V(g)$color[is.na(V(g)$color)] <- 0
  plot(g)
}

bfs_sp <- function(g, v) {
  edges <- c()
  q <- queue()
  num_nodes <- length(V(g))
  mark <- rep(0, num_nodes)
  # Initialize a vector of path length
  p_length <- rep(NA, num_nodes)
  mark[v] <- 1
  # Set path length to start node to zero
  p_length[v] <- 0
  pushback(q, v)
  while (length(q) != 0) {
    u <- pop(q)
    for (w in neighbors(g, u)) {
      if (mark[w] != 1) {
        mark[w] <- 1
        # Upon first encounter, set path length to active node's path length + 1
        p_length[w] <- p_length[u] + 1
        pushback(q, w)
        edges <- c(edges, u, w)
        ##For visualization of the algorithm traversal uncomment
        # visualize_ep(g, edges, p_length)
        # readline()
      }
    }
  }
  if (any(is.na(p_length))) {
    warning('Graph has multiple components')
  }
  return(p_length)
}

# Test the bfs_sp function
bfs_sp(at, 1)

# Task 2: Determine and mark the center of the graph
mark_center = function(g) {
  # Write a subfunction that determines the eccentricity from the output of bfs_sp
  my_eccentricity <- function(v, g) {
    return(max(bfs_sp(g, v)))
  }
  # Determine eccentricity of each node
  ecc <- sapply(V(g), my_eccentricity, g = g)
  # Compare with igraph function
  if (any(ecc - eccentricity(g) != 0)) {
    stop('function my_eccentricity returns erroneous output!')
  }
  # Mark the center nodes
  V(g)$color <- 'yellow'
  V(g)$color[ecc == min(ecc)] <- 'red'
  plot(g)
}

# Test the mark_center function
mark_center(at)
