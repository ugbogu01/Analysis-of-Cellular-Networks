# Load the igraph library
library(igraph)

# Define degree sequence and sort it
degr_seg <- c(3, 3, 3, 2, 2, 1)
at_seq <- sort(degree(at))

# Function to perform stub matching
stub_match <- function(dg_sq) {
  stubs <- rep(1:length(dg_sq), times = dg_sq)
  es <- sample(stubs, length(stubs))
  g <- graph(edges = es, directed = FALSE)
  return(g)
}

# Function to generate a simple graph using stub matching
simple_stub <- function(deg_seq, k) {
  stubs <- rep(1:length(deg_seq), times = deg_seq)
  iter <- 0
  issimple <- FALSE
  while (iter < k && !issimple) {
    edges <- sample(stubs, length(stubs))
    g <- graph(edges, directed = FALSE)
    A <- as.matrix(g[])
    iter <- iter + 1
    # Check if the graph is simple
    if (all(diag(A) == 0) && all(A <= 1)) {
      issimple <- TRUE
    } else if (iter == k) {
      stop('Maximum iterations reached without generating a simple graph')
    }
  }
  return(g)
}

# Function to perform edge switching
switch_rand <- function(g, k, n) {
  iter <- 0
  switch <- 0
  while (iter < k && switch < n) {
    es <- E(g)[sample(1:length(E(g)), 2)]
    edge_m <- ends(g, es)
    if (length(unique(as.vector(edge_m))) == 4) {
      if (!(are_adjacent(g, edge_m[1,1], edge_m[2,1]) || are_adjacent(g, edge_m[1,2], edge_m[2,2]))) {
        g <- delete.edges(g, es)
        g <- add.edges(g, c(edge_m[1,1], edge_m[2,1], edge_m[1,2], edge_m[2,2]))
        switch <- switch + 1
        iter <- 0            
      } else if (!(are_adjacent(g, edge_m[1,1], edge_m[2,2]) || are_adjacent(g, edge_m[2,1], edge_m[1,2]))) {
        g <- delete.edges(g, es)
        g <- add.edges(g, c(edge_m[1,1], edge_m[2,2], edge_m[2,1], edge_m[1,2]))
        switch <- switch + 1
        iter <- 0
      } else {
        iter <- iter + 1
      }
    } else {
      iter <- iter + 1
    }
  }
  if (iter == k) {
    stop(paste('Failed to generate edge switch with', k , 'tries'))
  }
  return(g) 
}

# Task 1: Create a Zachary graph
g1 <- graph('Zachary')

# Function to iteratively generate random graphs with the same degree sequence
iter_rand <- function(g, n, mode = 'switching') {
  res <- vector(mode = 'list', n)
  nc <- 2
  # Check connectedness (this is not mandatory, since in the lecture we did not fix the number of components as a network property)
  while (nc > 1) {
    if (mode == 'switching') {
      res[[1]] <- switch_rand(g, 50, 5)
    } else if (mode == 'stubmatch') {
      degseq <- sort(degree(g), decreasing = TRUE)
      res[[1]] <- simple_stub(degseq, 1000)
    } else {
      stop('Mode has to be either switching or stubmatch')
    }
    nc <- components(res[[1]])$no
  }
  for (i in 2:n) {
    nc <- 2
    # Check connectedness
    while (nc > 1) {
      if (mode == 'switching') {
        res[[i]] <- switch_rand(res[[i - 1]], 50, 5)
      } else if (mode == 'stubmatch'){
        res[[i]] <- simple_stub(degseq, 1000)
      }
      nc <- components(res[[i]])$no
    }
  }
  return(res)
}

# Function to test the significance of assortativity in the observed graph
test_assort <- function(obs_g, null_g) {
  x <- assortativity.degree(obs_g)
  null_dist <- sapply(null_g, assortativity.degree)
  plot(density(null_dist))
  # Assortativity is maximum/most extreme at 1; we want to test for assortativity
  pval <- (sum(null_dist >= x) + 1) / (length(null_dist) + 1)
  # One could also define 0 as being the least assortative and take the absolute assortativity to ask the question if the graph is significantly 
  # more assortative than a random one with the same properties; that's a matter of definition
  return(pval)
}

# Test significance using edge switching
p_switch <- test_assort(g1, iter_rand(g1, 100))

# Test significance using stub matching - Currently fails due to too many non-simple graphs implemented
p_stub <- test_assort(g1, iter_rand(g1, 100, mode = 'stubmatch'))

# Create an atlas graph
at <- graph_from_atlas(500)

# Test whether increasing the number of random graphs changes p (if we subsampled enough)
p_1000 <- test_assort(g1, iter_rand(g1, 1000))

# Function to test the significance of connecting two nodes of specified degrees
test_connect <- function(g, k1, k2, null) {
  k_connect <- function(g, k1, k2) {
    # Function to check if a random pair of nodes with degrees k1 and k2 are connected
    deg <- degree(g)
    # Check if there is more than one node with the specified degrees
    if (sum(deg %in% k1) >= (3 - length(unique(c(k1, k2)))) && sum(deg %in% k2) >= (3 - length(unique(c(k1, 2))))) {
      n1 <- sample(which(deg %in% k1), 1)
      n2 <- sample(setdiff(which(deg %in% k2), n1), 1)
      return(g[n1, n2] > 0)
    } else {
      stop('There is no node of the specified degree in the graph')
    }
  }

# Calculate the null distribution and p-value
null_dist <- sapply(null, k_connect, k1 = k1, k2 = k2)
pval <- (sum(null_dist) + 1) / (length(null_dist) + 1)

return(pval)
}

# Test if probability for a pair of nodes of degree 16, 1 to be connected
pba_16_1 <- test_connect(g1, 16, 2, iter_rand(g1, 100))

# Test if probability for a pair of nodes of degree 3, 1 to be connected
pba_3_1 <- test_connect(g1, 3, 1, iter_rand(g1, 100))