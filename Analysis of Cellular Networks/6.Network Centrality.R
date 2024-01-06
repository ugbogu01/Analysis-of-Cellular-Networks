library(igraph)

# Initialize an atlas graph with 500 nodes
at <- graph_from_atlas(500)

# Initialize a Barabasi graph with the same number of nodes
ba <- sample_pa(7, m = 2, directed = FALSE)

# Initialize an Erdos Renyi graph with the same number of nodes and edges
er <- sample_gnm(7, length(E(ba)), directed = FALSE)

# Get the eigenvalues and eigenvectors of the adjacency matrices
graphs <- list(at, ba, er)
ev <- lapply(graphs, function(x) eigen(as.matrix(x[])))

# Function to calculate Bonacich centrality
c_bonac <- function(g, beta) {
  A <- as.matrix(g[])
  I <- diag(1, ncol(A))
  raw_bon <- solve(I - beta * A) %*% A %*% rep(1, ncol(A))
  alpha <- 1 / (sqrt(sum(raw_bon^2)) * ncol(A))
  f_bon <- alpha * raw_bon
  return(f_bon)
}

# Calculate c_bonac with beta = 0.7
bonac_b7 <- sapply(graphs, c_bonac, beta = 0.7)

# Calculate c_bonac with beta = 1/lambda
bonac_rlamb <- matrix(nrow = ncol(as_adjacency_matrix(graphs[[1]])), ncol = length(graphs))
for (i in 1:length(graphs)) {
  bonac_rlamb[, i] <- c_bonac(graphs[[i]], beta = 1 / ev[[i]]$values[1] - 0.000001)
}

# Check correlation
b7_cor <- numeric(length(graphs))
rlamb_cor <- numeric(length(graphs))
for (i in 1:length(graphs)) {
  b7_cor[i] <- cor(x = ev[[i]]$vectors[, 1], y = bonac_b7[, i])
  rlamb_cor[i] <- cor(x = ev[[i]]$vectors[, 1], y = bonac_rlamb[, i])
}

# Report
names(b7_cor) <- c('at', 'ba', 'er')
names(bonac_rlamb) <- names(b7_cor)
print('Correlation between eigenvalue and Bonacich centrality with beta = 0.7')
print(b7_cor)
print('Correlation between eigenvalue and Bonacich centrality with beta = 1/lambda')
print(rlamb_cor)

# Task 2
# Source transitivity function from homework 4
source('Homework4.R')

check_ertrans <- function(g) {
  # Function that checks if the transitivity of graph g is higher than the expected transitivity
  # of an Erdos-Renyi graph with the same number of edges and nodes
  # INPUT:
  # - igraph g: an igraph graph object for which transitivity is checked
  
  # Check transitivity of g
  g_trans <- get_transitivity(g)
  
  # Create 100 Erdos Renyi graphs with the same number of edges and nodes
  set.seed(2022)
  er_graphs <- lapply(rep(length(V(g)), 100), sample_gnm, m = length(E(g)))
  
  # Get transitivities
  er_trans <- sapply(er_graphs, get_transitivity)
  
  if (g_trans > mean(er_trans)) {
    print('Network has higher transitivity than the average comparable Erdos-Renyi network.')
  } else {
    print('Network does not have higher transitivity than the average comparable Erdos-Renyi network.')
  }
  print(paste('Transitivity of g:', g_trans))
  print('Summary statistics for 100 comparable Erdos-Renyi networks')
  print(summary(er_trans))
}

# Check for the three previous networks
check_ertrans(at)
check_ertrans(ba)
check_ertrans(er)
