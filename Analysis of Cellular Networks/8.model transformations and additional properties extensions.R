library(igraph)
library(utils)
library(dequer)

# Function to create complement graph and test correlation between node degrees
# Also plots the degree distribution of the base graph and complement graph
check_compdeg = function(g) {
  f = make_full_graph(length(V(g)), directed = FALSE)
  c = graph.difference(f, g)
  
  # Print Spearman correlation of degree centralities
  print("Spearman correlation of degree centralities")
  print(cor(x = degree(g), y = degree(c), method = "spearman"))
  
  # Plot degree distribution of the base graph
  par(mfrow = c(1, 2))
  plot(density(degree(g)), main = "Degree Distribution - Base Graph")
  
  # Plot degree distribution of the complement graph
  plot(density(degree(c)), main = "Degree Distribution - Complement Graph")
  readline()  # Press enter to see the next plot
}

# Test for 4 different Erdos-Renyi graphs
er = lapply(seq(0.2, 0.8, 0.2), function(x) { return(sample_gnp(15, x)) })
lapply(er, check_compdeg) 
# Degree centralities are perfectly negatively correlated, and the degree distribution is symmetric.

get_isubg = function(g, v) {
  g_elist = as_edgelist(g)
  V(g)$idx = 1:length(V(g))
  eidx = which(g_elist[, 1] %in% v & g_elist[, 2] %in% v)
  isubg = delete.vertices(g, V(g)[!(V(g) %in% v)])
  return(list(isubg, eidx))
}

get_ip4 = function(g) {
  # Function that gets half of all P4 paths that fulfill the condition in task 2
  V(g)$idx = 1:length(V(g))
  res = matrix(nrow = 0, ncol = 4)
  
  # Get all possible combinations of vertices
  cb = combn(length(V(g)), 4)
  
  for (i in 1:ncol(cb)) {
    isg = get_isubg(g, cb[, i])
    
    # For each combination, create the induced subgraph
    if (is.connected(isg[[1]])) {
      if (identical(sort(degree(isg[[1]])), c(1, 1, 2, 2))) {
        res = rbind(res, V(isg[[1]])$idx)
      }
    }
  }
  
  return(res)
}

# Create Erdos-Renyi and Barabasi graphs with the same number of edges and nodes varying Barabasi m
ba1 = lapply(1:3, function(x) { return(sample_pa(15, m = x, directed = FALSE)) })
m = sapply(ba1, function(x) { return(length(E(x))) })
er1 = lapply(m, function(x) { return(sample_gnm(15, x)) })

# Count P4 induced subgraphs
n_p4_ba = sapply(ba1, function(g) { return(2 * nrow(get_ip4(g))) })
n_p4_er = sapply(er1, function(g) { return(2 * nrow(get_ip4(g))) })

# For m > 1 in the Barabasi generation process, Erdos-Renyi networks with the same amount of edges and nodes have a higher number of P4 induced subgraphs

# Check for m1 for multiple combinations
ba2 = lapply(rep(15, 20), sample_pa)
er2 = lapply(rep(length(E(ba2[[1]])), 20), function(x) { return(sample_gnm(15, x)) })

# Count P4 induced subgraphs
n_p4_ba2 = sapply(ba2, function(g) { return(2 * nrow(get_ip4(g))) })
n_p4_er2 = sapply(er2, function(g) { return(2 * nrow(get_ip4(g))) })

# Perform Wilcoxon test to compare the number of P4 paths in Barabasi and Erdos-Renyi graphs
wilcox_test_result = wilcox.test(n_p4_ba2, n_p4_er2)
print(wilcox_test_result)
# There are significantly fewer P4 paths in Erdos-Renyi graphs than in Barabasi graphs with 15 nodes and 1 edge added at each step
