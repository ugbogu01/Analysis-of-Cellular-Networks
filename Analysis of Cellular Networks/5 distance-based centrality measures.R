library(igraph)

# Function to find all paths from a vertex u in a graph g
paths <- function(g, u) {
  count = 0
  visited = c(u)
  dist <- rep(1000000, length(V(g)))
  path <- rep(0, length(V(g)))
  dist[u] <- 0 
  path[u] <- 1
  while (!(length(intersect(V(g), visited)) == length(V(g)))) {
    curr <- which(dist == count)
    for (i in curr) {
      nei_i = setdiff(neighbors(g, i), visited)
      for (j in nei_i) {
        if (dist[j] > dist[i] + 1) {
          dist[j] = dist[i] + 1 
          path[j] = path[i]
        } else if (dist[j] == dist[i] + 1) {
          path[j] = path[j] + path[i]
        }
      }
    }
    visited <- union(visited, curr)
    count = count + 1
  }
  return(list(path, dist))
}

# Function to find all pairs shortest paths in a graph
all_pairs_dp <- function(g) {
  D = matrix(data = NA, nrow = length(V(g)), ncol = length(V(g)))
  P = matrix(data = NA, nrow = length(V(g)), ncol = length(V(g)))
  for (i in V(g)) {
    tmp_res = paths(g, i)
    P[i, ] = tmp_res[[1]]
    D[i, ] = tmp_res[[2]]
  }
  return(list(D, P))
}

# Function to calculate shortest paths from u to v through w
sp_viak <- function(D, P, u, v, w) {
  if (D[u, v] == (D[u, w] + D[w, v])) {
    sol = P[u, w] * P[w, v]
  } else {
    sol = 0
  }
  return(sol)
}

# Task 1: Function to get the center and periphery of a graph
get_centper <- function(g) {
  # Get shortest paths using all_pairs_dp function
  D = all_pairs_dp(g)[[1]]
  # Calculate node eccentricity
  ecc = apply(D, 2, max)
  # Initialize an array marking node in the center ('C'), periphery ('P'), or in-between ('B')
  node_mark = rep('B', length(ecc))
  # The center is comprised of all nodes with eccentricity equal to graph radius
  node_mark[ecc == min(ecc)] = 'C'
  # The periphery is comprised of all nodes with eccentricity equal to graph diameter
  node_mark[ecc == max(ecc)] = 'P'
  V(g)$color = "orange"
  V(g)$color[node_mark %in% 'C'] = "green"
  V(g)$color[node_mark %in% 'P'] = "red"
  plot(g)
  return(node_mark)
}

# Test on atlas graph
get_centper(graph_from_atlas(500))

# There are graphs where periphery and center coincide
# Fully connected graphs are one example of graphs with this property
fg = make_full_graph(7)
get_centper(fg)

# Task 2: Function to analyze correlation between eccentricity and number of shortest paths
all_shortestp_via = function(g) {
  if (components(g)[[3]] != 1) {
    stop('Input is a disconnected graph, eccentricity analysis aborted')
  }
  bfs_res = all_pairs_dp(g)
  D = bfs_res[[1]]
  P = bfs_res[[2]]
  n_nodes = length(V(g))
  n_spths = rep(0, length(V(g)))
  
  # For each node, calculate the number of shortest paths through it
  for (i in 1:n_nodes) {
    for (j in setdiff(1:(n_nodes - 1), i)) {
      for (k in setdiff((j + 1):n_nodes, i)) {
        n_spths[i] = n_spths[i] + sp_viak(D, P, j, k, i)
      }
    }
    # Since we use undirected graphs, multiply final result by two to get all paths
    n_spths[i] = n_spths[i] * 2
  }
  
  # Calculate node eccentricity
  ecc = apply(D, 2, max)
  
  # Analyze correlation between eccentricity and numbers of shortest paths
  testres = cor.test(x = ecc, y = n_spths, method = 'spearman')
  
  # Plot the correlation
  plot(x = ecc, y = n_spths, xlab = 'Vertex Eccentricity', 
       ylab = 'Number of Shortest Paths', 
       main = paste('Spearman correlation:', signif(testres[[4]], 2), 
                    'p-value:', signif(testres[[3]], 2)))
  readline()
  return(testres[[4]])
}

# Analyze correlation for Barabasi graphs generated with m = 4:6 - press enter to see plot for next m
lapply(ba, all_shortestp_via)
# Analyze correlation for Erdos Renyi graphs with equal amount of edges and nodes
lapply(er, all_shortestp_via)
