library(igraph)

# Create Zachary's Karate Club graph
za = graph('Zachary')

# Function to calculate modularity of a clustering
my_mod = function(g, cl) {
  cl_mod = function(idx, g) {
    return((length(E(induced.subgraph(g, idx))) / length(E(g))) -
             (sum(degree(g)[idx]) / (2 * length(E(g))))^2)
  }
  all_mod = sapply(cl, cl_mod, g = g)
  return(sum(all_mod))
}

# Function to plot clusters in a graph
plot_cl = function(g, cl) {
  V(g)$color = rep(0, length(V(g)))
  for (i in 1:length(cl)) {
    V(g)$color[cl[[i]]] = i
  }
  plot(g)
}

# Function for divisive spectral clustering using modularity as the quality score
sc = function(g) {
  # Get matrix
  # Divisive clustering --> start with all nodes in one cluster
  V(g)$idx = 1:length(V(g))
  cluster = list(c(1:length(V(g))))
  
  # Save current modularity
  max_mod = my_mod(g, cluster)
  break_flag = TRUE
  no_touch = c()
  
  # While there are divisible clusters
  while (length(setdiff(1:length(cluster), no_touch)) > 0) {
    for (i in setdiff(1:length(cluster), no_touch)) {
      isubg = induced.subgraph(g, cluster[[i]])
      L = as.matrix(isubg[]) * -1
      diag(L) = degree(isubg)
      ev = eigen(L)
      
      if (length(unique(ev$values[ev$values > 10^-9])) == 1 && length(unique(ev$values[ev$values > 10^-9])) < sum(ev$values > 10^-9) || 
          sum(ev$values > 10^-9) == 0) {
        no_touch = c(no_touch, i)  # Add cluster to indivisible clusters
      } else {
        vec = ev$vectors[, sum(ev$values > 10^-9)]
        
        if (all(vec < 0) || all(vec > 0) || any(vec == 0)) {
          no_touch = c(no_touch, i)  # Add cluster to indivisible clusters
        } else {
          # If eigenvector is splittable 
          # Replace first half of split
          cluster[[i]] = V(isubg)$idx[which(vec < 0)]
          # Add second half of split
          cluster = c(cluster, list(V(isubg)$idx[which(vec > 0)]))
          
          # Check modularity and if new maximum reached, update and return clustering
          print(my_mod(g, cluster))
          if (my_mod(g, cluster) > max_mod) {
            max_mod = my_mod(g, cluster)
            opt_clus = cluster
          }
        }
      }
    }
    
    plot_cl(g, cluster)
    readline()
  }
  
  return(opt_clus)
}
