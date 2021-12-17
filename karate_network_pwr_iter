# Reading and Plotting Data 
A = read.table("karate club.txt") %>% as.matrix()
g = graph_from_adjacency_matrix(A, mode = 'undirected', weighted = NULL, diag = FALSE)
plot.igraph(g, vertex.label=NA, vertex.size = 10)


# Function
power_iteration = function(network) {
  i = 1
  
  # reading network as adjacency matrix A
  A = as_adjacency_matrix(network, sparse = FALSE)
  
  # setting up B matrix using k_i*k_j/sum(A_ij)
  B = matrix(0, nrow = nrow(A), ncol = ncol(A))
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      B[i,j] = A[i, j] - (rowSums(A)[i] * colSums(A)[j] / sum(A))
    }
  }
  
  # setting up s to give us the first two eigen vectors
  s = matrix(data = 1, ncol= 2, nrow = nrow(A))

  # orthogonalized power iteration
  for (i in 1:1000) {
    prev_s = s
    s = t(B) %*% s
    s= qr.Q(qr(s)) 
    
    i = i + 1
  }
  
  # Q = eigenvalue; we know the first is negative from lots of trial & error 
  # We want to obtain the 2nd (if it were negative then we'd want the 3rd, etc)
  Q = matrix(data = NA, nrow=2, ncol=2)
  Q[,1] = t(s[,1]) %*% t(B) %*% s[,1]/sqrt(sum(s[,1]^2))
  Q[,2] = t(s[,2]) %*% t(B) %*% s[,2]/sqrt(sum(s[,2]^2))

  list(s=s, beta1=Q[1,1], beta2=Q[1,2], iterations = i)
}

# Run power iteration on our network
power_iteration(g)



# Saving only the second eigenvector since it's eigenvalue 
# is the largest positive
eigen_B = power_iteration(g)$s[,2]

# Setting up grouping vector
grouping = vector(length = 34)

# Separating groupings by positive/negative eigenvector values
for (i in 1:34){
  if (eigen_B[i] > 0){
    grouping[i] = 2
  } else {
    grouping[i] = 1
  }
}

# Plotting  with colors
plot.igraph(g, vertex.color = grouping, vertex.label=NA, vertex.size = 10)
