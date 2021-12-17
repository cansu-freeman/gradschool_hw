# Functions to get values for NN function later
get_beta_0 = function(alpha, i, m=4) {
  return (alpha[3*i-2]) }

get_beta = function(alpha, i, m=4) {
  return (c(alpha[3*i-1], alpha[3*i])) }

get_gamma_0 = function(alpha, i, m=4) {
  return (alpha[(m+1)*(i-1) + 1 + m*3]) }

get_gamma = function(alpha, i, m=4) {
  i_1 = (m+1)*(i-1) + 2 + m*3
  i_n = i_1 + (m-1)
  return (alpha[i_1:i_n]) }

sigma = function(Z) {
  return (1/(1 + exp(-Z))) }
  
  
# Now for the NN Function:
NN = function(alpha, x, m=4) {
  #initializing Z's
  Z = rep(0, 4)
  
  for (i in 1:m) {
    beta_0 = get_beta_0(alpha, i, m)
    beta = get_beta(alpha, i, m)
    Z[i] = sigma(beta_0 + sum(beta*x)) }
  
  # initializing T's
  T = rep(0, 2)
  
  for (i in 1:2) {
    gamma_0 = get_gamma_0(alpha, i, m)
    gamma = get_gamma(alpha, i, m)
    T[i] = sigma(gamma_0 + sum(gamma*Z)) }
  
  # P(y=1):
  p_1 = exp(T[1])/(exp(T[1]) + exp(T[2]))

  return (p_1)
}

# Log Likelighood and Gradient of Log Likelihood Functions
logL = function(alpha, X, y, m=4) {
  total = 0
  nsamp = length(y)
  
  for (i in 1:nsamp) {
    p = NN(alpha, X[i,], m)
    total = total + y[i]*log(p+10^-5) + (1-y[i])*log(1-p+10^-5) 
    #using 10^-5 to avoid getting a zero for p
  }
  return (total)
}

g_logL = function(alpha, X, y, m=4) {
  grad = rep(0, 22)
  og = logL(alpha, X, y, m)
  
  for (i in 1:22) {
    alpha_new = alpha
    alpha_new[i] = alpha_new[i] + 10^-7 # small step size
    grad[i] = logL(alpha_new, X, y, m) - og #difference
    grad[i] = grad[i]/(10^-7)
  }
  return (grad)
}


# Training Function
max_LL_NN = function(a, X, y, step_size = .01){
  
  alpha = a
  
  # About 1 million iterations, showing output by chunks
  for (i in 1:500) {
      if (i %% 100 == 0 | i == 1) {
        logL = logL(alpha, X, y)
        cat(i, " logL = ", logL, "\n")
      }
  
    for (i in 1:nrow(X)){
      direction = grad_logL(a, X[i,,drop=F], y[i])
      a = a + direction*step_size
    }
  }
  
  return(a)
}


# ten iterations as recommended
for (i in 1:10) {
  alpha = runif(22, min=-0.1, max=0.1) #random starting alpha each time
  result_a = max_LL_NN(alpha, X, y) #apply training - goal: maximizing
  a_matrix[i,] = result_a #creating matrix of all alphas
  likelihood[i] = logL(result_a, X, y) #applying logL to new alphas
  print(likelihood[i]) #print along the way for sanity check
} 


# Function to show the fit
fit = function(p, alpha) {
  n = 10000
  
  # Creating X
  x1 = 4*runif(n) - 2
  x2 = 4*runif(n) - 2
  X = cbind(x1, x2) 
  
  y_pred = ifelse(apply(X, 1, function(x) NN(alpha, x)) > p,1, 0)
  plot(x1, x2, col=y_pred+2, xlab="x1", ylab="x2", xlim=c(-2,2), ylim=c(-2,2), cex=.5)
}


# saved likelihoods and a_matrix from fn above as a_1, a_2, lh_1 and lh_2 
a_1_best = a_1[which.max(lh_1),] # grabs the max of the likelihoods
for (p in c(.25, .3, .4, .8)){   # trying out different probabilities
  fit(p, a_best)
} 

a_3_best = a_2[which.max(lh_2),]
for (p in c(.25, .3, .28, .8)){
  fit(p, a_3_best)
}

