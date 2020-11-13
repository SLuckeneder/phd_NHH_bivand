

# data
X <- cbind(1, matrix(rnorm(30), ncol = 3))
W <- matrix(0, 10L, 10L)
W[upper.tri(W)] <- rgamma(45L, 0.2, 0.2)
W <- 0.5 * (W + t(W)) # Make symmetric
W <- W / rowSums(W)

# parameters
beta <- c(-2, 1, 0.5, 0)
rho <- 0.3
theta <- c(0, 0.2, 0.3, 0)

# SAR
y <- solve(diag(10L) - rho * W) %*% (X %*% beta)

# SDM
y <- solve(diag(10L) - rho * W) %*% ((X %*% beta) + (X %*% theta))