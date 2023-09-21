# Example Script --------------------------------------------------------

library(MASS)
library(ivreg)

rho <- 1
bigN <- 100
beta_1 <- 1
gamma <- 1

# Simulation Functions --------------------------------------------------

sim_data <- function(bigN, beta_1 = 1, gamma, rho) {
  # simulate errors
  e_Sigma <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
  e <- mvrnorm(
    n = bigN, m = c(0, 0), Sigma = e_Sigma
  )
  e1 <- e[, 1]
  e2 <- e[, 2]
  # simulate z
  z <- rnorm(bigN)
  # simulate x
  x <- z*gamma + e2
  # simulate y
  y <- x*beta_1 + e1
  # return
  ret_list <- data.frame(
    y = y, 
    z = z, 
    x = x
  )
  return(ret_list)
}

# Set up for loop for replications
bigR <- 1000
data_results <- data.frame(
  replication = 1:bigR, 
  beta_hat = NA
)

for (r in 1:bigR) {
  set.seed(r)
  # simulate data
  data <- sim_data(bigN, beta_1, gamma, rho)
  # 2sls estimation
  model <- AER::ivreg(y ~ -1 + x | -1 + z, data = data)
  # save coefficient
  data_results$beta_hat[r] <- model$coefficients
}

data_results <- data_results |>
  cbind(
    data.frame(
    bigN = bigN, 
    rho = rho, 
    gamma = gamma)
  )

# On a local machine, we would iterate through the entire dgp_spec_grid
# in another for loop...
