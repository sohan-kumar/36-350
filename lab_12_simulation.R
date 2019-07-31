generate_data <- function(n, p) {
  cov <- rnorm(n * p)
  row <- sample(1:p, 1)
  cov_mat <- matrix(cov, nrow = n, ncol = p)
  return(list(covariates = cov_mat, responses = cov_mat[,row]))
}

model_select <- function(covariates, responses, cutoff) {
  reg <- lm(responses ~ covariates)
  return(lm(responses ~ covariates[,reg$coefficients < cutoff]))
}

run_simulation <- function(n_trials, n, p, cutoff) {
  mat <- generate_data(n, p)
  results <- vector()
  for (i in 1:n_trials) {
    trial <- model_select(mat[responses], mat[covariates], cutoff)
    results[i] <- coef(summary(trial))[,4]
    hist(results)
  }
}