generate_data <- function(n, p) {
  cov <- rnorm(n * p)
  row <- sample(1:p, 1)
  cov_mat <- matrix(cov, nrow = n, ncol = p)
  return(list(covariates = cov_mat, responses = cov_mat[,row]))
}

