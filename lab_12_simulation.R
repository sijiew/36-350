generate_data <- function(n, p) {
  return(list(covariates = matrix(rnorm(n*p), nrow = n),
              responses = rnorm(n)))
}

model_select <- function(covariates, responses, cutoff) {
  lm.0 <- lm(responses ~ covariates)
  indices <- summary(lm.0)$coefficients[-1,4] <= cutoff
  if (sum(indices) == 0) {
    return(vector(length = 0, mode = "numeric"))
  }
  lm.1 <- lm(responses ~ covariates[, indices])
  return(as.numeric(summary(lm.1)$coefficients[-1,4]))
}
