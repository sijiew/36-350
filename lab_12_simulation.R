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

run_simulation <- function(n_trials, n, p, cutoff) {
  p.values <- vector(length = 0, mode = "numeric")
  for (i in 1:n_trials) {
    d <- generate_data(n, p)
    data.vals <- model_select(d$covariates, d$responses, cutoff)
    p.values <- c(p.values, data.vals)
  }
  save(p.values, file = paste("n-",n,",p-",p,",cut-",cutoff,".RData", sep=""))
}

make_plot <- function(datapath) {
  load(datapath)
  hist(x = p.values,
       main = paste("P-Values of n: ", n, ", p: ", p, sep=""),
       xlab = "P-Values",
       ylab = paste("Frequency in trials"))
}

par(mfrow = c(3,3))
cutoff = 0.05
# Data
for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)) {
    run_simulation(1000, n, p, cutoff)
  }
}
# Plot
for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)) {
    make_plot(paste("n-",n,",p-",p,",cut-",cutoff,".RData", sep=""))
  }
}