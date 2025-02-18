# Simulate Bernoulli distribution
set.seed(123)
p_success <- 0.3
bernoulli_data <- rbinom(1000, size = 1, prob = p_success)

# Plot simulated data
barplot(table(bernoulli_data), main = "Simulated Bernoulli distribution", xlab = "Outcome", col = "red", border = "black")



# Simulate binomial distribution
set.seed(123)
p_success <- 0.4
n_trials <- 10 
binomial_data <- rbinom(2000, size = n_trials, prob = p_success)

# Plot simulated data
barplot(table(binomial_data), main = "Simulated binomial distribution", xlab = "Outcome", col = "red", border = "black")



# Define the (negative) log-likelihood function for a Bernoulli distribution
llk <- function(p, data) {
  -sum(data * log(p) + (1 - data) * log(1 - p))
}

# log likelihood curve
xx <- seq(0.01, 0.99, .01)
yy <- sapply(X=xx, FUN=llk, data =bernoulli_data)
plot(xx,yy,type='l', ylab='', xlab='')

# Maximize the log-likelihood to find MLE of p
mle_result <- optim(par = 0.3, fn = llk, data = bernoulli_data,method="Brent", 
                    lower = 0.000001, upper = 0.999999)
cat("p estimate (ML optimization):", mle_result$par, "\n")



# Plot beta distribution for different parameter values
alpha_values <- c(1,3,5)
beta_values <- c(1,3,5)

par(mfrow = c(3, 3))

for (i in 1:length(alpha_values))
  for (j in 1:length(beta_values)){
    xx <- seq(0, 1, length.out = 5000)
    yy <- dbeta(xx, shape1 = alpha_values[i], shape2 = beta_values[j])
    tit <- paste("Beta distribution (α =", alpha_values[i], ", β =", beta_values[j], ")")
    plot(xx,yy,type='l',xlim = c(0, 1), ylim = c(0, 5), 
         ylab='density',xlab='x',main = tit)
  }
# Reset plotting layout
par(mfrow = c(1, 1))



# Simulate Bernoulli distribution
set.seed(123)
p_success <- 0.3
bernoulli_data <- rbinom(100, size = 1, prob = p_success)
barplot(table(bernoulli_data), 
        main = "Sample from the Bernoulli population (p=0.3)", 
        xlab = "Outcome", col = "red", border = "black")

# Sample size

n <- length(bernoulli_data)

# Prior hyperparameters 

alpha_0 <- 1 
beta_0  <- 1 

# Calculate posterior parameters

alpha_n <- sum(bernoulli_data) + alpha_0 
beta_n  <- n - sum(bernoulli_data) + beta_0

# summary

cat("Posterior alpha:",alpha_n,"\n")

cat("Posterior beta:",beta_n,"\n")

cat("Posterior mean:", alpha_n/(alpha_n+beta_n),"\n") #Mean

library(ggplot2)

# Prior distribution

xx    <- seq(0, 1, length.out = 1000) 
prior_density <- dbeta(xx, alpha_0, beta_0)


# Posterior distribution

posterior_density <- dbeta(xx, alpha_n, beta_n)

# Plot prior and posterior distributions

p <- ggplot() +
  geom_line(aes(x = xx, y = prior_density), 
            color = "blue", linewidth = 1, show.legend=TRUE) +
  geom_line(aes(x = xx, y = posterior_density), 
            color = "red", linewidth = 1, show.legend=TRUE) +
  geom_vline(xintercept = p_success, linetype = "dashed", 
             color = "green", linewidth = 1, show.legend=TRUE) +
  scale_color_manual(values = c("blue", "red", "green"), 
                     name = "Distribution", 
                     labels = c("Prior", "Posterior", "True value")) +
  theme_minimal() +
  labs(title = "Prior and posterior distributions (and true value)", 
       x = "Probability of success (p)", 
       y = "Density") +
  theme(legend.position = "top")
p

#install.packages('rstan')

library(rstan)

# Stan model code

stan_model_code <- "
data {
  int<lower=0> n;              // Number of observations
  int<lower=0, upper=1> y[n];  // Binary outcomes
}

parameters {
  real<lower=0, upper=1> p;    // Probability of success
}

model {
  p ~ beta(1, 1);               // Prior distribution for p
  y ~ bernoulli(p);             // Likelihood
}
"

# Compile the Stan model

stan_model <- stan_model(model_code = stan_model_code)

# Create a list of data for Stan

stan_data <- list(n = n, y = bernoulli_data)

# Fit the model to the data

stan_fit <- sampling(stan_model, data = stan_data, chains = 4, iter = 1000)

#install.packages("bayesplot")
library(bayesplot)

## Extract posterior draws from stan_fit object

stan_fit_p <- extract(stan_fit, permuted = FALSE)

## Traceplot

mcmc_trace(stan_fit_p, pars = "p") +
  theme_default()

# Print a summary of the Stan fit

print(stan_fit)

# Plotting the posterior distribution
plot_title <- ggtitle("Posterior distribution of p",
                      "Median and 80% interval")
mcmc_areas(stan_fit, pars = "p", prob = 0.8) + 
  plot_title + 
  theme_minimal() 

# Compare these results 


cat("p estimate (Mean):", mean(stan_fit_p[,,1]), "\n")

cat("p estimate (Median):", median(stan_fit_p[,,1]), "\n")

# Define the logistic function

logistic  <- function(x) {
  exp(x)/(1+exp(x))}

# logistic curve
curve(logistic, -6, 6, col ="red")

# Generate a sequence of values between 0 and 1
xx <- seq(0.001, 0.999, length.out = 100)

# Logit transformation function
logit <- function(x) {
  log(x / (1 - x))
}

# Probit transformation function
probit <- function(x) {
  qnorm(x)
}

# Apply the transformations to the sequence of values
yy_logit  <- logit(xx)
yy_probit <- probit(xx)

# Plot logit transformation
plot(xx, yy_logit, type = "l", col = "blue",
     main = "Logit and probit transformations",
     xlab = "Probability (p)", ylab = "Transformed value",
     ylim = c(min(yy_logit, yy_probit), max(yy_logit, yy_probit)))

# Add probit transformation to the plot
lines(xx, yy_probit, col = "red")

# Add legend
legend("topleft", legend = c("logit", "probit"), col = c("blue", "red"), lty = 1)

# Generate a sequence of values between 0 and 1
xx <- seq(-5, 5, length.out = 100)

# Inverse logit transformation function
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}

# Probit transformation function
invprobit <- function(x) {
  pnorm(x)
}

# Apply the transformations to the sequence of values
yy_logit  <- invlogit(xx)
yy_probit <- invprobit(xx)

# Plot inverse logit transformation
plot(xx, yy_logit, type = "l", col = "blue",
     main = "Inverse logit and probit transformations",
     ylab = "Probability (p)", xlab = "",
     ylim = c(min(yy_logit, yy_probit), max(yy_logit, yy_probit)))

# Add invserse probit transformation to the plot
lines(xx, yy_probit, col = "red")

# Add legend
legend("topleft", legend = c("inverse logit", "inverse probit"), col = c("blue", "red"), lty = 1)
