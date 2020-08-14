#code  - adapted from Rasmus Baath's wonderful tutorial
#http://www.sumsar.net/files/posts/2017-bayesian-tutorial-exercises/modeling_exercise1.html

n_draw <- 100000

#Telemarketing success rate is 35%
#Test returns positives for 8 out of 20

# Defining and drawing from the prior distribution
#assume non-informative prior
prior_rate <- runif(n_draw, 0, 1)
hist(prior_rate)

# Defining the generative model
likelihood <- function(rate) {
  signups <- rbinom(1, size = 20, prob = rate)
  signups
}

# Simulating the data
signups <- rep(NA, n_draw)
for(i in 1:n_draw) {
  signups[i] <- likelihood(prior_rate[i])
}

# Filtering out those parameter values that didn't result in the
# data that we actually observed
posterior_rate <- prior_rate[signups == 8]

# Checking that there enough samples left
length(posterior_rate)

# Plotting and summarising the posterior.
hist(posterior_rate, xlim = c(0, 1))


mean(posterior_rate)

quantile(posterior_rate, c(0.025, 0.975))


#What is the chance of getting a better than 40% rate of conversions
sum(posterior_rate > 0.4) / length(posterior_rate)


#What is the distribution of signups if we marketed to 100 people

signups_100 <- rbinom(n = length(posterior_rate), size = 100, prob = posterior_rate)

hist(signups_100, xlim = c(0, 100))

quantile(signups_100, c(0.10, 0.9))

