# Statistical Thinking (ETC2420 / ETC5242)
# Semester 2, 2025
#
# Week 8, Workshop

# Disclaimer:
#
# The code below was written live during the workshop and run interactively.
# As such, it may not necessarily be properly formatted or as polished as
# production-quality code.
#
# We recommend that you consult the recording of the workshop when reviewing
# this code. Various important details were conveyed verbally in the workshop,
# and some aspects were shown through incremental edits of the code, neither of
# which are captured below.


library(MASS)
library(tidyverse)
library(qqplotr)

quick_histogram <- function(x, bins = 20) {
  ggplot(tibble(x = x)) + aes(x = x, y = after_stat(density)) +
    geom_histogram(bins = bins, colour = "blue", fill = "blue", alpha = 0.5) +
    theme_bw()
}

income <- read_csv("data/income.csv")
income <- income$income
income

quick_histogram(income, 10)

ggplot(tibble(income = income), aes(sample = income)) +
  stat_qq_band(distribution = "exp", identity = TRUE) +
  stat_qq_line(distribution = "exp", identity = TRUE) +
  stat_qq_point(distribution = "exp") +
  theme_bw()


prior_alpha <- 1
prior_beta <- 1

sampsize <- length(income)
mean(income)

posterior_alpha <- prior_alpha + sampsize
posterior_beta  <- prior_beta + sampsize * mean(income)

posterior_alpha / posterior_beta

# 95% cred. int.
qgamma(c(0.025, 0.975), posterior_alpha, posterior_beta)

posterior_args <- list(shape = posterior_alpha, rate = posterior_beta)

ggplot() + geom_function(fun = dgamma, args = posterior_args,
                         xlim = c(0, 0.03), linewidth = 1.2) +
  theme_bw() + xlab(expression(lambda))

credibility_factor <- sum(income) / (sum(income) + prior_beta)
credibility_factor


## Median

rate_posterior_draws <- rgamma(5000, posterior_alpha, posterior_beta)

quick_histogram(rate_posterior_draws) +
  geom_function(fun = dgamma, args = posterior_args,
                xlim = c(0, 0.03), linewidth = 1.2, inherit.aes = FALSE) +
  xlab(expression(lambda))

median_posterior_draws <- qexp(0.5, rate_posterior_draws)

quick_histogram(median_posterior_draws, 20) + xlab("Median income")

mean(median_posterior_draws)

quantile(median_posterior_draws, c(0.025, 0.975))


## 99th percentile

top1_posterior_draws <- qexp(0.99, rate_posterior_draws)

quick_histogram(top1_posterior_draws, 20) + xlab("99th percentile income")

quantile(top1_posterior_draws, c(0.025, 0.975))
