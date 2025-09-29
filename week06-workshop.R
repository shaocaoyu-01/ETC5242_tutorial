# Statistical Thinking (ETC2420 / ETC5242)
# Semester 2, 2025
#
# Week 6, Workshop

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

quick_histogram <- function(x, bins = 20) {
  ggplot(tibble(x = x)) + aes(x = x, y = after_stat(density)) +
    geom_histogram(bins = bins, colour = "blue", fill = "blue", alpha = 0.5) +
    theme_bw()
}

income <- read_csv("data/income.csv")
income <- income$income

quick_histogram(income, 10)


income_normal <- fitdistr(income, "normal")
income_normal

quick_histogram(income, 10) +
  geom_function(fun = dnorm, args = income_normal$estimate,
                xlim = c(-100, 350), colour = "red",
                inherit.aes = FALSE)

ggplot(tibble(income = income)) + aes(sample = income) +
  stat_qq(distribution = qnorm, dparams = income_normal$estimate) +
  stat_qq_line(distribution = qnorm, dparams = income_normal$estimate) +
  #geom_abline(intercept = 0, slope = 1, colour = "red") +
  theme_bw()


income_gamma <- fitdistr(income, "gamma")
income_gamma

quick_histogram(income, 10) +
  geom_function(fun = dgamma, args = income_gamma$estimate,
                xlim = c(0, 350), colour = "red",
                inherit.aes = FALSE)

ggplot(tibble(income = income)) + aes(sample = income) +
  stat_qq(distribution = qgamma, dparams = income_gamma$estimate) +
  stat_qq_line(distribution = qgamma, dparams = income_gamma$estimate) +
  #geom_abline(intercept = 0, slope = 1, colour = "red") +
  theme_bw()

library(qqplotr)

ggplot(tibble(income = income)) + aes(sample = income) +
  stat_qq_band(distribution = "gamma") +
  stat_qq_line(distribution = "gamma") +
  stat_qq_point(distribution = "gamma") +
  theme_bw()


ggplot(tibble(x = rexp(80))) + aes(sample = x) +
  stat_qq_band() + stat_qq_line() + stat_qq_point() +
  theme_bw()

income_gamma

ci_upper <- income_gamma$estimate + 1.96* income_gamma$sd
ci_lower <- income_gamma$estimate - 1.96* income_gamma$sd
tibble(parameter = names(income_gamma$estimate), ci_lower, ci_upper)


income_gamma$estimate["shape"] / income_gamma$estimate["rate"]

qgamma(0.75, shape = income_gamma$estimate["shape"], rate= income_gamma$estimate["rate"])



mean(income)
