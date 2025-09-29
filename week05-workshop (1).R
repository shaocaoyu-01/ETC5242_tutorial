# Statistical Thinking (ETC2420 / ETC5242)
# Semester 2, 2025
#
# Week 5, Workshop

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


library(tidyverse)

income <- read_csv("data/income.csv")
income <- income$income

income

hist(income, 50)

quick_histogram <- function(x, bins = 20) {
  ggplot(tibble(x = x)) + aes(x = x, y = after_stat(density)) +
    geom_histogram(bins = bins) +
    theme_bw()
}

quick_histogram(income, 20)

# Median

median(income)

B <- 5000
median_boot <- numeric(B)   # rep(NA, B)

for (b in 1:B) {
  income_b <- sample(income, replace = TRUE)
  median_boot[b] <- median(income_b)
}

quick_histogram(median_boot, 80)
quantile(median_boot, c(0.025, 0.975))


# 99th percentile

top1 <- quantile(income, 0.99)

top1_boot <- numeric(B)

for (b in 1:B) {
  income_b <- sample(income, replace = TRUE)
  top1_boot[b] <- quantile(income_b, 0.99)
}

quick_histogram(top1_boot, 80)
quantile(top1_boot, c(0.025, 0.975))


# Ratio of quartiles

quantile(income, 0.25)
quartile_ratio <- quantile(income, 0.75) / quantile(income, 0.25)

quartile_ratio

quartile_ratio_boot <- numeric(B)

for (b in 1:B) {
  income_b <- sample(income, replace = TRUE)
  quartile_ratio_boot[b] <- quantile(income_b, 0.75) / quantile(income_b, 0.25)
}

quick_histogram(quartile_ratio_boot, 80)
quantile(quartile_ratio_boot, c(0.025, 0.975))
