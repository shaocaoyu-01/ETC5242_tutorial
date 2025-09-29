# Statistical Thinking (ETC2420 / ETC5242)
# Semester 2, 2025
#
# Week 9, Workshop

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

# A further note specific to this week:
#
# In this workshop we make use of the R base graphics system, rather than the
# ggplot/tidyverse system.  This is primarily for speed and convenience: the
# base graphics system is very quick and concise to use for scatter plots and
# diagnostic plots of regression models.  The main goal of the plots in the
# workshop was to see the various concepts illustrated, rather than on learning
# the R code.  You can do the latter more effectively via the notes & exercises
# in the tutorials.


library(tidyverse)

x <- runif(30, 1, 5)
y1 <- rnorm(30, 1 + 2 * x, 1)

d1 <- tibble(xvar = x, yvar = y1)
d1

plot(x, y1, col = 4, las = 1)

l1 <- lm(y1 ~ x)
l2 <- lm(yvar ~ xvar, d1)

abline(l1, col = 4)

library(broom)

tidy(l1)
glance(l1)

confint(l1)

plot(l1, 1:3)

fit_and_draw_plots <- function(x, y) {
  fitted_model <- lm(y ~ x)
  par(mfrow = c(2, 2))
  plot(x, y, col = "blue", pch = 19, las = 1,
       xlab = deparse1(substitute(x)),
       ylab = deparse1(substitute(y)),
       main = "Data and fitted model")
  abline(fitted_model, col = "blue", lwd = 2)
  plot(fitted_model, 1:3, las = 1)
}

fit_and_draw_plots(x, y1)

y2 <- rnorm(30, 1 + 2 *x + 2 * x^2, 1)
fit_and_draw_plots(x, y2)

l2 <- lm(y2 ~ x)
glance(l2)

y3 <- rnorm(30, 1 + 2 * x, 1 + 0.5 * x^2)
fit_and_draw_plots(x, y3)

y4 <- rchisq(30, 2 * x)
fit_and_draw_plots(x, y4)

data(Animals, package = "MASS")
head(Animals, 4)

m0 <- lm(brain ~ body, Animals)
m0

plot(Animals, col = 4)
abline(m0, col = 4)

LogAnimals <- log(Animals)
head(LogAnimals, 4)

m1 <- lm(brain ~ body, LogAnimals)
m1

plot(LogAnimals, col = 4)
abline(m1, col = 4)

fit_and_draw_plots(LogAnimals$body, LogAnimals$brain)

LogAnimals2 <- LogAnimals[-c(6, 16, 26), ]

m2 <- lm(brain ~ body, LogAnimals2)
m2

plot(LogAnimals2, col = 4)
abline(m2, col = 4)

Animals[-c(6, 16, 26), ]
Animals[c(6, 16, 26), ]
