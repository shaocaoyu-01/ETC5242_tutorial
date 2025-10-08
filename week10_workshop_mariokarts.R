library(tidyverse)
library(openintro)
library(broom)
library(car)
library(meifly)
library(GGally)

#what can i do for it


diagplots <- functions(m) {
  op<- par(mfow = c(2,3))
  plot(m,1:6)
  par(op)
}


mariokart
View(mariokart)
?mariokart
ggpairs(mariokart[,1:11])

m1 <-lm(total_pr ~ cond + duration + wheels, data = mariokart)


tidy(m1)

mariokart



m2 <- lm(total_pr ~ cond +duration +wheels, data= mariokart_singles)

tidy(m2)
diagplots(m2)

m3 <- lm(total_pr ~ cond +duration +wheels + stock_photo, data =mariokarts)


tidy(m3)
diagplot(m3)


m4 <- lm(total_pr ~ cond +duration +wheels + stock_photo+ seller_ra, data =mariokarts)


tidy(m4)
diagplot(m4)


m0 <-lm(total_pr ~ 1, data = mariokart_singles)

tidy(m0)


glance(m2)

glance(m3)

glance(m4)

?step

step(m4)

step(m4, direction = "both")

step_out

tidy(step_out)



?fitall

?lego_sample
