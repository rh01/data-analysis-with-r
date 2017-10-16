# set random seed to reproduce your results
set.seed(20171016)

# load librarys
library(statsr)
library(ggplot2)
library(dplyr)

# load data that 
data("ames")
dim(ames)
names(ames)

ggplot(data = ames,
       aes(x=area)) + geom_histogram(binwidth = 250)

ames %>%
  summarise(mu = mean(area), pop_medp_iqr=IQR(area),
                pop_min=min(area), p = median(area),
                sigma = sd(area), poop_max = max(area),
                pop_q1 = quantile(area, 0.25),
                pop_q3 = quantile(area, 0.75))

# get a simple random sample size 50 from population 
samp1 <- ames %>%
  sample_n(size = 50)

ggplot(data = samp1, 
       aes(x = area)) + geom_histogram()

samp1 %>%
  summarise(x_bar = mean(area))
# 1499->mu, 1519.0->x_bar 
