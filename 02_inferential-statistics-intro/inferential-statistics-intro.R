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

# q2
 ames%>%
  sample_n(size = 100) %>%
  summarise(x_bar = mean(area))

 ames %>%
  sample_n(size=1000) %>%
  summarise(x_bar = mean(area))

# samp_100 %>%
#   summarise(x_bar = mean(area))
# 
# 
# samp_1000 %>%
#   summarise(x_bar = mean(area))


ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))


# repeat sampleing
# rep 15000 times and with replacement
# take 15,000 samples of size 50
sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE)  %>%
  summarise(x_bar = mean(area))
ggplot(data = sample_means50, aes(x=x_bar)) + geom_histogram(binwidth = 20)

dim(sample_means50)
# Exercise: How many elements are there 
# in sample_means50? Describe the sampling 
# distribution, and be sure to specifically
# note its center. Make sure to include a
# plot of the distribution in your answer.
sample_means50 %>%
  summarise(count = n(), x_bar = mean(x_bar),
            s = sd(x_bar, na.rm = TRUE), pop_med = median(x_bar),
            pop_iqr = IQR(x_bar), pop_min = min(x_bar),
            pop_max = max(x_bar),
            pop_q1 = quantile(x_bar, 0.25),
            pop_q3 = quantile(x_bar, 0.75))

sample_means_small <- ames %>%
  rep_sample_n(size = 10, reps = 25) %>%
  summarise(x_bar = mean(area))
dim(sample_means_small)
sample_means_small %>%
  summarise(count = n())
ggplot(data = sample_means_small, aes(x = x_bar)) + geom_histogram(binwidth = 60)


