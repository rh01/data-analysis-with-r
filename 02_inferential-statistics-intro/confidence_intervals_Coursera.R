# generate random seed for repreduced
set.seed(20170912)

# load library
library(ggplot2)
library(dplyr)
library(statsr)

# load data from specific data
data(ames)

# 
n <-60
samp <- sample_n(ames, n)

# Confidence intervals
samp %>% 
  summarise(x_bar = mean(area), count = n())


# for example. I can compute the critucl value for a 95% confidence interal using
z_star_95  = qnorm(0.025)
z_star_95
# or
z_star_95_2 = qnorm(0.975)
z_star_95_2

samp %>%
  summarise(lower = mean(area) - z_star_95_2*sd(area)/sqrt(n()),
            upper = mean(area) + z_star_95_2*sd(area)/sqrt(n()))

params <- ames %>%
  summarise(mu = mean(area))
params


ci <- ames %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_95_2*sd(area)/sqrt(n()),
            upper = mean(area) + z_star_95_2*sd(area)/sqrt(n()))

ci %>%
  slice(1:5)


ci <- ci %>%
  mutate(capture_mu = ifelse(
    lower < params$mu & upper > params$mu, "yes", "no"
  ))

ci_data  <- data.frame(
  ci_id = c(1:50, 1:50),
  ci_bounds = c(ci$lower, ci$upper),
  capture_mu = c(ci$capture_mu, ci$capture_mu)
)
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id,
                           group = ci_id, color = capture_mu)) + geom_point(size = 2) + geom_line() + geom_vline(xintercept = params$mu, color="darkgray")



params99 <- ames %>%
  summarise(mu = mean(area))

params99


z_star_99 = qnorm(0.995)
z_star_99

ci99 <- ames %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_99*sd(area)/sqrt(n()),
            upper = mean(area) + z_star_99*sd(area)/sqrt(n()))

ci99 %>%
  slice(1:5)
# ci99

# create new variable
ci99  <- ci99 %>%
  mutate(capture_mu = ifelse(params99$mu > lower & params99$mu < upper, "yes", "no"))
ci99

ci99_data  <- data.frame(
  ci_id = c(1:50, 1:50),
  ci_bounds = c(ci99$lower, ci99$upper),
  capture_mu = c(ci99$capture_mu, ci99$capture_mu)
)

ggplot(data = ci99_data, aes(x = ci_bounds, y = ci_id,
                           group = ci_id, color = capture_mu)) + geom_point(size = 2) + geom_line() + geom_vline(xintercept = params99$mu, color="darkgray")

 
