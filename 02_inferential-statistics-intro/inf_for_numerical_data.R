# new a seed 
set.seed(20171020)

library(ggplot2)
library(dplyr)
library(statsr)

# load data
data(nc)

# EDA
str(nc)
names(nc)
dim(nc)[1]

summary(nc$gained)
# ggplot(data = nc, aes(x = nc$gained)) + geom_histogram(binwidth = 10)

ggplot(data = nc, aes(x = habit, y = weight)) + geom_boxplot(outlier.color = "red")
nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))


inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci",
          method = "theoretical", order = c("smoker", "nonsmoker"), conf_level = 0.99
)

inference(y = weeks, data = nc, statistic = "mean", type = "ci",
          method = "theoretical", order = c("smoker", "nonsmoker"), conf_level = 0.99
)




