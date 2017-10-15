

library(ggplot2)
library(statsr)
library(dplyr)

data("kobe_basket")
dim(kobe_basket)
names(kobe_basket)
show(kobe_basket)

# show data structure
str(kobe_basket)

head(kobe_basket)

kobe_basket[1:6]
 
calc_streak(kobe_basket$shot)

kobo_streak <- calc_streak(kobe_basket$shot)
ggplot(data = kobo_streak, aes(x= length))+geom_histogram(binwidth = 1)

# simulate 
coin_outcomes <- c("heads", "tails")
sample(coin_outcomes, size = 1, replace = TRUE)

sim_fair_coin = sample(coin_outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)

sim_unfair_coin <- sample(coin_outcomes,
                          size = 100,
                          replace = TRUE,
                          prob = c(0.2, 0.8))
table(sim_unfair_coin)


# sim the independent shooter
shot_outcomes = c("H","M")
sim_basket = sample(shot_outcomes,
                    size = 1,
                    replace=TRUE)


sim_basket <- sample(shot_outcomes,
                    size = 133,
                    replace=TRUE,
                    prob = c(0.45,.55))
table(sim_unfair_basket)


sim_streak <-calc_streak(sim_basket)
ggplot(sim_streak,
       aes(length)) + geom_histogram(binwidth = 1)
