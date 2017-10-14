# import librarys and packages
library('ggplot2')
library('statsr')
library('dplyr')

# import present data
# fist data source is from 16xx-
# second data sourcec is from 1940-
data('present')
data('arbuthnot')

# data dim and some range of two data
dim(present)
dim(arbuthnot)

names(present)
names(arbuthnot)

# select specfic column 
present$year
present$boys
range(present$year)
sum(present$boys + present$girls)
present$boys + present$girls

# add new column called total and prop_boys 
present <- present %>%
  mutate(total = boys + girls)
present <- present %>%
  mutate(prop_boys = boys / total)

# plot year vs prop_boys
ggplot(data = present, aes(x = year, y=prop_boys))+geom_line()

# add new column called more_boys which contains the value of either TRUE if that year had more boys than girls, or FALSE if that year did not. 
present <- present %>%
  mutate(more_boys = boys > girls)


# Calculate the boy-to-girl ratio each year, and store these values in a new variable called prop_boy_girl in the present dataset.
present <- present %>%
  mutate(prop_boy_girl = boys / girls)

ggplot(data = present, aes(x=year, y=prop_boy_girl))+geom_line() + geom_point()


# In what year did we see the most total number of births in the U.S.?
present %>%
 mutate(total = boys + girls) %>%
 arrange(desc(total))
