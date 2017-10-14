library(ggplot2)
library('statsr')
library('dplyr')

# load data
data("nycflights")

# show data column name 
names(nycflights)

# shown data structure
str(nycflights)

# We might want to find out how delayed flights headed to a particular destination tend to be.
# We might want to evaluate how departure delays vary over months.
# Or we might want to determine which of the three major NYC airports has a better on time percentage for departing flights.

# Analysis
ggplot(data = nycflights,aes(x = dep_delay))+geom_histogram()

# ggplot(data = nycflights,aes(x = dep_delay))+geom_bar()


ggplot(data = nycflights,aes(x = dep_delay))+geom_histogram(binwidth = 15)
ggplot(data = nycflights,aes(x = dep_delay))+geom_histogram(binwidth = 100)

ggplot(data = nycflights,aes(x = dep_delay))+geom_histogram(binwidth = 150)

# now focus on rdu destination and plot hostgram 
rdu_flights <- nycflights %>%
  filter(dest == "RDU")
ggplot(data = rdu_flights, aes(x= dep_delay)) + geom_histogram()



# We can also obtain numerical summaries for these flights:
rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n=n())



# headed to SFO 
# | means or, "," means and
sfo_feb_flight = nycflights %>%
  filter(dest =="SFO" | month == 2)

sfo_feb_flight = nycflights %>%
  filter(dest =="SFO" , month == 2)

ggplot(data = sfo_feb_flight, aes(x = dep_delay))+geom_histogram()


dim(sfo_feb_flight)


sfo_feb_flight %>%
  summarise(mean_dd = mean(arr_delay), sd_dd = sd(arr_delay),n=n(),max_hour = max(arr_delay)/60)
ggplot(data = sfo_feb_flight, aes(x = arr_delay/60))+geom_histogram()

sfo_feb_flight %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

# Calculate the median and interquartile range for arr_delays of flights in the sfo_feb_flights data frame, grouped by carrier. Which carrier has the hights IQR of arrival delays?
sfo_feb_flight %>%
  group_by(carrier) %>%
  summarise(median = median(arr_delay),iqr = IQR(arr_delay)) %>%
  arrange(desc(iqr))

# Which month would you expect to have the highest average delay departing from an NYC airport?
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

# Which month has the highest median departure delay from an NYC airport?
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay),median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

ggplot(data = nycflights,aes(x = factor(month),
                             y = dep_delay)) + geom_boxplot()


# first classify each flights as "on time" or "delayed"
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on_time", "delayed"))


# then group flights by origin airport
nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rates = sum(dep_type == "on_time")/n()) %>%
  arrange(desc(ot_dep_rates))


# The summarise step is telling R to count up how many records of the currently found group are on time - sum(dep_type == “on time”) - and divide that result by the total number of elements in the currently found group - n() - to get a proportion, then to store the answer in a new variable called ot_dep_rate.

ggplot(data=nycflights, aes(x = origin, fill=dep_type))+geom_bar()

# create new variable called avg_speed
nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time/60))

nycflights %>%
  select(avg_speed, tailnum) %>%
  arrange(desc(avg_speed))

# Make a scatterplot of avg_speed vs. distance. Which of the following is true about the relationship between average speed and distance.
ggplot(data=nycflights, aes(x = avg_speed, y=distance)) + geom_point()

# 
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on_time", "delayed"))
# Suppose you define a flight to be “on time” if it gets to the destination on time or earlier than expected, regardless of any departure delays. Mutate the data frame to create a new variable called arr_type with levels "on time" and "delayed" based on this definition. Then, determine the on time arrival percentage based on whether the flight departed on time or not. What proportion of flights that were "delayed" departing arrive "on time"? [NUMERIC INPUT]
nycflights %>%
  summarise(ot_arr_rates = sum(arr_type == "on_time" & dep_type=="delayed")/sum(dep_type == "delayed")) %>%
  arrange(desc(ot_arr_rates))

  
