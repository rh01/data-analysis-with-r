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
ggplot(data = sfo_feb_flight, aes(x = arr_delay))+geom_histogram()

sfo_feb_flight %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

