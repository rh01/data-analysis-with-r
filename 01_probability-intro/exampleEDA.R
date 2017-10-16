load("selected_nzes2011.Rdata")

library(dplyr)
library(ggplot2)
library(statsr)
str(selected_nzes2011)
names(selected_nzes2011)

selected_nzes2011 %>% 
  select(jpartyvote, jdiffvoting, X_singlefav) %>% 
  str()

grep("singlefav", names(selected_nzes2011), value=TRUE)
selected_nzes2011 %>% 
  select(jpartyvote, jdiffvoting, X_singlefav) %>% 
  str()


selected_nzes2011 %>%
  group_by(jpartyvote) %>%
  summarise(count = n())


selected_nzes2011 %>%
  filter(jpartyvote != "Don't know", is.na(X_singlefav)) %>%
  group_by(jpartyvote) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


selected_nzes2011 %>%
  group_by(X_singlefav) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


selected_nzes2011 %>%
  filter(!is.na(X_singlefav), jpartyvote != "Don't know") %>%
  group_by(X_singlefav) %>%
  summarise(count = n())
          

selected_nzes2011 %>%
  group_by(jdiffvoting) %>%
  summarise(count = n())

selected_nzes2011 %>%
  filter(!is.na(jdiffvoting), jdiffvoting!="Don't know") %>%
  group_by(jdiffvoting) %>%
  summarise(count = n())


selected_nzes2011 <- selected_nzes2011 %>%
  mutate(samepart = ifelse(jpartyvote == X_singlefav, "same", "different"))

selected_nzes2011 %>%
  filter(!is.na(X_singlefav),!is.na(jpartyvote),jpartyvote!="Don't know") %>%
  group_by(jpartyvote, X_singlefav, samepart) %>%
  summarise(count = n())%>%
  arrange(desc(count))

selected_nzes2011 %>%
  group_by(jpartyvote, X_singlefav, samepart) %>%
  summarise(count =n()) %>%
  filter(samepart == "same")

selected_nzes2011 %>%
  group_by(jpartyvote, X_singlefav, samepart) %>%
  summarise(count =n()) %>%
  filter(samepart == "different")



selected_nzes2011 %>%
  group_by(jpartyvote, X_singlefav, samepart) %>%
  summarise(count =n()) %>%
  filter(is.na(samepart))


# part4
str(selected_nzes2011$jnzflike)
str(selected_nzes2011$jage)
selected_nzes2011 %>%
  group_by(jnzflike) %>%
  summarise(count =n())
selected_nzes2011 %>%
  filter(!is.na(jage)) %>%
  summarise(agemean = mean(jage), agemedian = median(jage), agesd = sd(jage), 
            agemin = min(jage), agemax = max(jage))

# Approach 1: Strongly liking and disliking NZ First and age
selected_nzes2011 %>%
  filter(jnzflike %in% c("0","10")) %>%
  group_by(jnzflike) %>%
  summarise(count = n())

selected_nzes2011 <- selected_nzes2011 %>%
  mutate(retiredage = ifelse(jage >= 65, "retired age", "working age"))
selected_nzes2011 %>%
  group_by(retiredage) %>%
  filter(!is.na(retiredage))%>%
  summarise(count = n())

selected_nzes2011 <- selected_nzes2011 %>% 
  mutate(numlikenzf = as.numeric(jnzflike))

selected_nzes2011 %>%
  group_by(jnzflike, numlikenzf) %>%
  summarise(count = n())

selected_nzes2011 <- selected_nzes2011 %>%
  mutate(numlikenzf = as.numeric(as.character(jnzflike)))


selected_nzes2011 %>% 
  group_by(jnzflike, numlikenzf) %>% 
  summarise(count = n())


