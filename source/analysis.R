library("dplyr")
library("ggplot2")
library("plotly")
library("readr")
library("leaflet")

#Loading dataset into R:
urlfile="https://raw.githubusercontent.com/vera-institute/incarceration-trends/ae6a18945c23b56aa8d4b00ff83090d83a171fac/incarceration_trends.csv"

incarceration_dataset <- read_csv(url(urlfile))
spec(incarceration_dataset)
View(incarceration_dataset)
dim(incarceration_dataset)

#-------------------------------------------------------------------------------
#My 5 values of interest:

#Value #1: What is the average jail population across all counties in 2018?
av_jail_pop <- incarceration_dataset %>%
  filter(year == 2018) %>%
  summarise(av_jail_pop = mean(total_jail_pop, na.rm =TRUE)) %>%
  pull(av_jail_pop)

#Value #2: Which state has the largest jail population, proportional to its population?
state_largest_jail_pop <- clean_incarceration_dataset %>%
  group_by(state) %>%
  summarize(prop_jail_pop = sum(total_jail_pop, na.rm = TRUE)/sum(total_pop, na.rm = TRUE)) %>%
  filter(prop_jail_pop == max(prop_jail_pop)) %>%
  pull(state)

#Value #3: Which state has the largest proportion of juvenile inmates in jails?
state_largest_juvenile_pop <- clean_incarceration_dataset %>%
  group_by(state) %>%
  summarize(prop_juvenile_pop = sum((female_juvenile_jail_pop + male_juvenile_jail_pop), na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(prop_juvenile_pop == max(prop_juvenile_pop)) %>%
  pull(state)

#Value #4: How has total inmates in jails from ice changed from 1980 to 2000?
jail_pop_1980 <- incarceration_dataset %>%
  filter(year == 1980) %>%
  summarise(total_inmates_1980 = sum(total_jail_pop, na.rm = TRUE))%>%
  pull(total_inmates_1980)

jail_pop_2000 <- incarceration_dataset %>%
  filter(year == 2000) %>%
  summarise(total_inmates_2000 = sum(total_jail_pop, na.rm = TRUE))%>%
  pull(total_inmates_2000)

jail_pop_1980_to_2000 <- jail_pop_2000 - jail_pop_1980

#Value #5: What is the average proportion of blacks in jails by state?
prop_blacks <- clean_incarceration_dataset %>%
  summarize(prop_blacks = sum(black_jail_pop, na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(prop_blacks)

#-------------------------------------------------------------------------------
#Trends Over Time Chart:
#(Use Q 3- yr vs pop) 3: How has total jail population changed from 1970 to 2018, by race?
# Or: Better: Total jail population in CA, by race, from 1970 to 2018


#-------------------------------------------------------------------------------
#Variable Comparison Chart:
#(Use Q 2) or compare latinx pop vs total in jail from ice
#2: Does commuting zone have an affect on jail population?


#-------------------------------------------------------------------------------
#Map: 
#(Use Q 1 or 5) 5: Which state has the largest proportion of (prev answer) people in jails?
#1: What state had the highest portion of its residents in jails in 2018?
  #Use total_pop_15to64 for this - incorporate race?

new_incarceration <- incarceration_dataset %>%
  state.abb[match(state,state.name)]
View(new_incarceration)

highest_jail_pop <- incarceration_dataset %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  mutate(prop_in_jail = (total_jail_pop/total_pop_15to64)*100) %>%
  select(state, county_name, prop_in_jail) %>%
  state.abb[match("state",state.name)]

View(highest_jail_pop)


states <- map_data("state")
ggplot(states) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white",
    size = .1
  ) + 
  coord_map()

states <- map_data("state") %>%
  rename(state = region) %>%
  left_join(incarceration_dataset, by="state")

View(states)
