library("dplyr")
library("ggplot2")
library("plotly")
library("readr")
library("leaflet")
library("knitr")

#Loading dataset into R:
urlfile="https://raw.githubusercontent.com/vera-institute/incarceration-trends/ae6a18945c23b56aa8d4b00ff83090d83a171fac/incarceration_trends.csv"

incarceration_dataset <- read_csv(url(urlfile))
spec(incarceration_dataset)
View(incarceration_dataset)
dim(incarceration_dataset)
clean_incarceration_dataset <- na.omit(incarceration_dataset)

#-------------------------------------------------------------------------------
#My 5 values of interest:
#summary_info <- list()
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
  summarize(prop_blacks = sum(black_jail_pop, na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE)*100) %>%
  pull(prop_blacks)

#-------------------------------------------------------------------------------
#Variable Comparison Chart:
#Comparing latinx population vs total in jail from ice
var_comparison_data <- clean_incarceration_dataset %>%
  filter(latinx_pop_15to64 <= 1000000)

var_comparison_plot <- ggplot(data = var_comparison_data) +
  geom_point(mapping = aes(x = latinx_pop_15to64, y = total_jail_from_ice)) +
  geom_smooth(mapping = aes(x = latinx_pop_15to64, y = total_jail_from_ice)) +
  labs(
    title = "Comparing Latinx Population vs Total in Jail from ICE",
    x = "Latinx Population",
    y = "Population in Jail from ICE"
  ) 

ggplotly(var_comparison_plot)

View(var_comparison_data)

#-------------------------------------------------------------------------------
#Trends Over Time Chart:
#Total jail population in CA from 2000 to 2013
time_plot_data <- clean_incarceration_dataset %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  summarise(combo_total_jail_pop = sum(total_jail_pop)) 

time_plot <- ggplot(data = time_plot_data) +
  geom_line(mapping = aes(x = year, y = combo_total_jail_pop)) +
  labs(
    title = "Total jail population in California from 2000 to 2013",
    x = "Year (2000-2013)",
    y = "Total Jail Population"
  )
ggplotly(time_plot)

View(time_plot_data)

#-------------------------------------------------------------------------------
#Map: 
#What state had the highest (portion) amount of its residents in jails in 2000?

highest_jail_pop <- clean_incarceration_dataset %>%
  filter(year == 2000) %>%
  select(state, total_jail_pop) %>%
  mutate(state = tolower(state))

View(highest_jail_pop)

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(highest_jail_pop, by="state")

View(state_shape)

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "white",
    size = .1
  ) + 
  coord_map()+
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Jail Populations")  +
  theme_bw()

View(state_shape)


#Real
library(ggplot2)
all_states <- map_data("state")
View(all_states)

jail_pop_Plot <- ggplot()+geom_polygon(
  data=state_shape, 
  aes(x= long, y= lat, group = group, fill=total_jail_pop),
  color="grey50")+coord_map()
jail_pop_Plot


