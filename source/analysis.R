library("dplyr")
library("ggplot2")
library("plotly")
library("readr")
library("leaflet")
library("knitr")

incarceration_dataset <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/ae6a18945c23b56aa8d4b00ff83090d83a171fac/incarceration_trends.csv", stringsAsFactors = FALSE)
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
  summarize(prop_jail_pop = (sum(total_jail_pop, na.rm = TRUE)/sum(total_pop, na.rm = TRUE))*100) %>%
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

#Code for line chart
var_comparison_plot <- ggplot(data = var_comparison_data) +
  geom_point(mapping = aes(x = latinx_pop_15to64, y = total_jail_from_ice)) +
  geom_smooth(mapping = aes(x = latinx_pop_15to64, y = total_jail_from_ice)) +
  labs(
    title = "Comparing Latinx Population vs Total in Jail from ICE",
    x = "Latinx Population",
    y = "Population in Jail from ICE"
  ) 

#-------------------------------------------------------------------------------
#Trends Over Time Chart:
#Total jail population in CA from 2000 to 2013
time_plot_data <- clean_incarceration_dataset %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  mutate(combo_aapi_jail_pop = sum(aapi_jail_pop)) %>%
  mutate(combo_black_jail_pop = sum(black_jail_pop)) %>%
  mutate(combo_latinx_jail_pop = sum(latinx_jail_pop)) %>%
  mutate(combo_native_jail_pop = sum(native_jail_pop)) %>%
  mutate(combo_white_jail_pop = sum(white_jail_pop)) %>%
  mutate(combo_other_jail_pop = sum(other_race_jail_pop))

#Code for dot plot
time_plot <- ggplot(data = time_plot_data) +
  geom_line(mapping = aes(x = year, y = combo_latinx_jail_pop, color = "Latinx")) +
  geom_line(mapping = aes(x = year, y = combo_white_jail_pop, color = "White")) +
  geom_line(mapping = aes(x = year, y = combo_black_jail_pop, color = "Black")) +
  geom_line(mapping = aes(x = year, y = combo_aapi_jail_pop, color = "AAPI")) +
  geom_line(mapping = aes(x = year, y = combo_other_jail_pop, color = "Other")) +
  geom_line(mapping = aes(x = year, y = combo_native_jail_pop, color = "Native")) +
  scale_color_manual(name = "Race", values = c("Latinx" = "red", "Black" = "darkblue", "AAPI" = "lightblue", "Native" = "darkgreen", "White" = "lightpink", "Other" = "orange")) +
  labs(
    title = "Jail population in California, by race, from 2000 to 2013",
    x = "Year (2000-2013)",
    y = "Total Jail Population"
  )

#-------------------------------------------------------------------------------
#Map: 
#What state had the highest amount of its residents in jails in 2018?

#Use state codes to help merge
state_codes <- read.csv('./Data/statecodes.csv', stringsAsFactors = FALSE)

state_codes_fix <- state_codes %>%
  rename(state = Code)

highest_jail_pop <- incarceration_dataset %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(prop_jail_pop = (sum(total_jail_pop, na.rm = TRUE)/sum(total_pop, na.rm = TRUE))*100) %>%
  select(state, prop_jail_pop) #%>%
  #group_by(state) %>%
  #summarise(total_jail_pop = sum(prop_jail_pop, na.rm = T)) 

first_merge <- highest_jail_pop %>%
  left_join(state_codes_fix, by="state") %>%
  mutate(State = tolower(State)) %>%
  rename(Abb = state) %>%
  rename(state = State) 

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  group_by(state) %>%
  left_join(first_merge, by="state") %>%
  group_by(state)
  #summarise(sum_total_jail_pop = log(sum(total_jail_pop, na.rm = T))) 


#Code for map
us_plot <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prop_jail_pop),
    color = "white",
    size = .1
  ) + 
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Jail Populations")  +
  theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),       
    axis.ticks = element_blank(),      
    axis.title = element_blank(),      
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()    
  ) +
  labs(x="", y="", title="Jail Population by State")

#END

