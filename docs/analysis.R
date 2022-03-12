# Sean Lim
# A3 - Incarceration Data
# Analysis and Plots
# 2/25/22

# Load packages
library(tidyverse)
library(maps)
library(mapproj)
# Load dataset
jail_dataset <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(jail_dataset)

# Total population of minority women in prison
jail_dataset$total_minority_men_prison <- (jail_dataset$black_male_prison_pop + jail_dataset$latinx_male_prison_pop + jail_dataset$aapi_male_prison_pop + 
                                         jail_dataset$native_male_prison_pop + jail_dataset$other_race_male_prison_pop)

# Black men in prison
jail_dataset$percentage_black_men_prison <- (jail_dataset$black_female_prison_pop / jail_dataset$male_prison_pop) * 100

# AAPI men in prison 
jail_dataset$percentage_aapi_men_prison <- (jail_dataset$aapi_male_prison_pop / jail_dataset$male_prison_pop) * 100

# Percentage of Latino prison populations
jail_dataset$percentage_aapi_prison_pop <- (jail_dataset$aapi_prison_pop / jail_dataset$total_prison_pop) * 100

# Percentage of AAPI prison population from the total AAPI population
jail_dataset$percentage_aapi_prison_from_total_pop <- (jail_dataset$aapi_prison_pop / jail_dataset$aapi_pop_15to64) * 100

# Percentage of minority prison admissions
jail_dataset$total_minority_prison_adm <- ((jail_dataset$black_prison_adm + jail_dataset$latinx_prison_adm + jail_dataset$aapi_prison_adm + 
  jail_dataset$native_prison_adm + jail_dataset$other_race_prison_adm) / jail_dataset$total_prison_adm) * 100


# Filter data to get rid of NA values
filter_data <- na.omit(jail_dataset)

# Find the state with highest percentage of minority prison admissions
state_highest_minority_percent <- filter_data %>%
  group_by(state) %>%
  summarize(total_minority_prison_adm = max(total_minority_prison_adm)) %>%
  filter(total_minority_prison_adm == max(total_minority_prison_adm)) %>%
  pull(state)

# Find the highest count of minority women in prison
county_highest_minority <- filter_data %>%
  group_by(county_name) %>%
  summarize(total_minority_men_prison = max(total_minority_men_prison)) %>%
  filter(total_minority_men_prison == max(total_minority_men_prison)) %>%
  pull(total_minority_men_prison)

# Find the state with the highest percentage of Black women in prison
state_max_black_men_prison <- filter_data %>%
  group_by(state) %>%
  summarize(percentage_black_men_prison = max(percentage_black_men_prison)) %>%
  filter(percentage_black_men_prison == max(percentage_black_men_prison)) %>%
  pull(state)

# Find the state with the highest percentage of AAPI women in prison
state_max_aapi_men_prison <- filter_data %>%
  group_by(state) %>%
  summarize(percentage_aapi_men_prison = max(percentage_aapi_men_prison)) %>%
  filter(percentage_aapi_men_prison == max(percentage_aapi_men_prison)) %>%
  pull(state)

# Find the county with the highest percentage of AAPI in prison from the AAPI population
county_highest_aapi_prison <- filter_data %>%
  group_by(county_name) %>%
  summarize(percentage_aapi_prison_from_total_pop = max(percentage_aapi_prison_from_total_pop)) %>%
  filter(percentage_aapi_prison_from_total_pop == max(percentage_aapi_prison_from_total_pop)) %>%
  pull(county_name)

# Find data from California and Washington
ca_wa_data <- filter_data %>%
  filter(state == "CA" | state == "WA") 


# Create plot of percentage of AAPI prison populations over time
aapi_prison_pop_plot <- ca_wa_data %>%
  ggplot(mapping = aes(x=year, y=percentage_aapi_prison_pop, color = state), alpha = .3) +
  geom_point() +
  labs(
    title = "Percentage of AAPI Prison Populations Over Time in CA and WA",
    x = "Time (in Years)",
    y =  "Percentage",
    fill = "State"
  )

# Plot the graph
plot(aapi_prison_pop_plot)


# Create scatterplot comparing the percentage of Black women and AAPI women prison population
black_vs_aapi_men_prison_pop <- ggplot(data = filter_data) +
  geom_point(mapping = aes(x = percentage_aapi_men_prison, y = percentage_black_men_prison), color = "#89b0ae", alpha = .5) +
  labs(
    title = "Percentage of Black vs AAPI Men Prison Population",
    x = "Percentage of AAPI Men",
    y = "Percentage of Black Men"
  )  

# Plot the graph
plot(black_vs_aapi_men_prison_pop)


# Create blank theme for minimalist map visual
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

# Create map data of the states and their counties
state_shape <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Join the map data of the state and county shapes along with the filtered data
map_data <- state_shape %>%
  left_join(filter_data, by = "fips")

# Create choropleth map describing the variable of the total admission rate of minorities
heat_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_minority_prison_adm),
    color = "white",
    size = .1        
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$total_minority_prison_adm)), 
                        low = "#aac2f6", high = "#8b2846") +
  blank_theme +
  labs(
    title = "Prison Minority Admissions", 
    fill = "Prison Admissions in Percentages"
  )

# Plot the map
plot(heat_map)

