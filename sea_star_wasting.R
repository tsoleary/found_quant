# seas star wasting disease ----------------------------------------------------

library(dplyr)
library(ggplot2)

sun_data <- read.csv("Sunflower_data_complete_abundance_score_lower_limit_with_temp.csv")

star_data <- read.csv("sea_star_wasting_prevalence_2014.csv")

star_data_sum <- star_data %>%
  mutate(prop_healthy = healthy / count) %>%
  group_by(Site, species) %>%
  summarize(sd_prop_healthy = sd(prop_healthy),
            prop_healthy = mean(prop_healthy))

# grouped by species
ggplot(star_data_sum, aes(fill = Site, y = prop_healthy, x = species)) + 
  geom_bar(position = "dodge", stat = "identity")

# grouped by site
ggplot(star_data_sum, aes(fill = species, y = prop_healthy, x = Site)) + 
  geom_bar(position = "dodge", stat = "identity")
  
# get an average count per species and site
star_data_count <- star_data %>%
  group_by(Site, species) %>%
  summarize(sd_count = sd(count),
            count = mean(count))

# grouped by site
ggplot(star_data_count, aes(fill = species, y = count, x = Site)) + 
  geom_bar(position = "dodge", stat = "identity")


# prop_healthy over time for each site
star_data_date <- star_data %>%
  mutate(prop_healthy = healthy / count) %>%
  group_by(Site, Date) %>%
  summarize(sd_prop_healthy = sd(prop_healthy),
            prop_healthy = mean(prop_healthy))

star_data_date$Date <- julian(as.Date(star_data_date$Date,  format = "%m/%d/%y"))

ggplot(star_data_date, aes(color = Site, y = prop_healthy, x = Date)) + 
  geom_line()

# prop_healthy over time for each species
star_data_date <- star_data %>%
  mutate(prop_healthy = healthy / count) %>%
  group_by(species, Date) %>%
  summarize(sd_prop_healthy = sd(prop_healthy),
            prop_healthy = mean(prop_healthy))

star_data_date$Date <- julian(as.Date(star_data_date$Date,  format = "%m/%d/%y"))

ggplot(star_data_date, aes(color = species, y = prop_healthy, x = Date)) + 
  geom_line()



  


