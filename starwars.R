## Create your goal tibble to replicate 

# Run this line to see what your end product should look like
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
sw.wrangled.goal <- read_csv("sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor))

# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 

## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute


# Start with the original starwars data frame
starwars_df <- starwars

# Wrangle the data to match the desired structure
sw_wrangled <- starwars_df %>%
  #delete data when both height and mass is NA
  filter(!(is.na(height) & is.na(mass))) %>%
  # Separate names 
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  # Convert height from cm to inches
  mutate(height_in = sprintf("%.4f", height * 0.393701),
         # Ensure height_cm is an integer value
         height_cm = as.integer(height),
         # Create initials
         initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)),
         # Determine on Brown hair 
         brown_hair = hair_color == "brown",
         # Convert gender 
         gender = ifelse(gender == "masculine", "m", ifelse(gender == "feminine", "f", NA))) %>%
  rename(hair = hair_color) %>%
  # Select all of the cols I want 
  select(first_name, last_name, initials, height_in, height_cm, mass, hair, gender, species, homeworld, brown_hair)

# Print 
print(sw_wrangled)

# graph height_cm
# I feel like there is a bit different because I dont have the gaps?
library(ggplot2)
ggplot(sw.wrangled.goal, aes(x = height_cm)) +
  geom_histogram(binwidth = 50, boundary = 100, fill = "grey", color = "black") +
  scale_x_continuous(breaks = seq(100, 250, by = 50)) +
  labs(title = "Histogram of Height in cm", x = "Height (cm)", y = "Count") +
  theme_minimal()

# Graph Sorted hair 
# I dont know how to have the count order 
ggplot(sw.wrangled.goal, aes(x = hair)) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Bar Chart of Sorted_hair", x = "Sorted_hair", y = "Count") +
  theme_minimal() 

# scatter plot for mass vs. height
ggplot(sw.wrangled.goal, aes(x = height_in, y = mass)) +
  geom_point() +  
  labs(title = "Scatter Plot of Mass vs Height", x = "Height (in)", y = "Mass") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 160)) 