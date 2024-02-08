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

# Assignment 13
# call the library
library(ggplot2)
library(ggsci)
library(dplyr)
library(tidyr)

# rename
sw.wrangled.goal <- sw.wrangled.goal %>%
  mutate(gender = as.character(gender), 
         gender = case_when(
           gender == "f" ~ "female",    
           gender == "m" ~ "male",     
           is.na(gender) ~ "Others",    
           TRUE ~ gender                
         ))

# define colors
gender_colors <- c("female" = "red", "male" = "grey", "Others" = "orange")  

# create the graph 
gg <- ggplot(sw.wrangled.goal, aes(x = height_cm, y = mass, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, aes(group = gender)) + 
  facet_wrap(~gender, scales = "free_y") + 
  scale_color_manual(values = gender_colors) +  
  theme_minimal(base_family = "Arial") + 
  scale_x_continuous(limits = c(60, 270)) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", family = "Arial", color = "white", hjust = 0),
    strip.background = element_rect(color = "black", fill = "darkgreen", size = 1),
    panel.background = element_rect(fill = "pink"),
    legend.position = "bottom",
  ) +
  labs(
    title = "Height and weight across gender presentation",
    subtitle = "A cautionary tale in misleading \"free\" axis scales & bad design choices",
    x = "Height (cm)",
    y = "Mass (Kg)",
    fill = "Gender Presentation"
  )

# print
print(gg)


# Assignment 12
# Plot 1
library(ggplot2)

# Create the boxplot
hair_color <- ggplot(sw.wrangled.goal, aes(x = hair, y = mass, fill = hair)) + 
  geom_boxplot() + 
  # giving each hair color colors 
  scale_fill_manual(values = c(
    'none' = 'red', 
    'brown' = 'yellow',
    'black' = 'greenyellow',
    'bald' = 'green4',
    'white' = 'green3',
    'blond' = 'cyan4',
    'auburn, white' = 'deepskyblue3',
    'blonde' = 'purple',
    'brown, grey' = 'orchid2',
    'grey' = 'hotpink2'
  )) +  
  scale_y_continuous(limits = c(0, 160)) +
  labs(title = "Colorful hair", x = "Hair color(s)", y = "Mass (kg)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

print(hair_color)

# Plot 2
library(ggplot2)

sw.wrangled.goal <- sw.wrangled.goal %>%
  mutate(brown_hair_presence = if_else(hair == "brown", "Has brown hair", "No brown hair"))

Mass_Brown <- ggplot(sw.wrangled.goal, aes_string(x = mass, y = height_in)) +
  # error in this step indicate "Error: object 'mass' not found", but I am sure there is a col called "mass" in sw.wrangled.goal, very confused 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~brown_hair_presence) + 
  labs(title = "mass vs. height by brown-hair-havingness",
       subtitle = "A critically important analysis",
       x = "mass",
       y = "height_in") +
  theme_minimal()

# Print the plot
print(Mass_Brown)

# Plot 3
library(ggplot2)

sw.wrangled.goal <- sw.wrangled.goal %>%
  mutate(species_first_letter = substr(species, 1, 1))

ggplot(sw.wrangled.goal, aes(x = species_first_letter, fill = gender)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values = c("f" = "salmon", "m" = "lightblue")) +
  labs(
    title = "A clear male human bias",
    x = "count",
    y = "species_first_letter",
    fill = "gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



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