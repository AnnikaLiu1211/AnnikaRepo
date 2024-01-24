library(tidyverse) 

## Create your goal tibble to replicate 

# Run this line to see what your end product should look like
library(magrittr)
library(dplyr)
library(readr)
sw.wrangled.goal <- read_csv("sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor))

# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 

## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute

starwars <- read_csv("sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor))  

sw.wranged <- starwars %>%
  pivot_longer(cols = -c(first_name, last_name, initials, height_in, height_cm, mass, hair, gender, species, homeworld),
               names_to = c(".value", "variable"),
               names_sep = "_")

print(sw.wranged)
# I have one step I am unable to do. when I add brown_hair as a cols, it appears the error that Error in `pivot_longer()`:! `cols` must select at least one column.
# I tried to debug for the whole afternoon and I tried to make brown_hair as a factor, using mutate(brown_hair = if_else(brown_hair, "TRUE", "FALSE") but I realized it is not the same thing on the goal image. 
# everything works fine if I do not include it as a clos, will book a meeting to figure out on this, Thank you! 


## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled.goal, sw.wrangled.goal)
