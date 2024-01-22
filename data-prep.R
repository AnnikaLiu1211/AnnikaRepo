# Homework 3.2 mock-up plan
# import the librarys
library(writexl)
library(dplyr)
library(readxl)

# read the original data
file_path <- "Depression.xlsx"
Depression.data <- readxl::read_excel(file_path)

# Create Level_types including three variable names: Anxiety Level (SAS), Depression Level (SDS), Depression Level (HAMD)
# I will make decision later on if I should use SDS or HAMD 
Depression.data <- Depression.data.long %>%
  unite(New_Variable, `Anxiety Level (SAS)`, `Depression Level (SDS)`, `Depression Level (HAMD)`, sep = "")
# I hope that my level measure can correspond to the above Level_Types and contain the values of the original Anxiety Level (SAS), Depression Level (SDS) and Depression Level (HAMD)

# creating a Level_Score  can correspond to the above Level_Types and containing: Index Score (SAS), Index Score (SDS), HAMD Total Score

# Creating Symptoms including all symptoms' names
# Creating Symptoms_Score correspond with the symptoms' name 

# I am so sorry I really dont know how I can do this to make them correspond with each other

# create a new Excel file
Depression.data.long_file_path <- "Depression.data.long.xlsx"

# Write the intermediate dataset to the Excel file
writexl::write_xlsx(Depression.data.long, Depression.data.long_file_path)
