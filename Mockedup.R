library(writexl)
library(dplyr) 
library(readxl)
library(tidyr)
library(stringr)

# read the original data
file_path <- "Depression.xlsx"
Depression.data <- readxl::read_excel(file_path)

# making Anxiety_level and Depression_Score level
level_measure_long <- Depression.data %>%
  pivot_longer(
    cols = c(Anxiety_Level, Depression_Level),
    names_to = "Level_Types",
    values_to = "Level_Measure"
  ) %>%
  mutate(Level_Types = str_replace(Level_Types, "_Level", ""))

# making Anxiety_Score and Depression_Score long
level_score_long <- Depression.data %>%
  pivot_longer(
    cols = c(Anxiety_Score, Depression_Score),
    names_to = "Score_Type",
    values_to = "Level_Score"
  ) %>%
  mutate(Score_Type = str_replace(Score_Type, "_Score", ""))

# making sysmtoms long 
symptoms_long <- Depression.data %>%
  select(ID, Somatic_Score, Cognitive_Impairment_Score, Feelings_of_Despair_Score) %>%
  pivot_longer(
    cols = c(Somatic_Score, Cognitive_Impairment_Score, Feelings_of_Despair_Score),
    names_to = "Symptoms",
    values_to = "Symptoms_Score"
  ) %>%
  mutate(Symptoms = sub("_Score", "", Symptoms)) %>%
  mutate(Level_Types = "Depression") # only match with depression 

# combine level_score_long, level_measure_long and symptoms_long
final_data <- left_join(level_measure_long, symptoms_long, level_score_long, by = c("ID", "Level_Types"))

# check
print(final_data)

library(openxlsx)
write.xlsx(final_data, "mockedupdata.xlsx")