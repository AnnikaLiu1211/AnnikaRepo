
library(tidyverse)
library(car)
library(writexl)
library(dplyr) 
library(readxl)
library(tidyr)
library(stringr)
library(patchwork)
library(broom)


file_path <- "ADepression.xlsx"
Depression.data <- readxl::read_excel(file_path)

modol <- lm(Depression_Score ~ Gender, data = Depression.data)
regression_model <- tidy(modol)


score_types <- c("Depression_Score", "Somatic_Score", "Cognitive_Impairment_Score", "Feelings_of_Despair_Score")

t_tests <- lapply(score_types, function(score_type) {
  t.test(reformulate("Gender", response = score_type), data = Depression.data)
})

names(t_tests) <- score_types
lapply(t_tests, print)

# Descripriptive data 
mean_point <- function(y) {
  geom_point(stat = "summary", fun = mean, aes(y = y, colour = "Mean"), shape = 18, size = 3, show.legend = TRUE)
}

table_data <- data.frame(
  Category = c("Depression Score", "Anxiety/Somatic Scores", "Cognitive Impairment Score", "Hopelessness Score"),
  Male_Mean = c(male_mean, male_mean_somatic, male_mean_cognitive, male_mean_despair),
  Female_Mean = c(female_mean, female_mean_somatic, female_mean_cognitive, female_mean_despair)
)

# Display the table
knitr::kable(table_data, caption = "Mean Scores by Gender")