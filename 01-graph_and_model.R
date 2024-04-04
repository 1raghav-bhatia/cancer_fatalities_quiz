#### Preamble ####
# Purpose: To create the graph and linear model from the simulated dataset.
# Author: Raghav Bhatia 
# Date: 4 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the simulated dataset.


#### Workspace setup ####
library(boot)
library(broom.mixed)
library(collapse)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(arrow)

#### Read data ####
cancer_data <- read_csv("data/cleaned_data/cancer_fatalities.csv")


### Graph ###

deaths <- cancer_data |>
  group_by(year) |>
  summarise(total_deaths = sum(Number_of_Deaths))

death_plot <- deaths |>
  ggplot(aes(x = year, y = total_deaths)) +
  geom_col()
  
death_plot


### Model ####

set.seed(100)

# This glm regresses total deaths on year

deaths_by_year_model <-
  stan_glm(
    total_deaths ~ year,
    data = deaths,
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 100
  )


#### Save model ####

saveRDS(
  deaths_by_year_model,
  file = "models/deaths_by_year_model.rds"
)


