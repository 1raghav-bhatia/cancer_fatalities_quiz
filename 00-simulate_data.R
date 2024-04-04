#### Preamble ####
# Purpose: Simulates the voter preferences dataset.
# Author: Raghav Bhatia 
# Date: 4 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(readr)

#### Simulate data ####
set.seed(100) 
# for reproducibility

# Number of entries
num_years <- 20

# Simulating the data
simulated_data <- tibble(
  year = rep(
            2004:2023,
              each = 5
              ),
  Hospital_Name = rep(
                        c(
                          "St Vincents Public Hospital",
                          "Westmead Hospital",
                          "Concord Repatriation General Hospital",
                          "Sydney Adventist Hospital",
                          "Royal Prince Alfred Hospital"
                          ),
                        times = num_years
                        ),
  Number_of_Deaths = rpois(
                      n = 100,
                      lambda = 1500
                      )
)

# Viewing the first few rows of the simulated data
head(simulated_data)

# Storing the simulated table

write_csv(simulated_data, "data/cleaned_data/cancer_fatalities.csv")

# Testing the simulated table

# Test if the dataset has 100 entries
test_that("Dataset has 100 entries", {
  expect_equal(nrow(simulated_data), 100)
})

test_that("Years are in expected range", {
  expect_true(all(simulated_data$year >= 2004 & simulated_data$year <= 2023))
})

test_that("Hospital_Name contains only valid entries", {
  valid_hospitals <- c("St Vincents Public Hospital", "Westmead Hospital", 
                       "Concord Repatriation General Hospital", "Sydney Adventist Hospital", 
                       "Royal Prince Alfred Hospital")
  expect_true(all(simulated_data$Hospital_Name %in% valid_hospitals))
})

test_that("Number_of_Deaths is non-negative", {
  expect_true(all(simulated_data$Number_of_Deaths >= 0))
})

test_that("Data has correct number of rows", {
  expect_equal(nrow(simulated_data), num_years * 5)
})

test_that("Total deaths per year are reasonably plausible", {
  expect_true(simulated_data$Number_of_Deaths <= 15,000) 
})

test_that("Mean deaths per year is close to lambda", {
  expect_lte(abs(mean(simulated_data$Number_of_Deaths) - 1500), 200)
})

test_that("Number of unique years is 20", {
  expect_equal(unique(simulated_data$year), 20)
})

test_that("Mean deaths per year is less than a large value", {
  expect_lte(mean(simulated_data$Number_of_Deaths) , 15,000)
})

test_that("Number of variables is 3", {
  expect_equal(length(colnames(simulated_data)) , 3)
})

