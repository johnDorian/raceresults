library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)
library(dplyr)

data <- read.csv("data/testing_race_results_local.csv") %>% 
  rename(`Race Time` = Race.Time,
         `BIB Number` = Bib.Number) %>% 
  mutate(Race = "Lochness Beast Race", 
         Year = 2016)

