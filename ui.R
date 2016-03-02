library(shiny)
library(readr)
library(dplyr)
# Load the ggplot2 package which provides
# the 'mpg' dataset.

race_results <- read_csv("data/race_results.csv")
genders <- (race_results %>% distinct(Gen)  %>% filter(!is.na(Gen)))$Gen
# Define the overall UI
shinyUI(
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    titlePanel("Simple race reults demo"),
    
    # Create a new Row in the UI for selectInputs
  
    # Create a new row for the table.

    fluidRow(
      column(7,
             htmlOutput("text")
      )
    ),
    fluidRow(
      column(7,plotOutput("plot"))
      
    )
  
    
  
    
    
  )
)
