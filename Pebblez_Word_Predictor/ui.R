library(dplyr)
library(shiny)
library(ggplot2)
library(shinycssloaders)
library(shinyBS) 
library(shinythemes)


shinyUI(
    navbarPage(
        "Coursera Data Science Specialization Capstone",
        theme = shinytheme("united"),
        tabPanel("Word Prediction",
                 br(),
                 br(),
                 # Sidebar
                 sidebarLayout(
                     sidebarPanel(
                         helpText("This app predicts the next word given an input word or a sentence"),
                         br(),
                         textInput("inputString", "Please enter a word or a sentence here",value = ""),
                         br(),
                         br()
                     ),
                     mainPanel(
                         h2("Next Word"),
                         h5("The next word for a given word or a sentence is predicted using n-gram modelling and backoff techniques."),
                         br(),
                         strong("Next word is"),
                         verbatimTextOutput("prediction") %>% withSpinner(color="orange1"),
                         br(),
                     )
                 )
        ),
        tabPanel(
            "About",
            h3("Coursera Data Science Specialization: Capstone Final Project"),
            h4("Author: Pebblez - 2025"),     
            br(),
            p("This application used Shiny Apps built in RStduio for the final project for the course",
              a(href = "https://www.coursera.org/learn/data-science-project", "Data Science Capstone")
            ),
            p("which is part of the Data Science Specialization Course offered by Johns Hopkins University via Coursera.",
              a(href = "https://www.coursera.org/specializations/jhu-data-science", "Data Science Specialization")
            ),
            br(),
            p("Source code of this application is available at",
              a(href = "https://github.com/pebblez1208/Data-Science-Capstone-Project",
                "https://github.com/pebblez1208/Data-Science-Capstone-Project")
            )
        )
    )
)