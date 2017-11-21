#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(sqldf)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Mining - Predict Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 5,
       textInput("yourtext",
                   "Type a sentence (max 100 characters)",
                   value = "",
                   placeholder = "Enter a sentence and click Predict Next Word",
                   width = 350
                 ),
      actionButton("submit",label=" Predict Next Word",icon = icon("send", class= "glyphicon glyphicon-send", lib="glyphicon") )
      #submitButton("Predict Next Word",icon = icon("send", class= "glyphicon glyphicon-send", lib="glyphicon") )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       verbatimTextOutput("predicted"),
       verbatimTextOutput("Note")
      
    )
  )
))
