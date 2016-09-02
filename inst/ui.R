library(shiny)
library(productplots)
library(plyr)
library(ggmosaic)
library(plotly)

# Define UI for application that creates a mosaic plot
shinyUI(fluidPage(

  # Application title
  headerPanel("Mosaic Plots with ggplot2"),

    # sidebar with options for controlling parameters
    sidebarPanel(
      selectizeInput('group', "Variables to compare:", NULL, multiple = TRUE, options = list(maxItems = 3)),
      selectizeInput('group2', "Variables to condition on:", NULL, multiple = TRUE, options = list(maxItems = 2)),
      selectizeInput('col', 'Variable to fill:', names(happy[c(2,4:9)]), selected = "health"),
      selectizeInput('div', 'Divider', c("mosaic", "mosaic reversed", "double decker"), selected="mosaic" ),
      sliderInput('offset', 'Offset', min =0, max = 10, value = 3),
    #  sliderInput("slider1", label = h3("Slider"), min = 0, max = .1, value = .01),
      checkboxInput("coord", label = "Flip coordinates", value = FALSE),
      checkboxInput("labels", label = "Add labels", value = FALSE),
    actionButton("goButton", "Create mosaic plot"),

   #   uiOutput("formula"),
      width=3
    ),

    # plotting
    mainPanel(
      # textOutput("funct"),
      verbatimTextOutput("formula"),
      plotlyOutput("mosaicplot", height = "700px"),

      width = 9
      )

))
