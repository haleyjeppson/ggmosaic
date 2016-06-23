library(shiny)
library(productplots)
library(plyr)
library(ggmosaic)


# Define UI for application that creates a mosaic plot
shinyUI(fluidPage(

  # Application title
  headerPanel("Mosaic Plot"),

    # sidebar with options for controlling parameters
    sidebarPanel(
      selectizeInput('group', "Variables to compare:", NULL, multiple = TRUE, options = list(maxItems = 3)),
      selectizeInput('group2', "Variables to condition on:", NULL, multiple = TRUE, options = list(maxItems = 2)),
      selectizeInput('col', 'Variable to fill:', names(happy[c(2,4:9)]), selected = "health"),
      selectizeInput('div', 'Divider', c("mosaic", "ddecker"), selected="mosaic" ),
      selectizeInput('offset', 'Offset', c(0,0.01,0.025,0.05,0.075,0.1), selected = 0.01),
      uiOutput("formula"),
      width=3
    ),

    # plotting
    mainPanel(
      # textOutput("funct"),
      plotOutput("mosaicplot", height = "700px"),
      width = 9
      )

))
