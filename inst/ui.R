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
      selectizeInput('group', "Select variables to compare:", NULL, multiple = TRUE, options = list(maxItems = 3)),
      selectizeInput('group2', "Select variables to condition on:", NULL, multiple = TRUE, options = list(maxItems = 2)),
      uiOutput("values"),
      selectizeInput(
        'col', 'Color', names(happy[c(2,4:9)]), selected = "health"
      )
    ),

    # plotting
    mainPanel(
      # textOutput("funct"),
      plotOutput("mosaicplot")
    )

))
