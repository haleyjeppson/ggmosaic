library(shiny)
library(productplots)
library(plyr)
library(ggmosaic)


# Define UI for application that creates a mosaic plot
shinyUI(fluidPage(

  # Application title
  titlePanel("Mosaic Plot"),

  sidebarLayout(

    # sidebar with options for controlling parameters
    sidebarPanel(
      selectizeInput('group', "Select variables to compare:", NULL, multiple = TRUE, options = list(maxItems = 3)),
      uiOutput("values"),
      selectizeInput(
        'col', 'Color', names(happy[c(2,4:8)])
      )
    ),

    # plotting
    mainPanel(
      # textOutput("funct"),
      plotOutput("mosaicplot")
    )
  )
))
