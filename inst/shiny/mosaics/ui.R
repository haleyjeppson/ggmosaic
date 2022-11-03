library(shiny)
library(shinythemes)
library(DT)
# Define UI for application that creates a mosaic plot
shinyUI(
  fluidPage(
    theme = shinytheme("sandstone"),
    tags$head(
      tags$style(HTML("
      .btn {border: none; font-size: 24px; line-height: 22px; font-weight: 500; text-transform: uppercase;}"))
    ),
    titlePanel("Hello ggmosaic!"),
    br(),
    # javascript code for listening to the keypress
    tags$script('
            $(document).on("keyup", function (e) {
            if(e.keyCode == 72){
            Shiny.onInputChange("keyPressedH", Math.random());}
            if(e.keyCode == 83){
            Shiny.onInputChange("keyPressedS", Math.random());}
            if(e.keyCode == 66){
            Shiny.onInputChange("keyPressedB", Math.random());}
            if(e.keyCode == 86){
            Shiny.onInputChange("keyPressedV", Math.random());}
            if(e.keyCode == 37){
            Shiny.onInputChange("keyPressedLeft", Math.random());}
            if(e.keyCode == 38){
            Shiny.onInputChange("keyPressedUp", Math.random());}
            if(e.keyCode == 39){
            Shiny.onInputChange("keyPressedRight", Math.random());}
            if(e.keyCode == 40){
            Shiny.onInputChange("keyPressedDown", Math.random());
                }});'),

    sidebarLayout(
      sidebarPanel(selectInput(inputId = "dataset", label = "Select a dataset:", choices = c("happy", "fly", "titanic"), selected = "fly"),
                   br(),
                   h5(strong("Select variables:")),
                   p("Use arrow keys (or buttons below) to add, remove, or switch variables."),
                   HTML("&nbsp;&nbsp;&nbsp"), actionButton("key_up", label = "\u2B06"), actionButton("key_down", label = "\u2B07"), actionButton("key_left", label = "\u2B05"), actionButton("key_right", label = "\u27A1"),
                   br(),
                   br(),
                   br(),
                   h5(strong("Select output:")),
                   p("Use 'h', 'v', 's', and 'b' keys (or buttons below) to switch type of divider:"),

                   column(width = 6,
                          p("horizontal vs. vertical"),
                          actionButton("key_h", label = "H"), HTML("&nbsp"), actionButton("key_v", label = "V")
                          ),
                   column(width = 6,
                          p("spine vs. bar"),
                          actionButton("key_s", label = "S"), HTML("&nbsp"), actionButton("key_b", label = "B")
                          ),

                    #HTML("&nbsp"),
                   br(),
                   br(),
                   br(),
                   br()
      ),
      mainPanel(tabsetPanel(
                  tabPanel("Mosaic Plot",
                           br(),
                           plotOutput("mosaicplot", height = "500px"),
                           br(),
                           br()
                  ),
                  tabPanel("Data",
                           br(),
                           DTOutput("datatable", width = "400px"),
                           br()
                  )
                ),
                h5("Code to recreate mosaic plot:"),
                verbatimTextOutput("code"),
                br(),
                br(),
                br(),
                br()
      )
    )

  )
)
