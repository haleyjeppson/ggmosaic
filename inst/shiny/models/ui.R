library(shiny)
library(shinythemes)
library(DT)
# Define UI for application that creates a mosaic plot
shinyUI(
  fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("EDA with Mosaic Plots"),
    navlistPanel(widths = c(3, 8),
      #"Step 1:",
      tabPanel("Step 1: data & variable selection",
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

               fluidRow(

               column(8,
                      h4("Step 1: Dataset and variable selection"),
                      br(),
                      tabsetPanel(
                        tabPanel("Mosaic Plot",

                                 br(),
                                 plotOutput("mosaicplot", height = "500px"),
                                 br(),
                                 # h5("Formula:", textOutput("formula", inline = TRUE)),
                                 br(),
                                 #actionButton("action", label = "Run model"),
                                 br(),
                                 br()
                        ),
                        tabPanel("Data",
                                 br(),
                                 DTOutput("datatable", width = "400px"),
                                 br()



                                 )
                      )
                      # br(),

               ),

               column(3,
                      br(),br(),br(),br(),br(),
                      selectInput(inputId = "dataset", label = "Select a dataset:", choices = c("happy", "fly", "titanic"), selected = "fly"),
                      br(),
                      h5(strong("Select variables:")),

                      p("Use arrow keys to add, remove, or switch variables."),
                      br(),
                      h5(strong("Select output:")),
                      p("Use 'h', 'v', 's', and 'b' keys to switch type of divider."),

                      br(),
                      br(),
                      actionButton("run_model", label = "Confirm variable selection"),
                      br()
                      #hr()

               )),
               fluidRow(
               # h5("Code output:"),
               # verbatimTextOutput("code"),
               br(),
               br(),
               br(),
               br()
               )


      ),
      # "Step 2:",
      tabPanel("Step 2: Model Selection",
               column(8,
                      h4("Step 2: Model selection"),
                      br(),
                      tabsetPanel(
                        tabPanel("Model Plot",
                                 plotOutput("modelplot", height = "500px"),
                                 br(),
                                 # h5("Model formula:", textOutput("model_formula", inline = TRUE)),
                                 br(),
                                 # h5("Interactions:", textOutput("interact", inline = TRUE)),
                                 br(),
                                 # actionButton("action", label = "Run model"),
                                 hr()
                        ),
                        tabPanel("Residual plot",
                                 plotOutput("residplot", height = "500px")
                        ),
                        tabPanel("Model output",
                                 br(),
                                 h5(strong("Model results:")),
                                 br(),
                                 gt::gt_output("model_info"),
                                 br(),
                                 hr(),
                                 h5(strong("Model coefficients:")),
                                 br(),
                                 gt::gt_output("model_coeffs"),
                                 br()
                        )
                      )
               ),
               column(3,
                      br(),br(),br(),br(),br(),br(),

                      selectInput("residual_type", "Type of residual:", c("Observed values", "Raw residuals", "Pearson residuals"), selected = "Pearson residuals"),
                      #br(),#br(),
                      actionButton("add_residuals", label = "View residuals"),
                      br(),br(),br(),br(),


                      checkboxGroupInput("interaction_opts", "Interactions:", NULL),
                      # hr(),
                      #br(), #br(),
                      actionButton("update_model", label = "Update Model"),
                      br(),br(),br()
               )
      )

      #
      #
      #
      # tabPanel("Model output",
      #
      #          br(),
      #          h5("Model Output:"), #, textOutput("formula", inline = TRUE)),
      #          br()
      #          ),
      # tabPanel("Residuals",
      #          h4("Step 3: View residuals"),
      #          plotOutput("residualplot", height = "500px"),
      #          br(),
      #          h5("Model Output:"), #, textOutput("formula", inline = TRUE)),
      #          br(),
      #          actionButton("step3", label = "Add interactions")
      # )
    )

  )
)
