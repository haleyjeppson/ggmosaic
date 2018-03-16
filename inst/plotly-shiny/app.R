library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("p"),
  verbatimTextOutput("event")
)

data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels = c("Yes", "No"))

server <- function(input, output, ...) {

  output$p <- renderPlotly({
    ggplot(data = titanic) +
      geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
  })

  output$event <- renderPrint({
    event_data("plotly_click")
  })

}

shinyApp(ui, server)
