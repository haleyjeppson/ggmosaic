library(shiny)
library(ggmosaic)
library(productplots)

shinyServer(function(input, output, session) {

  names<- names(happy)
  d<- data.frame(value=names, label=names)
  updateSelectizeInput(session, 'group', choices = d[c(2,4,5,6,7,8),], server=TRUE)
  updateSelectizeInput(session, 'group2', choices = d[c(2,4,5,6,7,8),], server=TRUE)

  output$values<- renderText({
    input$group
  })

  xstr <- reactive({
    xx <- paste(input$group, collapse = ",")
     sprintf("product(%s)", xx)
  })

  cond <- reactive({
    c <- paste(input$group2, collapse = ",")
    sprintf("product(%s)", c)
    })


  output$mosaicplot = renderPlot({
     ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ))
    })


})
