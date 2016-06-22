library(shiny)
library(ggmosaic)
library(productplots)

shinyServer(function(input, output, session) {

  names<- names(happy)
  d<- data.frame(value=names, label=names)
  updateSelectizeInput(session, 'group', choices = d[c(2,4,5,6,7,8),], server=TRUE)
  output$values<- renderText({
    input$group
  })


  output$mosaicplot = reactivePlot(function(){
   # if(length(input$group) > 0) vars = paste(input$group, collapse=",")

   # if(length(input$group) > 0)  col = paste(input$col)

   # ggplot(data = happy) + geom_mosaic(aes(x = product(vars), fill = col))
   # ggplot(data = happy) + geom_mosaic(aes(x = product(input$group), fill = input$col))
    if(length(input$group) > 0)
    {
      expression = "ggplot(data = happy) + geom_mosaic( aes(x = product("
      expression = paste(expression, input$group[1])
      if(length(input$group) > 1 ){
        for(x in 2 : length(input$group))
        {
          expression = paste(expression,",",input$group[x])
        }
      }

    if (length(input$col) > 0) expression = paste(expression,"), fill=", input$col, ")) + theme(axis.text.x=element_text(angle = 25, hjust = 1))" )
    else expression =  paste(expression,"))) + theme(axis.text.x=element_text(angle = 25, hjust = 1))" )
    }
    else
      return()

    #      theme(axis.text.x=element_text(angle = -70, hjust = 0))")

    print(expression)
    eval(parse(text = expression))
  })


})
