library(shiny)
library(ggmosaic)
library(productplots)
library(plotly)

data(happy, package = "ggmosaic")

shinyServer(function(input, output, session) {

  names<- names(happy)
  d<- data.frame(value=names, label=names)
  updateSelectizeInput(session, 'group', choices = d[c(2,4:9),], server=TRUE, selected=d[2,])
  updateSelectizeInput(session, 'group2', choices = d[c(2,5:9),], server=TRUE)


  xstr <- reactive({
    vars <- rev(input$group)
    xx <- paste(vars, collapse = ",")
     sprintf("product(%s)", xx)
  })

  cond <- reactive({
    if (length(input$group2 > 0)){
      conds <- rev(input$group2)
      c <- paste(conds, collapse = ",")
      sprintf("product(%s)", c)
    }
    else return()
    })

  off <- reactive({
    log(1+(input$offset/300))

  })

  plot <- reactive({
    if (input$div == "mosaic"){
    gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ), offset = off())+
      theme(axis.text.x = element_text(size=rel(1), angle = 0, hjust=-.51, vjust=-.5), axis.title.x = element_text(size=rel(1.5)),
            legend.key.size = unit(1, "cm"), legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size = rel(1.5)))
    }
    else{
      if (input$div == "mosaic reversed"){
        gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ),
                                                  offset = off(), divider = mosaic("v"))+
          theme(axis.text.x = element_text(size=rel(1), angle = 0, hjust=1), axis.title.x = element_text(size=rel(1.5)),
                legend.key.size = unit(1, "cm"), legend.text = element_text(size=rel(1.2)),
                legend.title = element_text(size = rel(1.5)))
      }
      else {
      gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ),
                                                offset = off(), divider=ddecker())+
        theme(axis.text.x = element_text(size=rel(1), angle = 0, hjust=1), axis.title.x = element_text(size=rel(1.5)),
              legend.key.size = unit(1, "cm"), legend.text = element_text(size=rel(1.2)),
              legend.title = element_text(size = rel(1.5)))
      }}

    if (input$coord == TRUE){
      gg <- gg + coord_flip()
    }
    else gg

    if (input$labels == TRUE){
      create_labels <- function(plot, sub=NA){
        data=ggplot_build(plot)$data[[1]]
        # data = subset(data, fill=="grey50")
        data=subset(data, level==max(data$level))
        if (!is.na(sub)){
          data = subset(data, sub)
        }
        data$x = (data$xmin+data$xmax)/2
        data$y = (data$ymin+data$ymax)/2
        # data$label <- apply(data[grep("^[x]{1,}[0-9]", names(data))],1,paste, collapse = "\n")
        df <- data.frame(x=data$x, y=data$y, label=data$label)
        return(df)
      }

      labs <- create_labels(gg)

      gg <- gg + annotate("text", x = labs$x, labs$y, label=labs$label, colour = "black")
    }
    else gg

    gg

  })


  output$mosaicplot = renderPlotly({
    if (input$goButton == 0)
      return()

    isolate({ggplotly(plot())   })})

  form <- reactive({
   formula <-  paste(rev((input$group)), collapse="+")

   if (!is.null(input$col)) {
    if(!is.null(input$group2)){
      if (!(any(grepl(input$col, input$group2)) == TRUE)) {
        if (!(any(grepl(input$col, input$group)) == TRUE)) {
          formula <- paste(input$col, "+",formula) }

      }
    }
    else {
      if (!(any(grepl(input$col, input$group)) == TRUE)) {
        formula <- paste(input$col, "+", formula) }
    }}


  formula <- paste("weight~", formula)

  if (! is.null(input$group2)) {
      formula <- paste(formula, paste(rev((input$group2)), collapse="+"), sep="|")
  }
  formula

})

  output$formula <- renderText({
    if (input$goButton == 0)
      return()

    isolate({ paste("Formula : ", form()) })
  })

})

