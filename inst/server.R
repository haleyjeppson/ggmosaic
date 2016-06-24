library(shiny)
library(ggmosaic)
library(productplots)

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

  plot <- reactive({
    if (input$div == "mosaic"){
    gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ), offset = as.numeric(input$offset))+
      theme(axis.text.x = element_text(size=rel(1.7), angle = 25, hjust=1), axis.title.x = element_text(size=rel(2.2)),
            legend.key.size = unit(1, "cm"), legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size = rel(1.5)))
    }
    else{
      if (input$div == "mosaic reversed"){
        gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ),
                                                  offset = as.numeric(input$offset), divider = mosaic("v"))+
          theme(axis.text.x = element_text(size=rel(1.7), angle = 25, hjust=1), axis.title.x = element_text(size=rel(2.2)),
                legend.key.size = unit(1, "cm"), legend.text = element_text(size=rel(1.2)),
                legend.title = element_text(size = rel(1.5)))
      }
      else {
      gg <-  ggplot(data = happy) + geom_mosaic(aes_string( weight = "wtssall", x = xstr(), conds=cond(), fill = input$col ),
                                                offset = as.numeric(input$offset), divider=ddecker())+
        theme(axis.text.x = element_text(size=rel(1.7), angle = 25, hjust=1), axis.title.x = element_text(size=rel(2.2)),
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


  output$mosaicplot = renderPlot({ plot()   })

 # output$formula<- renderText({
   # ggplot_build(plot())$data[[1]]["formula"][1,]
  #  vars <- xstr()
  #  vars <- gsub("product[:(:]", "", vars)
  #  vars <- gsub(")", "", vars)
  #  vars <- gsub(",", " + ", vars)

  #  paste("weight ~",  vars)
  #  })


})
