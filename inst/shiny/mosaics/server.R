library(shiny)
library(ggmosaic)
library(rlang)
library(stringr)
library(tidyverse)
library(DT)

## LOAD DATA
# install_github("haleyjeppson/ggmosaic")
data(happy, package = "ggmosaic")
data(fly, package = "ggmosaic")
data(titanic, package = "ggmosaic")

##  remove NAs
happy <- happy[complete.cases(happy),]
fly <- fly[complete.cases(fly),]
titanic <- titanic[complete.cases(titanic),]

fly$do_you_recline <- forcats::fct_collapse(fly$do_you_recline,
                                            usually = c("usually", "always"),
                                            sometimes = c("about half the time", "once in a while"))

## color pallette & theme
theme_set(theme_mosaic())
mypal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00", "#CC79A7")#colorRampPalette(pal_locuszoom("default", .9)(7))(9)
# options(ggplot2.discrete.fill = mypal)
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE))
}

## TODO:
## need to figure out how to remove variables already used from the options for new variables
## add in translation from formula to math
## arrow keys

shinyServer(function(input, output, session) {

  ## SET-UP UI ---------------------------------------------------------------------------------------------
      observeEvent(input$dataset,{
        updateSelectizeInput(session, 'response_var', choices = names_vars(), selected = NULL)
      })


  ## PREPROCESS DATA ----------------------------------------------------------------------------------------
  ## grab selected data set
  data_selected <- reactive({
    switch(input$dataset,
           "happy" = happy,
           "fly" = fly,
           "titanic" = titanic)
  })

  ## identify potential weight variable in data
  weight_var <- reactive({
    dat <- data_selected()
    switch(input$dataset,
           "happy" = dat$wtssall,
           "fly" = 1,
           "titanic" = 1)
  })
  # will need to be changed to allow for more generalized data

  ## mutate character into factor, then keep only the factors with less than 10 levels
  check_levels <- function(x) length(levels(x)) < 10

  data_vars <- eventReactive(input$dataset, {
      data_vars <- data_selected() %>%
        mutate_if(is.character, as.factor) %>%
        select_if(is.factor) %>%
        select_if(check_levels) %>%
      mutate(weight = !!weight_var())
  })

  n_vars <- eventReactive(input$dataset, {
    length(data_vars()) -1
  })

  names_vars <- eventReactive(input$dataset, {
    tmp <- data_vars() %>% select(-weight)
    names(tmp)
  })



  ## VARIABLE SELECTION -------------------------------------------------------------------------------------
  ## set up reactive values
  var_index <- reactiveValues(values = 1, length = 0, select = 1)
  div_index <- reactiveValues(values = 1, length = 0, select = 1)

  ## add variable
  observeEvent(input$keyPressedUp | input$key_up, {
    var_index$length <- var_index$length + 1
    new_index_length <- var_index$length
    ## check for duplicates
    new_val <- 1
    while(new_val %in% var_index$values){
      new_val <- ((new_val) %% n_vars()) + 1
    }
    var_index$values[new_index_length] <- new_val
    ## need to also add to divider
    divs <- ggmosaic::mosaic()(new_index_length)[1]
    div_index$values[new_index_length] <- divs
  })

  ## remove variables when data changes
  observeEvent(input$dataset, {
    var_index$length = 0
    var_index$values <- var_index$values[1]
    div_index$values <- div_index$values[1]
  })


  ## remove variable
  observeEvent(input$keyPressedDown | input$key_down, {
    if(var_index$length > 0){
      var_index$length <- var_index$length - 1
      if(var_index$length > 0){
        new_index_length <- var_index$length
        var_index$values <- var_index$values[1:new_index_length]
        ## need to also remove from divider
        div_index$values <- div_index$values[1:new_index_length]
      }
    }
  })

  ## switch variable right
  observeEvent(input$keyPressedRight | input$key_right, {
    if(var_index$length > 0){
      var_index$select <- (var_index$select + 1) %% n_vars()
      counter_position <- as.numeric(var_index$select)
      variable_position <- (counter_position %% n_vars()) + 1
      last_position <- var_index$length
      while(variable_position %in% var_index$values){
        variable_position <- ((variable_position) %% n_vars()) + 1
      }
      var_index$values[last_position] <- variable_position
    }
  })

  ## switch variable left
  observeEvent(input$keyPressedLeft | input$key_left, {
    if(var_index$length > 0){
        var_index$select <- (var_index$select - 1) %% n_vars()
      counter_position <- as.numeric(var_index$select)
      variable_position <- (counter_position %% n_vars()) + 1
      last_position <- var_index$length
      while(variable_position %in% var_index$values){
        variable_position <- ((variable_position-2) %% n_vars()) +1
      }
      var_index$values[last_position] <- variable_position
    }
  })

  ## switch hspine OR hbar
  observeEvent(input$keyPressedH | input$key_h, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "v", "h")
      div_index$values[var_index$length] <- new_div
    }
  })

  ## switch vspine OR vbar
  observeEvent(input$keyPressedV | input$key_v, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "h", "v")
      div_index$values[var_index$length] <- new_div
    }
  })

  # switch to spine
  observeEvent(input$keyPressedS | input$key_s, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "bar", "spine")
      div_index$values[var_index$length] <- new_div
    }
  })

  ## switch to bar
  observeEvent(input$keyPressedB | input$key_b, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "spine", "bar")
      div_index$values[var_index$length] <- new_div
    }
  })


  ## names of variables that have been selected
  var_selection <- reactive({
    vars <- names_vars()[var_index$values]
    rev(vars)
  })


  ## vector of dividers
  divs <- reactive({
    # if(!input$response) {
    divs <- div_index$values
    rev(divs)
    # }
    # else{
    #   new_length <- length(div_index$values) + 1
    #   add_div <- ggmosaic::mosaic()(new_length)[1]
    #   div_index$values[new_length] <- add_div
    #
    # }
  })


  fill_variable <- reactive({
    #if(!input$response){
    vars <- var_selection()
    selected <- vars[1]
    as.character(selected)
    # }
    # else{
    #   as.character(input$response_var)
    # }
  })

  ## FIRST MOSAIC PLOT -----------------------------------------------------------------------------------------
  output$mosaicplot = renderPlot({
    ind <- var_index$length
    if(ind == 0){
      ggplot(data = data_vars()) + geom_mosaic(aes_string(weight = "weight", x = "product(1)", fill = "1")) +
        scale_fill_manual(values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE)) +
        theme_mosaic()
    }
    else{
      vars <- syms(var_selection())
      fill_var <- sym(fill_variable())
      ggplot(data = data_vars()) +
        geom_mosaic(aes(weight = weight, x = product(!!!vars), fill = !!fill_var), divider = divs()) +
        scale_fill_manual(values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE)) +
        theme_mosaic()
    }
  })


  ## CODE FOR MOSAIC PLOT ------------------------------------------------------------------------------------
  plotcode <- reactive({
    # happy <- happy[complete.cases(happy),]
    # fly <- fly[complete.cases(fly),]
    # titanic <- titanic[complete.cases(titanic),]
    #
    # fly$do_you_recline <- forcats::fct_collapse(fly$do_you_recline,
    #                                             usually = c("usually", "always"),
    #                                             sometimes = c("about half the time", "once in a while"))


    weight <- switch(input$dataset,
           "happy" = "wtsall",
           "fly" = "1",
           "titanic" = "1")
    setup <- paste0('library(ggmosaic)', '\n',
                    'mypal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00", "#CC79A7")', '\n\n')

    if(var_index$length == 0){
      plotcode <- paste0(#setup,
                         'ggplot(data = ', input$dataset, ') +
  geom_mosaic(aes(weight = ', weight, ', x = product(1), fill = 1)) +
  theme_mosaic()')
    }
    else{
      plotcode <-  paste0(#setup,
                          'ggplot(data = ', input$dataset, ') +
  geom_mosaic(aes(weight = ', weight, ', x = product(', paste(var_selection(), collapse=', '), '), fill = ', fill_variable(), '),
    divider = c("', paste(divs(), collapse='", "'), '")) +
  theme_mosaic()')
    }
    plotcode
  })

  output$code <- renderText({
    plotcode()
  })

  ## FORMULA FOR MOSAIC PLOT ------------------------------------------------------------------------------------
  form <- reactive({
    if(var_index$length == 0){
      formula <- "1"
    }
    else{
      formula <-  paste(rev(var_selection()), collapse=" + ")
    }
    formula <- paste0("weight ~ ", formula)
    formula
  })

  output$formula <- renderText({
      form()
  })


  ## HELPERS? --------------------------------------------------------------------------------------------------
  output$length <- renderText({
    var_index$length
  })

  output$values <- renderText({
    var_index$values
  })

  output$datatable <- DT::renderDT(
    data_vars(),
    options = list(
      pageLength = 10,
      initComplete = I("function(settings, json) {alert('Done.');}")
    )
  )

  output$selection <- renderText({
    rev(var_selection() )
  })

  output$fill <- renderText({
    if(var_index$length == 0) NULL
    else fill_variable()
  })
  output$select <- renderText({
    var_index$select
  })
  output$div <- renderText({
     div_index$values
  })



})

