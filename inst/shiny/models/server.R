library(shiny)
library(ggmosaic)
library(rlang)
library(stringr)
library(tidyverse)
library(DT)
library(broom)

## LOAD DATA
# install_github("haleyjeppson/ggmosaic", ref = "data")
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
  scale_fill_manual(..., values = mypal, guide = guide_legend(reverse = TRUE))
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
  observeEvent(input$keyPressedUp, {
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
  observeEvent(input$keyPressedDown, {
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
  observeEvent(input$keyPressedRight, {
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
  observeEvent(input$keyPressedLeft, {
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
  observeEvent(input$keyPressedH, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "v", "h")
      div_index$values[var_index$length] <- new_div
    }
  })

  ## switch vspine OR vbar
  observeEvent(input$keyPressedV, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "h", "v")
      div_index$values[var_index$length] <- new_div
    }
  })

  # switch to spine
  observeEvent(input$keyPressedS, {
    if(var_index$length > 0){
      current_div <- div_index$values[var_index$length]
      new_div <- str_replace(current_div, "bar", "spine")
      div_index$values[var_index$length] <- new_div
    }
  })

  ## switch to bar
  observeEvent(input$keyPressedB, {
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

  # output$mosaicplot = metaRender(renderPlot, {
  #   # browser()
  #   ind <- ..(var_index$length)
  #   if(ind == 0){
  #     metaExpr({
  #     ggplot(data = ..(data_vars())) + geom_mosaic(aes_string(weight = "weight", x = "product(1)", fill = "1")) +
  #       scale_fill_manual(values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE)) +
  #       theme_mosaic()
  #     })
  #   }
  #   else{
  #     metaExpr({
  #     vars <- syms(..(var_selection()))
  #     fill_var <- sym(..(fill_variable()))
  #     # metaExpr({
  #       ggplot(data = ..(data_vars())) + geom_mosaic(aes(weight = weight, x = product(!!!vars), fill = !!fill_var),
  #                                              divider = ..(divs())) +
  #       scale_fill_manual(values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE)) +
  #       theme_mosaic()
  #     })
  #   }
  # })

  ## CODE FOR MOSAIC PLOT ------------------------------------------------------------------------------------


  plotcode <- reactive({
    # happy <- happy[complete.cases(happy),]
    # fly <- fly[complete.cases(fly),]
    # titanic <- titanic[complete.cases(titanic),]
    #
    # fly$do_you_recline <- forcats::fct_collapse(fly$do_you_recline,
    #                                             usually = c("usually", "always"),
    #                                             sometimes = c("about half the time", "once in a while"))
    # data_vars <- data_selected() %>%
    #   mutate_if(is.character, as.factor) %>%
    #   select_if(is.factor) %>%
    #   select_if(check_levels) %>%
    #   mutate(weight = !!weight_var())
    # browser()
    weight <- switch(input$dataset,
           "happy" = "wtsall",
           "fly" = "1",
           "titanic" = "1")
    setup <- paste0('library(ggmosaic)', '\n',
                    'mypal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00", "#CC79A7")', '\n\n')

    if(var_index$length == 0){
      plotcode <- paste0(setup,
                         'ggplot(data = ', input$dataset, ') +
  geom_mosaic(aes(weight = ', weight, ', x = product(1), fill = 1)) +
  scale_fill_manual(NULL, values = mypal, guide = guide_legend(reverse = TRUE)) +
  theme_mosaic()')
    }
    else{
      plotcode <-  paste0(setup,
                          'ggplot(data = ', input$dataset, ') +
  geom_mosaic(aes(weight = ', weight, ', x = product(', paste(var_selection(), collapse=', '), '), fill = ', fill_variable(), '),
    divider = c("', paste(divs(), collapse='", "'), '")) +
  scale_fill_manual(values = mypal, guide = guide_legend(reverse = TRUE)) +
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

  # output$avail <- render_gt(
  #   data_frame(Variables = names_vars()) %>%
  #     #mutate(Selected = Variables %in% var_selection()) %>%
  #     #mutate(Selected = factor(Selected)) %>%
  #     gt()# rowname_col = "Variables")# %>%
  #     # tab_row_group(
  #     #   group = "selected",
  #     #   rows = Variables %in% var_selection()
  #     # ) %>%
  #     # tab_row_group(
  #     #   group = "Available",
  #     #   rows = !(Variables %in% var_selection())
  #     # )
  #     # data_color(columns = vars(Variables, Selected),
  #     #            colors = scales::col_factor(
  #     #              palette = c(
  #     #                "white", "grey80"),
  #     #              domain = c(0, 1))) #%>%
  #     #cols_hide(Selected) #data_color(columns = Variables)
  # )

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

  #updateSelectizeInput(session, 'vars', choices = var_selection(), selected = NULL)

  ## MODELING PROCESS ---------------------------------------------------------------------------------------------
  output$model_formula <- renderText({
    model_form()
  })
  output$interact <- renderText({
    input$interaction_opts
  })
  ## formula for model
  model_form <- reactive({
    formula <- form()
    if(is.null(input$interaction_opts)) {
      formula <- formula
    } else {
      interacts <- paste0(input$interaction_opts, collapse = " + ")
      formula <- paste0(formula, " + ", interacts)
    }
    formula
  })

  # save the model information
  model_info <- eventReactive({input$run_model | input$update_model}, {
    mod_dat <- data_vars() %>%
      group_by(!!!syms(var_selection())) %>%
      summarise(weight = sum(weight)) %>%
      ungroup()

    mod <- glm(model_form(), data = mod_dat, family = "poisson")
    mod
  })

  # print table for model deviance info
  output$model_info <- gt::render_gt(
    anova(model_info()) %>%
      as.data.frame() %>%
      rownames_to_column(var = "Term") %>%
      mutate_if(is_numeric, round, 2) %>%
      mutate(pvalue = 1 - pchisq(`Resid. Dev`, `Resid. Df`),
             pvalue = format.pval(pvalue, digits = 2)) %>%
      rename(p.value = pvalue)
  )

  # print table for model coefficient info
  output$model_coeffs <- gt::render_gt(
    tidy(model_info()) %>%
      mutate(p.value = format.pval(p.value, digits = 2)) %>%
      mutate_if(is_numeric, round, 2)
    )

  # calculate all the modeled data
  model_dat <- eventReactive({input$run_model | input$update_model}, {
     mod_dat <- data_vars() %>%
       group_by(!!!syms(var_selection())) %>%
       summarise(weight = sum(weight)) %>%
       ungroup()

      mod <- glm(model_form(), data = mod_dat, family = "poisson")

      mod_dat %>%
        mutate(fit =  round(predict(mod, type = "response"),2),
               resid_raw = round((weight - fit),2),
               resid_pearson = round(resid_raw/sqrt(fit),2),
               resid_sign = sign(resid_raw),
               resid_sign = ifelse(resid_sign <0, "neg_resid", "pos_resid"))
  })


  output$modelplot = renderPlot({
    if(!is.null(model_dat)){
      ggplot(model_dat()) +
        geom_mosaic(aes(weight = fit, x = product(!!!syms(var_selection())), fill = !!sym(fill_variable())),
                                               divider = divs()) +
        scale_fill_manual(values = mypal, na.value = "azure4", guide = guide_legend(reverse = TRUE)) +
        theme_mosaic()
    }

  })

  ## select type of residual
  resid_var <- reactive({
    switch(input$residual_type,
           "Observed values" = model_dat()$weight,
           "Raw residuals" = abs(model_dat()$resid_raw),
           "Pearson residuals" = abs(model_dat()$resid_pearson))
  })

  # add jittered points for residual plot
  output$residplot = renderPlot({
    if(!is.null(model_info())){

      model_plot <- ggplot(model_dat()) +
        geom_mosaic(aes(weight = fit, x = product(!!!syms(var_selection()))),
                    divider = divs(), alpha = .15) +
        scale_color_manual(values = c("#D43F3A", "#398CC4"), na.value = "grey30")+
        # scale_color_manual(values = c(mypal[c(1,6)]), na.value = "grey30") +
        scale_fill_manual(values = mypal, na.value = "azure4") +
        theme_mosaic()

      if(input$residual_type == "Observed values"){
        model_plot + geom_mosaic_jitter(aes(weight2 = resid_var(), weight = fit, x = product(!!!syms(var_selection()))), size = 2, alpha=.8)
      } else {
        model_plot + geom_mosaic_jitter(aes(weight2 = resid_var(), weight = fit, x = product(!!!syms(var_selection())),
                                 colour = resid_sign), size = 2, alpha=.8)
      }
    }

  })

  ## INTERACTIONS ------------------------------------------------------------------------


  ## CHECKBOX OPTIONS --------------------------------------------------------------------
  inter_opts <- function(vars){
    paste0(vars, collapse  = ":")
  }

  create_interactions <- function(vars){
    if(length(vars) < 2){
      return(NULL)
    } else {
    combos <- NULL
    for(i in 2:length(vars)){
      vars_combos <- t(combn(vars, i))
      combos <- c(combos, apply(vars_combos, 1, inter_opts))
    }
    return(combos)
  }}


  observe({
      var_interactions <- create_interactions(var_selection())

      updateCheckboxGroupInput(session, "interaction_opts",
                               label = "Select interactions:",
                               choices = var_interactions,
                               selected = NULL
      )
  })


})

