
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("R/")

shinyServer(function(session, input, output) {
  
  # 1. Front page ----
  
  output$p_bar <- renderPlot({
    if (input$bars_type == "number") {
      national_bars_num('P')
    } else if (input$bars_type == "rate") {
      national_bars_rate('P')
    }
  })
  
  output$f_bar <- renderPlot({
    if (input$bars_type2 == "number") {
      national_bars_num('F')
    } else if (input$bars_type2 == "rate") {
      national_bars_rate('F')
    }
  })
  

  # 2. Reason ----
  
  output$perm_reason <- renderPlot({perm_reason_bar(input$reasonschtype)})
  output$fixed_reason <- renderPlot({fixed_reason_bar(input$reasonschtype)})
  
  output$perm_reason_t <- renderTable({perm_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  output$fixed_reason_t <- renderTable({fixed_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  

  # 2. Characteristics ----
  
  output$char_ts <- renderPlot({char_series(input$char_char, input$char_sch, input$char_cat)})
  
  
  output$char_ts_age <- renderPlot({char_series_age(input$char_char, input$char_sch, input$char_cat, input$line)})
  
  output$char_ts_ethn <- renderPlot({char_series_ethn(input$char_char, input$char_sch, input$char_cat, input$Radio_Button_Ethn_Fac_1, input$Check_Button_Ethn_Fac_2)})
  
  output$char_ts_table <- renderDataTable({char_series_table(input$char_char, input$char_sch, input$char_cat, input$table_ethn_measure)}, 
                                          rownames = FALSE, 
                                          extensions = c('Buttons'),
                                          options = list(pageLength = 30,
                                                         dom = 't',
                                                         buttons = c('csv','copy'),
                                                         initComplete = JS(
                                                           "function(settings, json) {",
                                                           "$(this.api().table().header()).css({'background-color': '#ffffff', 'color': '#000'});",
                                                           "}")))
  
  output$char_prop <- renderPlotly({char_prop(input$char_char, input$char_sch, input$char_cat)})
  
  output$char_gaps <- renderPlot({char_gaps(input$char_char, input$char_sch, input$char_cat)})
  
  
  
  
  output$bar_chart <- renderPlot({bar_chart_percentages(input$char_char, input$char_sch, input$char_cat)})
  
  mydata <- ethnicity_data(nat_char_prep)
  
  values <- reactiveValues(cb = with(mydata, setNames(rep(FALSE, nlevels(characteristic_1)), levels(characteristic_1))))
  
  observeEvent(input$Check_Button_Ethn_Fac_2, {
    myFactor2_list<-mydata$characteristic_1[mydata$ethnic_level==input$Radio_Button_Ethn_Fac_1]
    values$cb[myFactor2_list] <- myFactor2_list %in% input$Check_Button_Ethn_Fac_2
  })
  
  
  
  observe({
    factor1Choice<-input$Radio_Button_Ethn_Fac_1
    
    myFactor2_list<-mydata$characteristic_1[mydata$ethnic_level==factor1Choice]
    
    updateCheckboxGroupInput(session, "Check_Button_Ethn_Fac_2",
                             choices = myFactor2_list,
                             selected = c("Total", "Black Total", "White British", "Indian", "Black Caribbean"))
    
    mydata2<-mydata[mydata$ethnic_level==factor1Choice,]
    
  #  output$char_ts_ethn <- renderPlot({char_series_ethn(input$char_char, input$char_sch, input$char_cat, input$myFactor2_list)})
    
  })
  
  
  
  
  
  # 3. LA trends ----
  
  output$t1_chart <- renderPlot({
    if (input$plot_type == "number") {
      la_plot_num(input$select2, input$select_cat)
    } else if (input$plot_type == "rate") {
      la_plot_rate(input$select2, input$select_cat)
    }
  })
  
  output$t1_table <- renderTable({
    if (input$plot_type == "number") {
      la_table_num(input$select2, input$select_cat)
    } else if (input$plot_type == "rate") {
      la_table_rate(input$select2, input$select_cat)
    }
  },
  bordered = TRUE,spacing = 'm',align = 'c')
  
  output$la_title <- renderText({paste(input$select2," summary")})
  
  output$la_perm <- renderText({paste("The number of permanent exclusions increased from ",la_perm_num(input$select2,201415),
                                      " (",la_perm_rate(input$select2,201415)," per cent) in 2014/15 to ",la_perm_num(input$select2,201516),
                                      " (",la_perm_rate(input$select2,201516), " per cent) in 2015/16, 
                                      which is equivalent to ", as.numeric(la_perm_rate(input$select2,201516))*100, " pupils per 10,000.")})
  
  output$la_fixed <- renderText({paste("The number of fixed period exclusions increased from ",la_fixed_num(input$select2,201415),
                                       " (",la_fixed_rate(input$select2,201415)," per cent) in 2014/15 to ",la_fixed_num(input$select2,201516),
                                       " (",la_fixed_rate(input$select2,201516), " per cent) in 2015/16, 
                                       which is equivalent to ", as.numeric(la_fixed_rate(input$select2,201516))*100, " pupils per 10,000.")})
  
  output$la_one_plus <- renderText({paste("The number of pupil enrolments with at least one fixed term exclusion increased from ",
                                          la_one_plus_num(input$select2,201415),
                                          " (",la_one_plus_rate(input$select2,201415)," per cent) in 2014/15 to ",la_one_plus_num(input$select2,201516),
                                          " (",la_one_plus_rate(input$select2,201516), " per cent) in 2015/16, 
                                          which is equivalent to ", as.numeric(la_one_plus_rate(input$select2,201516))*100, " pupils per 10,000.")})
  
  # 4. Map ----
  
  output$map <- renderLeaflet({excmap(input$select_map)})
  
  # 5. Methods ----

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(la_sch_table(input$select2), file, row.names = FALSE)
    }
  )
  
  output$downloadmain_ud <- downloadHandler(
    filename = function() {
      paste("national_region_la_school_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(main_ud, file, row.names = FALSE)
    }
  )  
  
  output$downloadreason_ud <- downloadHandler(
    filename = function() {
      paste("reason_for_exclusion", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reason_ud, file, row.names = FALSE)
    }
  )  
  
  output$downloadnatchar_ud <- downloadHandler(
    filename = function() {
      paste("national_characteristics", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(char_ud, file, row.names = FALSE)
    }
  ) 
  
   # 6. School summary tab ----
  
  output$table_school_summary <- renderDataTable({
    
    # Filter
    
    all_schools_data %>%
      filter(
        la_name == input$la_name_rob,
        EstablishmentName == input$EstablishmentName_rob
      )
    
  } )
  
  la_schools <- reactive({all_schools_data %>% filter(la_name == la_name_rob)})
  
  updateSelectizeInput(
    session = session, 
    inputId = 'EstablishmentName_rob',
    choices = all_schools_data$EstablishmentName[all_schools_data$la_name == "Barking and Dagenham"],
    server = TRUE)
  
  observe({
    updateSelectizeInput(
      session = session, 
      inputId = 'EstablishmentName_rob',
      choices = all_schools_data$EstablishmentName[all_schools_data$la_name == input$la_name_rob],
      server = TRUE)
  })
  
  #stop app running when closed in browser
  session$onSessionEnded(function() { stopApp() })
  
})

