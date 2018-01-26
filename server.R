

source("codefile_shiny.R")


server <- function(session, input, output) {
  
  # 1. Front page ----
  
  output$p_bar <- renderPlotly({national_bars('P')})
  
  output$f_bar <- renderPlotly({national_bars('F')})
  
  # 2. Reason ----
  
  output$perm_reason <- renderPlot({perm_reason_bar(input$reasonschtype)})
  output$fixed_reason <- renderPlot({fixed_reason_bar(input$reasonschtype)})
  
  output$perm_reason_t <- renderTable({perm_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  output$fixed_reason_t <- renderTable({fixed_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  

  # 2. Characteristics ----
  
  ## 2.1 FSM ----
  
  output$fsm_pri_table <- renderTable({
    if (input$fsm_table == "number") {
      fsm_sch_table_num("State-funded primary", input$select_cat_char)
    } else if (input$fsm_table == "rate") {
      fsm_sch_table_rate("State-funded primary", input$select_cat_char)
    }
  },  bordered = TRUE, spacing = 'm')
  
  output$fsm_sec_table <- renderTable({
    if (input$fsm_table == "number") {
      fsm_sch_table_num("State-funded secondary", input$select_cat_char)
    } else if (input$fsm_table == "rate") {
      fsm_sch_table_rate("State-funded secondary", input$select_cat_char)
    }
  },  bordered = TRUE, spacing = 'm')
  
  output$fsm_spec_table <- renderTable({
    if (input$fsm_table == "number") {
      fsm_sch_table_num("Special", input$select_cat_char)
    } else if (input$fsm_table == "rate") {
      fsm_sch_table_rate("Special", input$select_cat_char)
    }
  },  bordered = TRUE, spacing = 'm')
  
  output$fsm_tot_table <- renderTable({
    if (input$fsm_table == "number") {
      fsm_sch_table_num("Total", input$select_cat_char)
    } else if (input$fsm_table == "rate") {
      fsm_sch_table_rate("Total", input$select_cat_char)
    }
  },  bordered = TRUE, spacing = 'm')
  
  output$fsm_gap <- renderPlot({fsm_gap(input$select_cat_char)})
  
  output$fsm_prop_plot <- renderPlotly({fsm_prop(input$select_cat_char)})
  
  output$downloadfsmchar <- downloadHandler(
    filename = function() {
      paste("fsm_exclusions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fsmchar, file, row.names = FALSE)
    }
  ) 
  
  ## 2.2 SEN ----
  
  output$sen_pri_table <- renderTable({
    if (input$sen_table == "number") {
      sen_sch_table_num("State-funded primary",input$select_cat_sen)
    } else if (input$sen_table == "rate") {
      sen_sch_table_rate("State-funded primary",input$select_cat_sen)
    }
  },  bordered = TRUE,spacing = 'm')
  
  output$sen_sec_table <- renderTable({
    if (input$sen_table == "number") {
      sen_sch_table_num("State-funded secondary",input$select_cat_sen)
    } else if (input$sen_table == "rate") {
      sen_sch_table_rate("State-funded secondary",input$select_cat_sen)
    }
  },  bordered = TRUE,spacing = 'm')
  
  output$sen_spec_table <- renderTable({
    if (input$sen_table == "number") {
      sen_sch_table_num("Special",input$select_cat_sen)
    } else if (input$sen_table == "rate") {
      sen_sch_table_rate("Special",input$select_cat_sen)
    }
  },  bordered = TRUE,spacing = 'm')
  
  output$sen_tot_table <- renderTable({
    if (input$sen_table == "number") {
      sen_sch_table_num("Total",input$select_cat_sen)
    } else if (input$sen_table == "rate") {
      sen_sch_table_rate("Total",input$select_cat_sen)
    }
  },  bordered = TRUE, spacing = 'm')
  
  output$sen_gap <- renderPlot({sen_gap(input$select_cat_sen)})
  
  output$sen_prop_plot <- renderPlotly({sen_prop(input$select_cat_sen)})
  
  output$downloadsenchar <- downloadHandler(
    filename = function() {
      paste("sen_exclusions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(senchar, file, row.names = FALSE)
    }
  ) 
  
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
    
    school_summary_table %>%
      filter(
        la_name == input$la_name_rob,
        laestab == input$laestab_rob
      )
    
  } )
  
  la_schools <- reactive({school_summary_table %>% filter(la_name == la_name_rob)})
  
  updateSelectizeInput(
    session = session, 
    inputId = 'laestab_rob',
    choices = school_summary_table$laestab[school_summary_table$la_name == "City of London"],
    server = TRUE)
  
  observe({
    updateSelectizeInput(
      session = session, 
      inputId = 'laestab_rob',
      choices = school_summary_table$laestab[school_summary_table$la_name == input$la_name_rob],
      server = TRUE)
  })
  
  
  
  
  
  session$onSessionEnded(function() { stopApp() })
  
}

