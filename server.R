#server.R

#---------------------------------------------------------------------
#Load required code  

source("R/2. overview_tab.R")
source("R/characteristics_tab.R")
source("R/general.R")
source("R/gov_colours.R")
source("R/la_trends_tab.R")
source("R/map_tab.R")
source("R/reason_tab.R")
source("R/school_tab.R")

#---------------------------------------------------------------------
#Server

shinyServer(function(session, input, output) {

#-------------------------------------------------------------------    
#Front page
  
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
  
#------------------------------------------------------------------- 
#Reason
  
  staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}') 
  
  output$tbl <- DT::renderDataTable({
    
    line_string <- "type: 'line', width: '220px', height: '40px', chartRangeMin: 0"
    
    cd <- list(list(targets = 11, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))
    
    cb = JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ", 
                   line_string, " });\n}"), collapse = "")
    
    staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}') 
    
    dt <- DT::datatable(as.data.frame(exclusion_reason_table(input$la_name_exclusion_select, input$schtype, input$exclusion_type)[,4:15]),
                        rownames = FALSE, 
                        options = list(columnDefs = cd,
                                       fnDrawCallback = cb,
                                       drawCallback = staticRender_cb,
                                       pageLength = 12,
                                       dom = 't'
                                       ))
    dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency("sparkline"))
    dt              
  })
  
#------------------------------------------------------------------- 
#Characteristics
  
  output$char_ts <- renderPlot({char_series(input$char_char, input$char_sch, input$char_cat)})
  
  
  output$char_ts_age <- renderPlot({char_series_age(input$char_char, input$char_sch, input$char_cat, input$line)})
  
  output$char_ts_ethn <- renderPlot({char_series_ethn(input$char_char, input$char_sch, input$char_cat, input$table_ethn_measure, input$Check_Button_Ethn_Fac_2)})
  
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
  
  output$bar_chart <- renderPlot({bar_chart_percentages(input$char_char, input$char_sch, input$char_cat)})
  
  mydata <- ethnicity_data(nat_char_prep)
  
  values <- reactiveValues(cb = with(mydata, setNames(rep(FALSE, nlevels(characteristic_1)), levels(characteristic_1))))
  
  observeEvent(input$Check_Button_Ethn_Fac_2, {
    myFactor2_list<-mydata$characteristic_1[mydata$ethnic_level==input$table_ethn_measure]
    values$cb[myFactor2_list] <- myFactor2_list %in% input$Check_Button_Ethn_Fac_2
  })
  
  observe({
    factor1Choice<-input$table_ethn_measure
    
    myFactor2_list<-mydata$characteristic_1[mydata$ethnic_level==factor1Choice]
    
    updateCheckboxGroupInput(session, "Check_Button_Ethn_Fac_2",
                             choices = myFactor2_list,
                             selected = c("Total", "Black Total", "White British", "Indian", "Black Caribbean"))
    
    mydata2<-mydata[mydata$ethnic_level==factor1Choice,]
    
  })
  
  output$download_characteristics_data <- downloadHandler(
    filename = function() {
      paste(input$char_char, "_characteristics_data", ".csv", sep = "") 
    },
    content = function(file) {
      write.csv(characteristics_data_download(input$char_char), file, row.names = FALSE)
    }
  )
  
#------------------------------------------------------------------- 
#LA trends
  
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
  
  output$la_comparison_chart <- renderPlot({la_compare_plot(input$select2, input$select_cat)})
  
  output$la_comparison_table <- renderTable({la_compare_table(input$select2, input$select_cat)},
                                            bordered = TRUE,spacing = 'm',align = 'c')
  
  
  output$la_data_download_tab_1 <-  downloadHandler(
    filename = function() {
      paste(input$select2, "_exclusion_data", ".csv", sep = "") 
    },
    content = function(file) {
      write.csv(clean_la_data_download_tab_1(main_ud, input$select2) , file, row.names = FALSE)
    }
  )
  
  output$la_data_download_tab_2 <-  downloadHandler(
    filename = function() {
      paste(input$select2, "_exclusion_data", ".csv", sep = "") 
    },
    content = function(file) {
      write.csv(comparison_la_data_download_tab_2(main_ud, input$select2) , file, row.names = FALSE)
    }
  )
  
#------------------------------------------------------------------- 
#Map
  
  output$map <- renderLeaflet({excmap(input$select_map)})

#-------------------------------------------------------------------     
#Reason for exclusion
  
  output$download_reason_for_exclusion <-  downloadHandler(
    filename = function() {
      paste("area_exclusion_reason_data", ".csv", sep = "") 
      },
    content = function(file) {
      write.csv(exclusion_reason_table_download(input$la_name_exclusion_select), file, row.names = FALSE)
    }
  )
  
#------------------------------------------------------------------- 
#Methods

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
  
#------------------------------------------------------------------- 
#School summary tab
  
  output$table_school_summary <- renderDataTable(
    
    # Filter
    
    all_schools_data %>%
      filter(
        la_no_and_name == input$la_name_rob,
        laestab_school_name == input$EstablishmentName_rob
      ), 
    extensions = c('Buttons'), 
    options=list(dom = 't',
                 buttons = c('csv','copy'),
                 columnDefs = list(list(visible=FALSE, targets=c(2,3,12,13,14,15)))))
  
  
  
  la_schools <- reactive({all_schools_data %>% filter(la_no_and_name == la_name_rob)})
  
  updateSelectizeInput(
    session = session, 
    inputId = 'EstablishmentName_rob',
    choices = all_schools_data$laestab_school_name[all_schools_data$la_no_and_name == "301 - Barking and Dagenham"],
    server = TRUE)
  
  observe({
    updateSelectizeInput(
      session = session, 
      inputId = 'EstablishmentName_rob',
      choices = all_schools_data$laestab_school_name[all_schools_data$la_no_and_name == input$la_name_rob],
      server = TRUE)
  })
  
  output$school_data_download <-  downloadHandler(
    filename = function() {
      paste(substr(input$EstablishmentName_rob, 1, 7), "_exclusion_data", ".csv", sep = "") 
    },
    content = function(file) {
      write.csv(school_summary_table %>% filter (laestab == substr(input$EstablishmentName_rob, 1, 7)) , file, row.names = FALSE)
    }
  )
  
#------------------------------------------------------------------- 
#stop app running when closed in browser
  
  session$onSessionEnded(function() { stopApp() })
  

})


