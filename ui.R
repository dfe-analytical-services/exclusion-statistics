sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("R/")

shinyUI(
    navbarPage("Exclusion statistics", 
               theme = "shiny.css", 
               header=singleton(tags$head(includeScript('www/google-analytics.js'))),
               
                   # 1. Front page ----
                   
                   tabPanel("Overview",
                            sidebarLayout(
                              sidebarPanel(verticalLayout(
                                h3(strong("Exploring school exclusion statistics")),
                                h4(strong("(Proof of concept)")),
                                br("This tool is aimed at enabling users to further understand exclusions data and is currently under development."), 
                                hr(),
                                strong("Background"),
                                br("The purpose of this dashboard is to provide insight to lower level breakdowns included within our National Statistics release. 
                                It reports on permanent and fixed period exclusions from state-funded primary, state-funded secondary and special schools as 
                                reported via the School Census."),
                                br(strong("Latest National Statistics")),
                                br("All of the data used within this dashboard, including additional breakdowns, has been published in the",
                                  a("Permanent and fixed-period exclusions in England: 2015 to 2016", 
                                    href = "https://www.gov.uk/government/statistics/permanent-and-fixed-period-exclusions-in-england-2015-to-2016",
                                    target="_blank"), "National Statistics release's underlying data section and is also available for download via the data and methods tab."),
                                br(strong("Guidance and methodology")),
                                br("This dashboard shows breakdowns for the number and rate of permanent and fixed period exclusions as well as enrolments 
                                   receiving one or more fixed period exclusion. Rates are calculated using the number of sole and dual registered pupils on roll as of 
                                   Spring Census day. Further info, including definitions, is available in the data and methods tab."),
                                br(a("An exclusion statistics guide",
                                     href = "https://www.gov.uk/government/publications/exclusions-statistics-guide",
                                     target = "_blank"),"which provides historical information on exclusion statistics, technical background information to the figures 
                                and data collection, and definitions of key terms should be referenced alongside this dashboard."), 
                                br(strong("Definitons")),
                                br("Key defintions relating to statistics used in this application can be found in the data and methods tab.")
                              ), width = 5),
                              mainPanel(
                                br(),
                                strong("Permanent exclusions, 2006/07 to 2015/16"), 
                                br(),
                                em("State-funded primary, secondary and special schools"),
                                radioButtons("bars_type", label=NULL, c("rate", "number"), inline = TRUE),
                                plotOutput("p_bar", height ="8cm"),
                                hr(),
                                strong("Fixed period exclusions, 2006/07 to 2015/16"), 
                                br(),
                                em("State-funded primary, secondary and special schools"),
                                radioButtons("bars_type2", label=NULL, c("rate", "number"), inline = TRUE),
                                plotOutput("f_bar", height ="8cm"),
                                width = 7)),
                            hr(),
                            HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                    <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                    <br>
                    </br>')
             ),
             
             
             # 2. Pupil Characteristics ----
             
             tabPanel("Pupil characteristics",
                      h4(strong("Exclusions by pupil characteristic")),
                      sidebarLayout(sidebarPanel(
                        fluidRow(
                          column(4,
                                 selectInput("char_cat",
                                             label = "Select exclusion measure",
                                             choices = list(
                                               "Fixed period" = 'F',
                                               "Permanent" = 'P',
                                               "One or more fixed" = 'O'),
                                             selected = 'P'),
                                 
                                 selectInput("char_sch",
                                             label = "Select a school type",
                                             choices = list(
                                               "State-funded primary" = 'Primary',
                                               "State-funded secondary" = 'Secondary',
                                               "Special" = 'Special',
                                               "Total" = 'Total'),
                                             selected = 'Total')
                          ),
                          column(4,offset = 1,
                                 selectInput("char_char",
                                             label = "Select pupil characteristic",
                                             choices = list(
                                               "SEN provision" = 'sen',
                                               "FSM eligibility" = 'fsm',
                                               "Gender" = 'gender',
                                               "Age" = 'age',
                                               "Ethnicity" = 'ethn'),
                                             selected = 'gender'),
                                 downloadButton("download_characteristics_data", "Download the underlying data for the table below")
                          )
                        ), width = 12),
                        mainPanel(
                        #   tags$style(type="text/css",
                        #                      ".shiny-output-error { visibility: hidden; }",
                        #                      ".shiny-output-error:before { visibility: hidden; }"
                        # ),
                                  conditionalPanel(
                                    condition="input.char_char=='ethn'",
                                    radioButtons("table_ethn_measure", 
                                                 "Which measure of ethnicity?", 
                                                 c("Major Ethnic Grouping", "Minor Ethnic Grouping"), inline = TRUE)
                                  ),
                                  dataTableOutput("char_ts_table", width = "95%"),
                                  br(),
                                  br(),
                                  hr(),
                                  conditionalPanel(
                                    condition="input.char_char=='sen' | input.char_char=='fsm' | input.char_char=='gender'",
                                    plotOutput("char_ts", width="80%")
                                    
                                  ),
                                  conditionalPanel(
                                    condition="input.char_char=='age'",
                                    fluidRow (
                                      column(2, 
                                             checkboxGroupInput(inputId = "line",                                                                               
                                                                label = h4("What would you like to plot?"),                                                                       
                                                                choices = factor(reason_order_plot),
                                                                selected = c("Age 10","Age 14", "Total"))),
                                      column(8,
                                             br(),
                                             br(),
                                             br(),
                                             plotOutput("char_ts_age")))),
                                  
                                  
                                  conditionalPanel(
                                    condition="input.char_char=='ethn'",
                                    
                                    fluidRow(
                                      column(3, 
                                             
                                             checkboxGroupInput(inputId = "Check_Button_Ethn_Fac_2",                                                                               
                                                                label = h4("What would you like to plot?"),                                                                       
                                                                choices = List_Of_Ethnicities,
                                                                selected = c("Total", "Mixed Total"))),
                                      column(9,
                                             br(),
                                             br(),
                                             br(),
                                             plotOutput("char_ts_ethn")))),
                                  br(),
                                  hr(),
                                  plotOutput("bar_chart", width = "95%", height = '220px'),
                                  br(),
                                  width =12)), 
                       
             hr(),
             HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                  <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                  <br>
                  </br>')),

             
             # 3. LA Trends ----
             
             tabPanel(
               "Local Authority",
               sidebarLayout(
                 sidebarPanel(
                   h4(strong("Local Authority (LA) level exclusions")),
                   hr(),
                   h5(strong("Pick an area")),
                   selectInput("select2",
                     label = NULL,
                     list("England" = "England",
                          "Local Authority" = sort(unique((
                            main_ud$la_name[!is.na(main_ud$la_name) &
                                              main_ud$la_name != "."])))),
                     selected = "England"
                   ),
                   h5(strong("Pick an exclusion category")),
                   selectInput("select_cat",
                     label = NULL,
                     choices = list("Fixed" = 'F',
                                    "Permanent" = 'P',
                                    "One plus" = 'O'),
                     selected = 'F'
                   ),
                   hr(),
                   h5(strong(textOutput("la_title"))),
                   textOutput("la_perm"),
                   br(),
                   textOutput("la_fixed"),
                   br(),
                   textOutput("la_one_plus"),
                   hr(),
                   h5(strong("Notes")),
                   br(),
                   br(),
                   br(),
                   br()
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(
                     'Trend',
                     fluidRow(column(9,br(),
                                     column(3,
                                       radioButtons("plot_type", "Which measure?", c("rate", "number"), inline = TRUE)
                                     ))),
                     plotOutput("t1_chart", width = '23cm'),
                     br(),
                     tableOutput("t1_table"),
                     br(),
                     downloadButton("la_data_download_tab_1", "Download"),
                     br()),
                   tabPanel(
                     'Comparison to region and national',
                     br(),
                     strong("State-funded primary, secondary and special schools"),
                     br(),
                     br(),
                     plotOutput("la_comparison_chart", width = '23cm'),
                     br(),
                     tableOutput("la_comparison_table"),
                     br(),
                     downloadButton("la_data_download_tab_2", "Download"),
                     br())))
               ),
               hr(),
               HTML(
                 '<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                 <br>
                 <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                 <br>
                 </br>'
               )
             ), 
             
             # 4. Map ----
             
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Mapping exclusion rates")),
                          em("State-funded primary, secondary and special schools, 2015/16"),
                          br(),
                          br(),
                          h5(strong("Pick exclusion category")),
                          selectInput(
                            "select_map",
                            label = NULL,
                            choices = list("Permanent" = 'perm',
                                           "Fixed period" = 'fixed'),
                            selected = 'fixed'
                          ),
                          width = 3,
                          hr(),
                          h5(strong("Instructions")),
                          "From the dropdown menu above, please select the exclusion rate of interest. Then hover over your selected local authority to find out more information about exclusions data in that area.",
                          br(),
                          br(),
                          "The darkest shaded areas are in the top 20% of all local authorities for the selected exclusion rate and the lightest shaded areas in the bottom 20% for the selected exclusion rate."
                        ),
                        mainPanel(
                          leafletOutput("map", width = '25cm', height = '25cm') %>%
                            #spinner to appear while chart is loading
                            withSpinner(
                              color = "blue",
                              type = 5,
                              size = getOption("spinner.size", default = 0.4)
                            )
                        )
                      ),
                      hr(),
                      HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                           <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                           <br>
                           </br>')
                    ),

            
             #  # 5. Reason for exclusions ----
            
             tabPanel("Reason for exclusion",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Exclusions by reason")),
                          "Schools report exclusions broken down by reason",
                          fluidRow(
                            column(4,
                                   h5(strong("1. Pick an area")),
                                   selectInput("la_name_exclusion_select",
                                               label = NULL,
                                               list("England" = "England",
                                                    "Local Authority" = sort(unique((main_ud$la_name[!is.na(main_ud$la_name) & main_ud$la_name != "."])))),
                                               selected = "England",
                                               width='80%'),
                                   h5(strong("3. Pick an exclusion category")),
                                   selectInput("exclusion_type",
                                               label = NULL,
                                               choices = list(
                                                 "Fixed" = 'Fixed',
                                                 "Permanent" = 'Permanent'),
                                               selected = 'Fixed', width='80%')),
                            column(4,offset = 1,
                                   h5(strong("2. Pick a school type")),
                                   selectInput("schtype",
                                               label = NULL,
                                               choices = list(
                                                 "Primary" = 'State-funded primary',
                                                 "Secondary" = 'State-funded secondary',
                                                 "Special" = 'Special school',
                                                 "All schools" = 'Total'),
                                               selected = 'Total', width='80%'),
                                   br(),
                                   downloadButton("download_reason_for_exclusion", "Download underlying data for the table below")
                            )), width=12),
                        mainPanel(
                          htmlwidgets::getDependency('sparkline'),
                          #sparklineOutput("test_spark"),

                          DT::dataTableOutput("tbl", width = "95%"),
                          width=12
                        )),
                      hr(),
                      HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                    <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                    <br>
                    </br>')),
             
             # 6. Schools Summary 
             tabPanel("School level",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("School level exclusions")),
                          "The below table shows time series exclusion information for individual schools.",
                          "First, select the Local Authority the school sits in and then select the school of interest.",
                          br(),
                          br(),
                          selectInput("la_name_rob", label = "1. Select or type Local Authority name or 3 digit number" ,choices = sort(unique(all_schools_data$la_no_and_name)),  width='30%'),
                          selectizeInput("EstablishmentName_rob", label = "2. Select or type school name or LA/ESTAB number", choices = NULL, options = list(placeholder = "Select school", maxOptions = 50000),  width='30%'),
                          h5(strong("Note on suppresion")),
                          "Values of 'x' represent a value of less than three or a rate based upon a value lower than three, these figures are supressed for data protection purposes.",
                          br(),
                          br(downloadButton("school_data_download", "Download the underlying data for the table below")),
                          width=12),
                        mainPanel(
                          dataTableOutput("table_school_summary", width = "95%"),
                          width=12
                        )),
                      hr(),
                      HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                    <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                    <br>
                    </br>')),
             
             # 7. Data and methods ----
             tabPanel("Data and methods",
                      h4(strong("Data sources")),
                      "This tool uses open data published alongside the 'Permanent and fixed-period exclusions in
                    England: 2015 to 2016' National Statistics release, available at ....",
                      "The following datasets are available to download via the release's underling data files:",
                      br(),
                      br(),
                      h5(strong("SFR35_2017_national_region_la_school_data")),
                      "Number and percentage of permanent and fixed period exclusions and those pupils receiving
                    one or more fixed period exclusion. National, Regional, Local authority and School level -
                    2006/07 to 2015/16 inclusive.",
                      br(),
                      downloadButton("downloadmain_ud", "Download"),
                      br(),
                      br(),
                      h5(strong("SFR35_2017_reason_for_exclusion")),
                      "Number of permanent and fixed period exclusions by reason for exclusion. National, Regional
                    and Local authority level - 2006/07 to 2015/16 inclusive.",
                      br(),
                      downloadButton("downloadreason_ud", "Download"),
                      br(),
                      br(),
                      h5(strong("SFR35_2017_National_characteristics")),
                      "Number and percentage of permanent and fixed period exclusions and those pupils receiving one
                    or more fixed period exclusion by pupil characteristics. National level - 2011/12 to 2015/16
                    inclusive.",
                      br(),
                      downloadButton("downloadnatchar_ud", "Download"),
                      br(),
                      br(),
                      h4(strong("Definitions")),
                      hr(),
                      fluidRow(column(
                        h5("Permanent exclusion"), width =3),
                        column(
                          "A permanent exclusion refers to a pupil who is excluded and who will not come back
                    to that school (unless the exclusion is overturned).", width = 9)),
                      hr(),
                      fluidRow(column(
                        h5("Fixed period exclusion"), width =3),
                        column(
                          "A fixed period exclusion refers to a pupil who is excluded from a school for a set
                    period of time. A fixed period exclusion can involve a part of the school day and it
                    does not have to be for a continuous period. A pupil may be excluded for one or
                    more fixed periods up to a maximum of 45 school days in a single academic year.
                    This total includes exclusions from previous schools covered by the exclusion
                    legislation.", width = 9)),
                      hr(),
                      fluidRow(column(
                        h5("Pupils with one or more fixed period exclusion"), width =3),
                        column(
                          "Pupils with one or more fixed period exclusion refers to pupil enrolments who
                    have at least one fixed period exclusion across the full academic year. It includes
                    those with repeated fixed period exclusions.", width = 9)),
                      hr(),
               HTML('<div><img src="Department_for_Education.png" alt="Logo", width="120", height = "71"></div>
                    <br>
                    <div><b>This is a new service - if you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</b></div>
                    <br>
                    </br>'))
             
  )
  
)


