sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("R/")

shinyUI(
    navbarPage("Exclusion statistics", id = "nav", 
                   
                   # 1. Front page ----
                   
                   tabPanel("Overview",
                            sidebarLayout(
                              sidebarPanel(verticalLayout(
                                h3(strong("Understanding school exclusions statistics")),
                                hr(),
                                strong("Background"),
                                "The purpose of this dashboard is to provide insight to lower level breakdowns included within our 
                                National Statistics release. It reports on permanent and fixed period exclusions from state-funded primary, state-funded secondary 
                                and special schools as reported in the School Census. This is a pilot release from the department, published as an example of what future statistics dissemination may look like; helping users to understand exclusions data.",
                                hr(),
                                strong("Latest National Statistics"),
                                br(" All of the data used within this dashboard, including additional breakdowns, has been published in the releases underlying data section and is also available for download in data and methods tab. This can be found in our",
                                  a("Permanent and fixed-period exclusions in England: 2015 to 2016 report.", 
                                    href = "https://www.gov.uk/government/statistics/permanent-and-fixed-period-exclusions-in-england-2015-to-2016",
                                    target="_blank")),
                                hr(),
                                strong("Guidance and methodology"),
                                "This dashboard shows various breakdowns for the number and rate of permanent and fixed period exclusions as well as enrolments receiving one or more fixed period exclusion. Rates are calculated using the number of sole and dual registered pupils on roll as of Spring Census day. Further info, including definitions, is available in the data and methods tab.",
                                br(a("An exclusions statistics guide",
                                     href = "https://www.gov.uk/government/publications/exclusions-statistics-guide",
                                     target = "_blank"),"which provides historical information on exclusion statistics, technical 
                                background information to the figures and data collection, and definitions of key terms should be referenced 
                                alongside this release."), 
                                hr(),
                                strong("Definitons"),
                                "Defintions relating to statistics used in this application can be found in the data and methods tab."
                                
                              ), width = 5),
                              mainPanel(
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
                      HTML('<div>
                             <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                             <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>
                           <br>
                           </br>
                           ')
             ),
             
             
             # 2. Pupil Characteristics ----
             
             tabPanel("Pupil characteristics",
                      
                      sidebarLayout(sidebarPanel(
                        h4(strong("Exclusions by pupil characteristic")),
                        selectInput("char_char",
                                    label = "select characteristic",
                                    choices = list(
                                      "SEN provision" = 'sen',
                                      "FSM eligibility" = 'fsm',
                                      "Gender" = 'gender'),
                                    selected = 'gender'),
                        selectInput("char_sch",
                                    label = "select school type",
                                    choices = list(
                                      "State-funded primary" = 'Primary',
                                      "State-funded secondary" = 'Secondary',
                                      "Special" = 'Special',
                                      "Total" = 'Total'),
                                    selected = 'Total'),
                        selectInput("char_cat",
                                    label = "select measure",
                                    choices = list(
                                      "Fixed period" = 'F',
                                      "Permanent" = 'P',
                                      "One or more fixed" = 'O'),
                                    selected = 'P'),
                        tableOutput("char_ts_table"), width = 5
                      ),
                      mainPanel(
                        strong("chart title"),
                        br(),
                        "footnotes",
                        br(),
                        plotOutput("char_ts"), width = 7
                      )
                      ),
                      fluidRow(
                        column(
                          verticalLayout(
                            h4(strong("Proportion of exclusions by characteristic - 2015/16")),
                            em("State-funded primary, secondary and special schools"),
                            plotlyOutput("char_prop")
                          ),
                          width = 5
                        ),
                        column(
                          verticalLayout(
                            h4(strong("How is difference in exclusion rate changing over time?")),
                            em("State-funded primary, secondary and special schools"),
                            plotOutput("char_gaps")
                          ),
                          width = 5
                        )
                      )
             ,                            
             hr(),
             HTML('<div>
                             <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                             <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>
                           <br>
                           </br>')),
             
             # 3. LA Trends ----
             
             tabPanel("LA trends",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Local Authority (LA) level exclusions")),
                          br(),
                          h5(strong("Pick a local authority")),
                          selectInput(
                            "select2",
                            label = NULL,
                            choices = sort(unique(la_plot_data$la_name)),
                            selected = 'Darlington'
                          ),
                          h5(strong("Pick a measure")),
                          selectInput(
                            "select_cat",
                            label = NULL,
                            choices = list(
                              "Fixed" = 'F',
                              "Permanent" = 'P',
                              "One plus" = 'O'
                            ),
                            selected = 'F'
                          ),
                          h5(strong(textOutput("la_title"))),
                          textOutput("la_perm"),
                          br(),
                          textOutput("la_fixed"),
                          br(),
                          textOutput("la_one_plus"),
                          hr(),
                          h5(strong("National reference")),
                          "The number of permanent exclusions increased from 5,795 (0.07 per cent) in 2014/15 to
                          6,685 (0.08 per cent) in 2015/16, which is equivalent to 8 pupils per 10,000.",
                          br(),
                          br(),
                          "The number of fixed period exclusions increased from 302,975 (3.88 per cent) in 2014/15
                          to 339,360 (4.29 per cent) in 2015/16, which is equivalent to 429 pupils per 10,000.",
                          br(),
                          br(),
                          "There were 167,125 pupil enrolments (2.11 per cent) with at least one fixed term exclusion
                          in 2015/16, up from 154,060 pupil enrolments (1.98 per cent) in 2014/15, which is equivalent
                          to 211 pupils per 10,000."
                        ),
                        mainPanel(
                          br(),
                          fluidRow(column(9,
                                          br(),
                                          column(3,
                                                 radioButtons("plot_type", "Which measure?", c("rate", "number"), inline = TRUE)
                                          ))),
                          plotOutput("t1_chart", width = '23cm'),
                          br(),
                          tableOutput("t1_table"),
                          br()
                        )),
                      hr(),
                      HTML('<div>
                           <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                           <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>')),
             
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
                      HTML('<div>
                           <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                           <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>
                           <br>
                           </br>')
                    ),

            
             #  # 5. Reason for exclusions ----
             #  
             tabPanel("Reason for exclusion",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Exclusions by reason")),
                          "Schools report exclusions broken down by reason",
                          h5(strong("Pick a school type")),
                          selectInput("reasonschtype",
                                      label = NULL,
                                      choices = list(
                                        "Primary" = 'State-funded primary',
                                        "Secondary" = 'State-funded secondary',
                                        "Special" = 'Special school',
                                        "All schools" = 'Total'),
                                      selected = 'Total', width='30%'),
                          "INSERT A DOWNLOAD BUTTON",
                          width=12),
                        mainPanel()),
                      splitLayout(
                        verticalLayout(h3("Permanent"),
                                       strong("Permanent exclusions broken down by reason"),
                                       em("2015/16 academic year"),
                                       br(),
                                       plotOutput("perm_reason"),
                                       br(),
                                       br(),
                                       strong("Permanent exclusions broken down by reason"),
                                       em("2011/12 to 2015/16 academic year"),
                                       br(),
                                       tableOutput("perm_reason_t")),
                        verticalLayout(h3("Fixed period"), 
                                       strong("Fixed period exclusions broken down by reason"),
                                       em("2015/16 academic year"),
                                       br(),
                                       plotOutput("fixed_reason"),
                                       br(),
                                       br(),
                                       strong("Fixed period exclusions broken down by reason"),
                                       em("2011/12 to 2015/16 academic year"),
                                       br(),
                                       tableOutput("fixed_reason_t"))),
                      hr(),
                      HTML('<div>
                           <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                           <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>
                           <br>
                           </br>')),
             
             # 6. Schools Summary 
             tabPanel("School level exclusions",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Select a Local Authority and then an individual school")),
                          "The below table shows information about exclusions data for individual schools. Select the Local Authority name or number in the first tab above and then the school LAESTAB number or school name in the second tab. You are able to download this data as a .csv file, excel file or simple copy the data to your clipboard with the buttons below.",
                          br(),
                          br(),
                          selectInput("la_name_rob", label = "1. Select Local Authority code or name" ,choices = sort(unique(all_schools_data$la_no_and_name)),  width='30%'),
                          selectizeInput("EstablishmentName_rob", label = "2. Select School Name or LAESTAB number", choices = NULL, options = list(placeholder = "Select school", maxOptions = 50000),  width='30%'),
                          h5(strong("Note on suppresion")),
                          "Values of 'x' in the table below represent numbers that are below 3 exclusions and are supressed for data protection purposes, with the corresponding rate also supressed for the same reason.",
                          width=12),
                        mainPanel(
                          dataTableOutput("table_school_summary"),
                          width=12
                        )),
                      hr(),
                      HTML('<div>
                           <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                           <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                           </div>
                           <br>
                           </br>
                           <br>
                           </br>
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
                      br(),
                      br(),
                      br(),
                      hr()
                      
             , HTML('<div>
                  <span style="float:left">If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk</span>
                  <img src="Department_for_Education.png" alt="Logo" style="float:right", width="120", height = "71">
                  </div>
                  <br>
                  </br>'))
             
  )
  
)


