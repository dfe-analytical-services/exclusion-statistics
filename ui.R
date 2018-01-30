

source("codefile_shiny.R")

shinyUI(
    navbarPage("Exclusion statistics", id = "nav", 
                   
                   # 1. Front page ----
                   
                   tabPanel("Overview",
                            sidebarLayout(
                              sidebarPanel(verticalLayout(
                                strong("insert cool title for tool here"),
                                br(),
                                h3("Permanent and fixed period exclusions in England"),
                                br(),
                                strong("Background"),
                                "The purpose of this tool/dashboard is to provide insight to lower level breakdowns included within our 
                                National Statistics release.",
                                "It reports on permanent and fixed period exclusions from state-funded primary, state-funded secondary 
                                and special schools as reported in the School Census.",
                                hr(),
                                strong("Latest National Statistics"),
                                "Further information is available in the ", 
                                a("Permanent and fixed-period exclusions in England:2015 to 2016", 
                                    href = "https://www.gov.uk/government/statistics/permanent-and-fixed-period-exclusions-in-england-2015-to-2016",
                                    target="_blank"),
                                "National Statistics release",
                                hr(),
                                strong("Guidance and methodology"),
                                "The data and methods tab includes information on the data used in this tool as well as definitions for 
                                terms used throughout.",
                                "An exclusions statistics guide, which provides historical information on exclusion statistics, technical 
                                background information to the figures and data collection, and definitions of key terms should be referenced 
                                alongside this release.",
                                a("Exclusions statistics guide",
                                    href = "https://www.gov.uk/government/collections/statistics-school-workforce",
                                    target = "_blank")
                              ), width = 5),
                              mainPanel(
                                strong("Permanent exclusions, 2006/07 to 2015/16"), br(),
                                em("State-funded primary, secondary and special schools"),
                                radioButtons("bars_type", label=NULL, c("rate", "number"), inline = TRUE),
                                plotOutput("p_bar", height ="8cm"),
                                hr(),
                                strong("Fixed period exclusions, 2006/07 to 2015/16"), br(),
                                em("State-funded primary, secondary and special schools"),
                                radioButtons("bars_type2", label=NULL, c("rate", "number"), inline = TRUE),
                                plotOutput("f_bar", height ="8cm"),
                                width = 7)),
                            #dfE logo
                            verticalLayout(
                              img(src = "DfE_logo.png",
                                height = 145,
                                width = 350
                            ),
                        "If you would like to provide feedback on this tool please contact schools.statistics@education.gov.uk"),
                      br()
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
             ),
             
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
                        ))),
             
             # 4. Map ----
             
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Mapping exclusion rates")),
                          em("State-funded primary, secondary and special schools, 2015/16"),
                          h5(strong("Pick a measure")),
                          selectInput(
                            "select_map",
                            label = NULL,
                            choices = list("Permanent" = 'perm',
                                           "Fixed period" = 'fixed'),
                            selected = 'fixed'
                          ),
                          width = 3
                        ),
                        mainPanel(
                          leafletOutput("map", width = '25cm', height = '25cm') %>%
                            #spinner to appear while chart is loading
                            withSpinner(
                              color = "grey",
                              type = 5,
                              size = getOption("spinner.size", default = 0.4)
                            )
                        )
                      )
                    )
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
                                       tableOutput("fixed_reason_t")))),
             
             # 6. Schools Summary 
             tabPanel("School summary",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Select local authority and then school number")),
                          selectInput("la_name_rob", label = "1. Local Authority" ,choices = unique(school_summary_table$la_name)),
                          selectizeInput("laestab_rob", label = "2. School Number", choices = NULL),
                          br(),
                          br(),
                          h4(strong("Exclusions by reason")),
                          br(),
                          width=12),
                        mainPanel(
                          dataTableOutput("table_school_summary"),
                          width=12
                        ))),
             
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
                      hr()
             )
             
  )
  
)


