
#### 
# 1. Load packages ----

library(leaflet)
library(geojsonio)
library(rgdal)
library(sp)
library(data.table)
library(RColorBrewer)
library(raster)

library(pander)
library(tidyverse)
library(shinycssloaders)
library(plotly)

library(shiny)
library(DT)
library(ggalt)
library(magrittr)

####
# 2. General functions, formatting years and rounding ----

# Change the year variable into xxxx/xx format

formatyr <- function(refyear) {
  
  sub("(.{4})(.*)", "\\1/\\2", refyear)
  
}

# example
# formatyr(201213)
# = 2012/13



####
# 3. Load the data required ----

# load main_ud file
# includes main measures at school, la, region and national level for 2006/07 to 2015/16

main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv', col_types = cols(.default = "c"))

#glimpse(main_ud)

# load reason_ud file
# includes la, region and national level for 2006/07 to 2015/16
 
reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv", col_types = cols(.default = "c"))

# head(reason_ud)

# characteristics UD

char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))

####
# Front page ----

## national plots

# create a national summary table (used for plots):

nat_summary <-
  dplyr::select(
    filter(main_ud, level == 'National'),
    year,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate
  ) %>%
  arrange(year)


# National bar charts (front page)

national_bars_rate <- function(category) {
  if (category == 'P') {
    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(perm_excl_rate))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue4', stat = "identity") +
      ylab("Permanent exclusion rate")
  }
  
  if (category == 'F') {
    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(fixed_excl_rate))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue3', stat = "identity") +
      ylab("Fixed period exclusion rate")
  }
  
  plot <- plot +
    scale_y_continuous(breaks = seq(0, max(data$value + 0.5), 0.50)) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 14)) +
    geom_text(
      data = data,
      aes(label = sprintf("%.2f", value)),
      colour = "white",
      vjust = 2
    )
  
  return(plot)
}


national_bars_num <- function(category) {
  if (category == 'P') {
    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(perm_excl))
    
    plot <- data %>% 
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue4', stat = "identity") +
      ylab("Permanent exclusions")
  }
  
  if (category == 'F') {
    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(fixed_excl))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue3', stat = "identity") +
      ylab("Fixed period exclusions") 
  }
  
  plot <- plot + 
    theme_classic() +
    theme(axis.title.x = element_blank(),
          text=element_text(size=14)) +
    geom_text(
      data = data,
      aes(label = prettyNum(value, big.mark = ",")),
      colour="white",
      vjust = 2) +
    scale_y_continuous(labels = scales::comma)
  
  return(plot)
}

####
# LA trends ----

la_plot_data <-
  dplyr::select(
    filter(main_ud, level == 'Local authority', school_type != 'dummy'),
    year,
    la_name,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate) %>%
  mutate(school_type = ifelse(
    school_type == "state-funded primary","Primary",
    ifelse(school_type == "state-funded secondary","Secondary",
           ifelse(school_type == "special", "Special", 
                  ifelse(school_type == "total", "Total", "NA")))))


la_plot_rate <- function(la, category) {

  d <- filter(la_plot_data, la_name == la) 
              
  if (category == 'P') {
    ylabtitle <- "Permanent exclusion percentage"
    d <- d %>% mutate(y_var = perm_excl_rate) %>% filter(y_var != 'x') 
  }
  
  if (category == 'F') {
    ylabtitle <- "Fixed period exclusion percentage"
    d <- d %>% mutate(y_var = fixed_excl_rate) %>% filter(y_var != 'x') 
  }
  
  if (category == 'O') {
    ylabtitle <- "One or more fixed period exclusion percentage"
    d <- d %>% mutate(y_var = one_or_more_fixed_excl_rate) %>% filter(y_var != 'x') 
  }
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = school_type, colour = as.factor(school_type)) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab(ylabtitle) +
        scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(as.numeric(year))+101),
          aes(label = school_type),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  

la_plot_num <- function(la, category) {
  
  d <- filter(la_plot_data, la_name == la) 
  
  if (category == 'P') {
    ylabtitle <- "Permanent exclusions"
    d <- d %>% mutate(y_var = perm_excl) %>% filter(y_var != 'x') 
  }
  
  if (category == 'F') {
    ylabtitle <- "Fixed period exclusions"
    d <- d %>% mutate(y_var = fixed_excl) %>% filter(y_var != 'x') 
  }
  
  if (category == 'O') {
    ylabtitle <- "Enrolments with one or more fixed period exclusion"
    d <- d %>% mutate(y_var = one_plus_fixed) %>% filter(y_var != 'x') 
  }
  
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(y_var), 
          group = school_type, colour = as.factor(school_type)) +
      geom_path(size = 1) +
      xlab("Academic year") +
      ylab(ylabtitle) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = school_type),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
}



### LA time series tables

la_table_num <- function(la, category) {
  
  d <- filter(la_plot_data, la_name == la)
  
  if(category=='P') { 
    d <- d %>% mutate(t_var = perm_excl)
  }
  if(category=='F') {
    d <- d %>% mutate(t_var = fixed_excl)
  }  
  if(category=='O') {
    d <- d %>% mutate(t_var = one_plus_fixed)
  }  

  table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = t_var,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table)
    
  }
  

la_table_rate <- function(la, category) {
  
  d <- filter(la_plot_data, la_name == la)
  
  if(category=='P') { 
    d <- d %>% mutate(t_var = perm_excl_rate)
  }
  if(category=='F') {
    d <- d %>% mutate(t_var = fixed_excl_rate)
  }  
  if(category=='O') {
    d <- d %>% mutate(t_var = one_or_more_fixed_excl_rate)
  }  
  
  table <- d %>%
    mutate(
      yearf = formatyr(year),
      value = t_var,
      Type = school_type
    ) %>%
    dplyr::select(yearf, Type, value) %>%
    spread(key = yearf, value)
  
  row.names(table) <- NULL
  
  return(table)
  
}


# Numbers for LA summary text

la_perm_num <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(perm_excl))
  
}

la_fixed_num <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(fixed_excl))
  
}

la_one_plus_num <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(one_plus_fixed))
  
}


la_perm_rate <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(perm_excl_rate))
  
}

la_fixed_rate <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(fixed_excl_rate))
  
}

la_one_plus_rate <- function(la, refyear) {
  
  d <- filter(main_ud, year == refyear,la_name == la)
  
  return(filter(d, level == 'Local authority', school_type == 'total') %>%
           dplyr::select(one_or_more_fixed_excl_rate))
  
}


####
# 5. MAP exclusion rates ----

ukLocalAuthoritises <- shapefile("data/England_LA_2016.shp")

exc_data <- filter(main_ud, level == 'Local authority', school_type == 'total', year ==201516) %>%
  select(old_la_code,perm_excl_rate, fixed_excl_rate)

exc_data$perm_excl_rate <- as.numeric(exc_data$perm_excl_rate)
exc_data$fixed_excl_rate <- as.numeric(exc_data$fixed_excl_rate)

ukLocalAuthoritises <- spTransform(ukLocalAuthoritises, CRS("+proj=longlat +ellps=GRS80"))
englishLocalAuthorities = subset(ukLocalAuthoritises, LA15CD %like% "E") # Code begins with E

englishLocalAuthorityData <- merge(englishLocalAuthorities, 
                                   exc_data, 
                                   by.x = 'LA_Code', 
                                   by.y = 'old_la_code',
                                   all.y = TRUE)

#permanent exc

# Create bins for colour plotting
perm_excl_rate_Pal = colorQuantile('YlOrRd', englishLocalAuthorityData$perm_excl_rate, n = 5)

# Add a label for tooltip (bit of html)
perm_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Permanent exclusion rate <strong>%g</strong> <sup></sup>",
                                        englishLocalAuthorityData$LA15NM, englishLocalAuthorityData$perm_excl_rate) %>%
  lapply(htmltools::HTML)

#fixed exc

# Create bins for colour plotting
fixed_excl_rate_Pal = colorQuantile('YlOrRd', englishLocalAuthorityData$fixed_excl_rate, n = 5)

# Add a label for tooltip (more html...)
fixed_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Fixed period exclusion rate <strong>%g</strong> <sup></sup>",
                                      englishLocalAuthorityData$LA15NM, englishLocalAuthorityData$fixed_excl_rate) %>%
  lapply(htmltools::HTML)


excmap <- function(measure) {
  
  if(measure == 'perm') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~perm_excl_rate_Pal(englishLocalAuthorityData$perm_excl_rate),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = perm_excl_rate_Labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data"))
    )
  }
  
  if(measure == 'fixed') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~fixed_excl_rate_Pal(englishLocalAuthorityData$fixed_excl_rate),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = fixed_excl_rate_Labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data"))
    )
  }
}


####
# Reason page  ----

perm_reason_table <- function(schtype) {
  
  data <- filter(reason_ud,
                 level == 'National',
                 school_type == schtype,
                 year >= 201112) %>%
    select(year, perm_physical_pupils:perm_other)
  
  data_long <- data %>% gather(key = reason,
                               value = exc,
                               perm_physical_pupils:perm_other)
  
  return(data_long %>% spread(key = year, value =  exc))
  
}

fixed_reason_table <- function(schtype){
  
  data <- filter(reason_ud,
                 level == 'National',
                 school_type == schtype,
                 year >= 201112) %>%
    select(year, fixed_physical_pupils:fixed_other) 
  
  data_long <- data %>% gather(key = reason, value = exc, fixed_physical_pupils:fixed_other)
  
  return(data_long %>% spread(key = year, value =  exc))
  
}


perm_reason_bar <- function(schtype){
  
  data <- filter(reason_ud,
                 level == 'National',
                 school_type == schtype,
                 year == 201516) %>%
    select(year, perm_physical_pupils:perm_other) 
  
  data_long <- data %>% gather(key = reason, value = exc, perm_physical_pupils:perm_other)
  
  return(ggplot(data=data_long, aes(x=reason,y=as.numeric(exc))) +
           geom_bar(fill = 'steelblue4',stat="identity")+
           theme_classic() +
           coord_flip() +
           ylab("Number of permanent exclusions"))
  
}

fixed_reason_bar <- function(schtype){
  
  data <- filter(reason_ud,
                 level == 'National',
                 school_type == schtype,
                 year == 201516) %>%
    select(year, fixed_physical_pupils:fixed_other) 
  
  data_long <- data %>% gather(key = reason, value = exc, fixed_physical_pupils:fixed_other)
  
  return(ggplot(data=data_long, aes(x=reason,y=as.numeric(exc))) +
           geom_bar(fill = 'steelblue4',stat="identity")+
           theme_classic() +
           coord_flip() +
           ylab("Number of fixed period exclusions"))
  
}


####
# Characteristics ----

# cleaning data for characteristics tab  
nat_char_prep <- filter(char_ud, ! characteristic_1 %in% c("SEN_provision_Unclassified","FSM_Unclassified")) %>%
  mutate(characteristic_1 = ifelse(
    characteristic_1 == "Gender_male","Boys",
    ifelse(characteristic_1 == "Gender_female", "Girls",
    ifelse(characteristic_1 == "Total", "Total",
    ifelse(characteristic_1 == "SEN_Provision_No_SEN","No SEN",
    ifelse(characteristic_1 == "SEN_provision_SEN_with_statement_EHC", "SEN with statement or EHC",
    ifelse(characteristic_1 == "SEN_provision_SEN_without_statement", "SEN without a statement or EHC",
    ifelse(characteristic_1 == "FSM_Eligible","FSM eligible",
    ifelse(characteristic_1 == "FSM_NotEligible", "FSM not eligible","NA"))))))))) %>%
  mutate(school_type = ifelse(
    school_type == "State-funded primary","Primary",
    ifelse(school_type == "State-funded secondary","Secondary",
           ifelse(school_type == "Special", "Special", 
                  ifelse(school_type == "Total", "Total", "NA"))))) 
  

char_series <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  }

  if (category == 'P') {
    ylabtitle <- "Permanent exclusion percentage"
    d <- d %>% mutate(y_var = perm_excl_rate) %>% filter(y_var != 'x')
  } else if (category == 'F') {
    ylabtitle <- "Fixed period exclusion percentage"
    d <- d %>% mutate(y_var = fixed_excl_rate) %>% filter(y_var != 'x')
  } else if (category == 'O') {
    ylabtitle <- "One or more fixed period exclusion percentage"
    d <- d %>% mutate(y_var = one_plus_fixed_rate) %>% filter(y_var != 'x')
  }
  
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(y_var), 
          group = characteristic_1, colour = as.factor(characteristic_1)) +
      geom_path(size = 1) +
      xlab("Academic year") +
      ylab(ylabtitle) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = characteristic_1),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
}


char_series_table <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  }
  
  if (category == 'P') {
    data <- d %>% mutate(t_var = perm_excl_rate)
  } else if (category == 'F') {
    data <- d %>% mutate(t_var = fixed_excl_rate)
  } else if (category == 'O') {
    data <- d %>% mutate(t_var = one_plus_fixed_rate)
  }
  
  data$year <- formatyr(data$year)
  
  char_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- char_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
}


### proportion chart 

char_prop <- function(char, sch_type, category){
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type, year == 201516)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type, year == 201516) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type, year == 201516) 
  }
  
  if (category == 'P') {
    data <- d %>% select(characteristic_1,perm_excl) %>% mutate(var = perm_excl)
  } else if (category == 'F') {
    data <- d %>% select(characteristic_1,fixed_excl) %>% mutate(var = fixed_excl)
  } else if (category == 'O') {
    data <- d %>% select(characteristic_1,one_plus_fixed) %>% mutate(var = one_plus_fixed)
  }
  
  return(data %>%
             plot_ly(labels = ~characteristic_1, values = ~var, textinfo="percent+value") %>%
             add_pie(hole = 0.5) %>%
             layout(showlegend = T,legend = list(x = 0.5, y = -0.4),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                    , margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))

  
}


## dumbell plot

char_gaps <- function(char, sch_type, category){
  
char_ud <- char_ud %>%
  mutate(school_type = ifelse(
    school_type == "State-funded primary","Primary",
    ifelse(school_type == "State-funded secondary","Secondary",
           ifelse(school_type == "Special", "Special", 
                  ifelse(school_type == "Total", "Total", "NA"))))) 
  
  if(category=='P') {
    measure <- 'perm_excl_rate'
  } else if (category=='F') {
    measure <- 'fixed_excl_rate'
  } else if (category=='O') {
    measure <- 'one_plus_fixed_rate'
  }
  
  
  if (char == 'gender') {
    d <- char_ud %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type,characteristic_1 %in% c('Gender_male', 'Gender_female'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(Gender_male) - as.numeric(Gender_female))
    data$char_no <- as.numeric(data$Gender_female)
    data$char_yes <-as.numeric(data$Gender_male)
    yes_label <- "Boys"
    no_label <- "Girls"
  } else if (char == 'sen') {
    d <- char_ud %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type,characteristic_1 %in% c('SEN_provision_SEN_with_statement_EHC', 'SEN_Provision_No_SEN'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC) - as.numeric(SEN_Provision_No_SEN))
    data$char_no <- as.numeric(data$SEN_Provision_No_SEN)
    data$char_yes <-as.numeric(data$SEN_provision_SEN_with_statement_EHC)
    yes_label <- "Statement/EHC"
    no_label <- "No SEN"
  } else if (char == 'fsm') {
    d <- char_ud %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type,characteristic_1 %in% c('FSM_Eligible', 'FSM_NotEligible'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(FSM_Eligible) - as.numeric(FSM_NotEligible))
    data$char_no <- as.numeric(data$FSM_NotEligible)
    data$char_yes <-as.numeric(data$FSM_Eligible)
    yes_label <- "FSM"
    no_label <- "Not FSM"
  }
  

  max_year <- formatyr(max(data$year))
  data$year <- factor(formatyr(data$year))

  return(ggplot() + 
    geom_segment(data=data, aes(y=year, yend=year, x=0, xend=max(data$char_yes)*1.1), color="#b2b2b2", size=0.15) +
    geom_dumbbell(data=data, aes(y=year, x=char_yes, xend=char_no),
                  size=2, color="gray", size_x=3, size_xend=3,
                  colour_x="steelblue4", colour_xend="steelblue2") + 
    geom_text(data=filter(data, year==max_year),
              aes(x=char_yes, y=year, label=yes_label),
              color="steelblue4", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=filter(data, year==max_year),
              aes(x=char_no, y=year, label=no_label),
              color="steelblue2", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=data, aes(x=char_yes, y=year, label=char_yes),
              color="steelblue4", size=5, vjust=2.5) + 
    geom_text(data=data, color="steelblue2", size=5, vjust=2.5,
              aes(x=char_no, y=year, label=char_no)) + 
    geom_text(data=data, aes(label=diff, y=year, x=max(c(data$char_no,data$char_yes))*1.1), color="#7a7d7e", size=5, vjust =2.5) +
    geom_text(data=filter(data, year==max_year), aes(x=max(c(data$char_no,data$char_yes))*1.1, y=year, label="diff"),
              color="#7a7d7e", size=5, vjust=-2) + 
    scale_x_continuous(expand=c(0,0), limits=c(min(c(data$char_no,data$char_yes))*0.6, max(c(data$char_no,data$char_yes))*1.2)) + 
    labs(x=NULL, y=NULL) + 
    theme_bw() + 
    theme(panel.border=element_blank(),
          axis.ticks=element_blank(),
          axis.text.x=element_blank(),
          plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2,face="bold"),
          axis.text=element_text(size=12))
    )
  
}





# school tab 

#school level info for a specific LA

la_sch_table <- function(la,refyear) {
  
  d <- filter(main_ud, level == "School",la_name == la) %>%
    select(
      year,
      la_name,
      laestab,
      school_type,
      headcount,
      perm_excl,
      perm_excl_rate,
      fixed_excl,
      fixed_excl_rate,
      one_plus_fixed,
      one_or_more_fixed_excl_rate
    )
  
  return(d)
}


#school level info for a specific LA

la_sch_table <- function(la,refyear) {
  
  d <- filter(main_ud, level == "School",la_name == la) %>%
    select(
      year,
      la_name,
      laestab,
      school_type,
      headcount,
      perm_excl,
      perm_excl_rate,
      fixed_excl,
      fixed_excl_rate,
      one_plus_fixed,
      one_or_more_fixed_excl_rate
    )
  
  return(d)
}


# School summary ----

school_summary_table <- filter(main_ud, level == "School", la_name != ".") %>% arrange(as.numeric(laestab)) %>%
  select(
    year,
    la_name,
    laestab,
    school_type,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate
  )

