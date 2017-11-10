

#### 
# 1. Load packages ----

library(leaflet)
library(geojsonio)
library(rgdal)
library(sp)
library(data.table)
library(RColorBrewer)
library(raster)

#library(RODBC)
library(pander)
#library(ggplot2)
library(tidyverse)
#library(dplyr)
library(shinycssloaders)
library(plotly)

library(DT)
#library(shinyjs)
library(ggalt)

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
# 4. Load the data required ----


# load main_ud file
# includes main measures at school, la, region and national level for 2006/07 to 2015/16

main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv')

#glimpse(main_ud)


# load reason_ud file
# includes la, region and national level for 2006/07 to 2015/16
# 
reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv")

# head(reason_ud)

# characteristics UD

char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv')

####
# 5. LA trends plot 

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
  
  if(category=='P') {
    
  d <- filter(la_plot_data, la_name == la, perm_excl_rate !='x')
  
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(perm_excl_rate), 
          group = school_type, colour = school_type) +
      geom_path(size = 1) +
      xlab("Academic year") +
      ylab("Permanent exclusion percentage") +
      scale_y_continuous(limits = c(0, max(as.numeric(d$perm_excl_rate))+0.01)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(year)+101),
        aes(label = school_type),
        position = position_nudge(y=0.1),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
  }
  
  if(category=='F') {
    
    d <- filter(la_plot_data, la_name == la,fixed_excl_rate !='x')
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(fixed_excl_rate), 
            group = school_type, colour = school_type) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab("Fixed period exclusion percentage") +
        scale_y_continuous(limits = c(0, max(as.numeric(d$fixed_excl_rate))+1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(year)+101),
          aes(label = school_type),
          position = position_nudge(y=0.1),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  
  if(category=='O') {
    
    d <- filter(la_plot_data, la_name == la,one_or_more_fixed_excl_rate !='x')
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(one_or_more_fixed_excl_rate), 
            group = school_type, colour = school_type) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab("One or more fixed period exclusion percentage") +
        scale_y_continuous(limits = c(0, max(as.numeric(d$one_or_more_fixed_excl_rate))+1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(year)+101),
          aes(label = school_type),
          position = position_nudge(y=0.1),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  
}




la_plot_num <- function(la, category) {
  
  if(category=='P') {
    
    d <- filter(la_plot_data, la_name == la, perm_excl !='x')
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(perm_excl), 
            group = school_type, colour = school_type) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab("Permanent exclusions") +
        scale_y_continuous(limits = c(0, max(as.numeric(d$perm_excl))+0.05)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(year)+101),
          aes(label = school_type),
          position = position_nudge(y=0.1),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  
  if(category=='F') {
    
    d <- filter(la_plot_data, la_name == la,fixed_excl !='x')
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(fixed_excl), 
            group = school_type, colour = school_type) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab("Fixed period exclusions") +
        scale_y_continuous(limits = c(0, max(as.numeric(d$fixed_excl))+1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(year)+101),
          aes(label = school_type),
          position = position_nudge(y=0.1),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  
  if(category=='O') {
    
    d <- filter(la_plot_data, la_name == la,one_plus_fixed !='x')
    
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(one_plus_fixed), 
            group = school_type, colour = school_type) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab("One or more fixed period exclusions") +
        scale_y_continuous(limits = c(0, max(as.numeric(d$one_plus_fixed))+1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(year)+101),
          aes(label = school_type),
          position = position_nudge(y=0.1),
          size = 5,
          hjust = 0,
          vjust = -1) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")))
  }
  
}




####
# 5. MAP

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
        addLegend(pal = perm_excl_rate_Pal, 
                  values = englishLocalAuthorityData$perm_excl_rate, 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright")
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
        addLegend(pal = fixed_excl_rate_Pal, 
                  values = englishLocalAuthorityData$fixed_excl_rate, 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright")
    )
  }
}

### LA time series tables

la_table_num <- function(la, category) {
  
  if(category=='P') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = perm_excl,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table)
    
  }
  
  if(category=='F') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = fixed_excl,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table)
    
  }
  
  if(category=='O') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = one_plus_fixed,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table) 

  }
  
}


la_table_rate <- function(la, category) {
  
  if(category=='P') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = perm_excl_rate,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table)
    
  }
  
  if(category=='F') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = fixed_excl_rate,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table)
    
  }
  
  if(category=='O') {
    
    d <- filter(la_plot_data, la_name == la)
    
    table <- d %>%
      mutate(
        yearf = formatyr(year),
        value = one_or_more_fixed_excl_rate,
        Type = school_type
      ) %>%
      dplyr::select(yearf, Type, value) %>%
      spread(key = yearf, value)
    
    row.names(table) <- NULL
    
    return(table) 
    
  }
  
}

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


# National bar chart (front page)

national_bars <- function(category) {
  if (category == 'P') {

    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(perm_excl_rate))
      
      p_bar_g <- data %>% 
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue4', stat = "identity") +
      theme_classic() +
      ylab("Permanent exclusion rate") +
      xlab("Academic year") +
      scale_y_continuous(breaks = seq(0, max(data$value + 0.01), 0.02)) +
      theme(axis.title.x = element_blank())
    
    return(ggplotly(p_bar_g))
  }
  
  if (category == 'F') {
    
    data <- filter(nat_summary, school_type == 'total') %>%
      mutate(year = as.factor(year),
             value = as.numeric(fixed_excl_rate))
      
    f_bar_g <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = 'dodgerblue3', stat = "identity") +
      theme_classic() +
      ylab("Fixed period exclusion rate") +
      xlab("Academic year") +
      scale_y_continuous(breaks = seq(0, max(data$value + 0.5), 0.50)) +
      theme(axis.title.x = element_blank())
    
    return(ggplotly(f_bar_g))
  }
}



perm_reason_table <- function(schtype){

data <-
  filter(reason_ud,
         level == 'National',
         school_type == schtype,
         year >= 201112) %>%
  select(year, perm_physical_pupils:perm_other) 

data_long <- data %>% gather(key = reason, value = exc, perm_physical_pupils:perm_other)

return(data_long %>% spread(key = year, value =  exc))

}

fixed_reason_table <- function(schtype){
  
  data <-
    filter(reason_ud,
           level == 'National',
           school_type == schtype,
           year >= 201112) %>%
    select(year, fixed_physical_pupils:fixed_other) 
  
  data_long <- data %>% gather(key = reason, value = exc, fixed_physical_pupils:fixed_other)
  
  return(data_long %>% spread(key = year, value =  exc))
  
}


perm_reason_bar <- function(schtype){

    data <-
      filter(reason_ud,
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
  
  data <-
    filter(reason_ud,
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
  
#la_sch_table("Darlington")


# x <- c("State-funded primary", "State-funded secondary", "Special", "Total")
# 
# DT %>%
#   mutate(category =  factor(category, levels = x)) %>%
#   arrange(category)



#data for download button

fsmchar <- char_ud %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'))

#proportion chart

fsm_prop <- function(category){

data <- char_ud %>% filter(
    school_type == "Total",
    year == 201516,
    characteristic_desc %in% c('FSM_Eligible'),
    characteristic_1 != "FSM_Unclassified") %>%
  mutate(characteristic_1 = ifelse(
    characteristic_1 == "FSM_Eligible","FSM eligible",
    ifelse(characteristic_1 == "FSM_NotEligible", "FSM not eligible",
           ifelse(characteristic_1 == "Total", "Total","NA"))))


if(category=='P') {
  
fsm_prop <- data %>% select(characteristic_1,perm_excl) 

return(fsm_prop %>%
  plot_ly(labels = ~characteristic_1, values = ~perm_excl) %>%
  add_pie(hole = 0.5) %>%
  layout(showlegend = T,legend = list(x = 0.3, y = -0.3),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         , margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
}

if(category=='F') {
  
  fsm_prop <- data %>% select(characteristic_1,fixed_excl) 
  
  return(fsm_prop %>%
           plot_ly(labels = ~characteristic_1, values = ~fixed_excl) %>%
           add_pie(hole = 0.5) %>%
           layout(showlegend = T, legend = list(x = 0.3, y = -0.3),
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
}

if(category=='O') {
  
  fsm_prop <- data %>% select(characteristic_1,one_plus_fixed) 
  
  return(fsm_prop %>%
           plot_ly(labels = ~characteristic_1, values = ~one_plus_fixed) %>%
           add_pie(hole = 0.5) %>%
           layout(showlegend = T, legend = list(x = 0.3, y = -0.3),
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
}

}


fsm_sch_table_num <- function(sch_type,category){
  
  data <- char_ud %>% filter(
      school_type == sch_type,
      characteristic_desc %in% c('FSM_Eligible', 'Total'),
      characteristic_1 != "FSM_Unclassified") %>%
    mutate(characteristic_1 = ifelse(
      characteristic_1 == "FSM_Eligible","FSM eligible",
      ifelse(characteristic_1 == "FSM_NotEligible", "FSM not eligible",
             ifelse(characteristic_1 == "Total", "Total","NA"))))
  
  data$year <- formatyr(data$year)
  
  if(category=='P') {
  
  fsm_sch <- data %>% select(year,characteristic_1,perm_excl)  
  
  data_wide <- fsm_sch %>% spread(key = year, value =  perm_excl)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
  }
  
  if(category=='F') {
    
    fsm_sch <- data %>% select(year,characteristic_1,fixed_excl)
    
    data_wide <- fsm_sch %>% spread(key = year, value =  fixed_excl)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='O') {
    
    fsm_sch <- data %>% select(year,characteristic_1,one_plus_fixed)
    
    data_wide <- fsm_sch %>% spread(key = year, value =  one_plus_fixed)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
}


fsm_sch_table_rate <- function(sch_type,category){
  
  data <- char_ud %>% filter(
    school_type == sch_type,
    characteristic_desc %in% c('FSM_Eligible', 'Total'),
    characteristic_1 != "FSM_Unclassified") %>%
    mutate(characteristic_1 = ifelse(
      characteristic_1 == "FSM_Eligible","FSM eligible",
      ifelse(characteristic_1 == "FSM_NotEligible", "FSM not eligible",
             ifelse(characteristic_1 == "Total", "Total","NA"))))
  
  data$year <- formatyr(data$year)
  
  if(category=='P') {
    
    fsm_sch <- data %>% select(year,characteristic_1,perm_excl_rate)
    
    data_wide <- fsm_sch %>% spread(key = year, value =  perm_excl_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='F') {
    
    fsm_sch <- data %>% select(year,characteristic_1,fixed_excl_rate)
    
    data_wide <- fsm_sch %>% spread(key = year, value =  fixed_excl_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='O') {
    
    fsm_sch <- data %>% select(year,characteristic_1,one_plus_fixed_rate)
    
    data_wide <- fsm_sch %>% spread(key = year, value =  one_plus_fixed_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
}

#fsm_sch_table_num("State-funded secondary","P")




fsm_gap <- function(category) {
  
  fsm <- char_ud %>% filter(school_type=='Total', characteristic_desc =='FSM_Eligible', characteristic_1 !='Total') %>% 
    select(year,characteristic_1,perm_excl_rate,fixed_excl_rate,one_plus_fixed_rate) %>%
    filter(characteristic_1 != 'FSM_Unclassified')
  
  if(category=='P') {
    
    fsm_perm <- fsm %>% select(year,characteristic_1,perm_excl_rate) %>% spread(key = characteristic_1, value = perm_excl_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
    
    df_p <- fsm_perm
    
    df_p$year <- factor(formatyr(df_p$year))
    df_p$fsm_not <- as.numeric(df_p$FSM_NotEligible)
    df_p$fsm_yes <- as.numeric(df_p$FSM_Eligible)
    
    pp <- ggplot() + 
      geom_segment(data=df_p, aes(y=year, yend=year, x=0, xend=max(df_p$fsm_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_p, aes(y=year, x=fsm_yes, xend=fsm_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_p, year=="2015/16"),
                aes(x=fsm_yes, y=year, label="FSM eligible"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_p, year=="2015/16"),
                aes(x=fsm_not, y=year, label="Non-FSM eligible"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_p, aes(x=fsm_yes, y=year, label=fsm_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_p, color="steelblue2", size=5, vjust=2.5,
                aes(x=fsm_not, y=year, label=fsm_not)) + 
      geom_text(data=df_p, aes(label=diff, y=year, x=max(df_p$fsm_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_p, year=="2015/16"), aes(x=max(df_p$fsm_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_p$fsm_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
      theme(axis.text=element_text(size=12))
    
    return(pp)
    
  }
  
  if(category=='F') {
    
    fsm_fixed <- fsm %>% select(year,characteristic_1,fixed_excl_rate) %>% spread(key = characteristic_1, value = fixed_excl_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
    
    df_f <- fsm_fixed
    
    df_f$year <- factor(formatyr(df_f$year))
    df_f$fsm_not <- as.numeric(df_f$FSM_NotEligible)
    df_f$fsm_yes <- as.numeric(df_f$FSM_Eligible)
    
    
    ff <- ggplot() + 
      geom_segment(data=df_f, aes(y=year, yend=year, x=0, xend=max(df_f$fsm_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_f, aes(y=year, x=fsm_yes, xend=fsm_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_f, year=="2015/16"),
                aes(x=fsm_yes, y=year, label="FSM eligible"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_f, year=="2015/16"),
                aes(x=fsm_not, y=year, label="Non-FSM eligible"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_f, aes(x=fsm_yes, y=year, label=fsm_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_f, color="steelblue2", size=5, vjust=2.5,
                aes(x=fsm_not, y=year, label=fsm_not)) + 
      geom_text(data=df_f, aes(label=diff, y=year, x=max(df_f$fsm_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_f, year=="2015/16"), aes(x=max(df_f$fsm_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_f$fsm_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
      theme(axis.text=element_text(size=12))
    
    return(ff)
    
  }
  
  if(category=='O') {
    
    fsm_one_plus <- fsm %>% select(year,characteristic_1,one_plus_fixed_rate) %>% spread(key = characteristic_1, value = one_plus_fixed_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
    
    df_o <- fsm_one_plus
    
    df_o$year <- factor(formatyr(df_o$year))
    df_o$fsm_not <- as.numeric(df_o$FSM_NotEligible)
    df_o$fsm_yes <- as.numeric(df_o$FSM_Eligible)
    
    
    op <- ggplot() + 
      geom_segment(data=df_o, aes(y=year, yend=year, x=0, xend=max(df_o$fsm_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_o, aes(y=year, x=fsm_yes, xend=fsm_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_o, year=="2015/16"),
                aes(x=fsm_yes, y=year, label="FSM eligible"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_o, year=="2015/16"),
                aes(x=fsm_not, y=year, label="Non-FSM eligible"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_o, aes(x=fsm_yes, y=year, label=fsm_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_o, color="steelblue2", size=5, vjust=2.5,
                aes(x=fsm_not, y=year, label=fsm_not)) + 
      geom_text(data=df_o, aes(label=diff, y=year, x=max(df_o$fsm_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_o, year=="2015/16"), aes(x=max(df_o$fsm_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_o$fsm_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.3))+
      theme(axis.text=element_text(size=12))
    
    return(op) 
    
  }
  
}


#data for download button
senchar <- char_ud %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'))


sen_sch_table_num <- function(sch_type,category){
  
  data <- char_ud %>% filter(
    school_type == sch_type,
    characteristic_desc %in% c('SEN_provision', 'Total'),
    characteristic_1 != "SEN_provision_Unclassified") %>%
    mutate(characteristic_1 = ifelse(
      characteristic_1 == "SEN_Provision_No_SEN","No SEN",
      ifelse(characteristic_1 == "SEN_provision_SEN_with_statement_EHC", "SEN with statement or EHC",
             ifelse(characteristic_1 == "SEN_provision_SEN_without_statement", "SEN without a statement or EHC",
                 ifelse(characteristic_1 == "Total", "Total","NA")))))
  
  data$year <- formatyr(data$year)
  
  if(category=='P') {
    
    sen_sch <- data %>% select(year,characteristic_1,perm_excl)  
    
    data_wide <- sen_sch %>% spread(key = year, value =  perm_excl)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='F') {
    
    sen_sch <- data %>% select(year,characteristic_1,fixed_excl)
    
    data_wide <- sen_sch %>% spread(key = year, value =  fixed_excl)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='O') {
    
    sen_sch <- data %>% select(year,characteristic_1,one_plus_fixed)
    
    data_wide <- sen_sch %>% spread(key = year, value =  one_plus_fixed)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
}


sen_sch_table_rate <- function(sch_type,category){
  
  data <- char_ud %>% filter(
    school_type == sch_type,
    characteristic_desc %in% c('SEN_provision', 'Total'),
    characteristic_1 != "SEN_provision_Unclassified") %>%
    mutate(characteristic_1 = ifelse(
      characteristic_1 == "SEN_Provision_No_SEN","No SEN",
      ifelse(characteristic_1 == "SEN_provision_SEN_with_statement_EHC", "SEN with statement or EHC",
             ifelse(characteristic_1 == "SEN_provision_SEN_without_statement", "SEN without a statement or EHC",
                    ifelse(characteristic_1 == "Total", "Total","NA")))))
  
  data$year <- formatyr(data$year)
  
  if(category=='P') {
    
    sen_sch <- data %>% select(year,characteristic_1,perm_excl_rate)
    
    data_wide <- sen_sch %>% spread(key = year, value =  perm_excl_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='F') {
    
    sen_sch <- data %>% select(year,characteristic_1,fixed_excl_rate)
    
    data_wide <- sen_sch %>% spread(key = year, value =  fixed_excl_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
  if(category=='O') {
    
    sen_sch <- data %>% select(year,characteristic_1,one_plus_fixed_rate)
    
    data_wide <- sen_sch %>% spread(key = year, value =  one_plus_fixed_rate)
    
    colnames(data_wide)[1] <- ""
    
    return(data_wide)
    
  }
  
}

#sen_sch_table_rate("State-funded secondary","P")






sen_gap <- function(category) {
  
  sen <- char_ud %>% filter(school_type=='Total', characteristic_desc =='SEN_provision', characteristic_1 !='Total') %>% 
    select(year,characteristic_1,perm_excl_rate,fixed_excl_rate,one_plus_fixed_rate) %>%
    filter(characteristic_1 %in% c('SEN_provision_SEN_with_statement_EHC', 'SEN_Provision_No_SEN'))
  
  if(category=='P') {
    
    sen_perm <- sen %>% select(year,characteristic_1,perm_excl_rate) %>% spread(key = characteristic_1, value = perm_excl_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
    
    df_p <- sen_perm
    
    df_p$year <- factor(formatyr(df_p$year))
    df_p$sen_not <- as.numeric(df_p$SEN_Provision_No_SEN)
    df_p$sen_yes <- as.numeric(df_p$SEN_provision_SEN_with_statement_EHC)
    
    pp <- ggplot() + 
      geom_segment(data=df_p, aes(y=year, yend=year, x=0, xend=max(df_p$sen_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_p, aes(y=year, x=sen_yes, xend=sen_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_p, year=="2015/16"),
                aes(x=sen_yes, y=year, label="Statement/EHC"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_p, year=="2015/16"),
                aes(x=sen_not, y=year, label="No SEN"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_p, aes(x=sen_yes, y=year, label=sen_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_p, color="steelblue2", size=5, vjust=2.5,
                aes(x=sen_not, y=year, label=sen_not)) + 
      geom_text(data=df_p, aes(label=diff, y=year, x=max(df_p$sen_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_p, year=="2015/16"), aes(x=max(df_p$sen_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_p$sen_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
      theme(axis.text=element_text(size=12))
    
    return(pp)
    
  }
  
  if(category=='F') {
    
    sen_fixed <- sen %>% select(year,characteristic_1,fixed_excl_rate) %>% spread(key = characteristic_1, value = fixed_excl_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
    
    df_f <- sen_fixed
    
    df_f$year <- factor(formatyr(df_f$year))
    df_f$sen_not <- as.numeric(df_f$SEN_Provision_No_SEN)
    df_f$sen_yes <- as.numeric(df_f$SEN_provision_SEN_with_statement_EHC)
    
    
    ff <- ggplot() + 
      geom_segment(data=df_f, aes(y=year, yend=year, x=0, xend=max(df_f$sen_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_f, aes(y=year, x=sen_yes, xend=sen_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_f, year=="2015/16"),
                aes(x=sen_yes, y=year, label="Statement/EHC"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_f, year=="2015/16"),
                aes(x=sen_not, y=year, label="No SEN"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_f, aes(x=sen_yes, y=year, label=sen_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_f, color="steelblue2", size=5, vjust=2.5,
                aes(x=sen_not, y=year, label=sen_not)) + 
      geom_text(data=df_f, aes(label=diff, y=year, x=max(df_f$sen_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_f, year=="2015/16"), aes(x=max(df_f$sen_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_f$sen_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
      theme(axis.text=element_text(size=12))
    
    return(ff)
    
  }
  
  if(category=='O') {
    
    sen_one_plus <- sen %>% select(year,characteristic_1,one_plus_fixed_rate) %>% spread(key = characteristic_1, value = one_plus_fixed_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
    
    df_o <- sen_one_plus
    
    df_o$year <- factor(formatyr(df_o$year))
    df_o$sen_not <- as.numeric(df_o$SEN_Provision_No_SEN)
    df_o$sen_yes <- as.numeric(df_o$SEN_provision_SEN_with_statement_EHC)
    
    
    op <- ggplot() + 
      geom_segment(data=df_o, aes(y=year, yend=year, x=0, xend=max(df_o$sen_yes)*1.1), color="#b2b2b2", size=0.15) +
      geom_dumbbell(data=df_o, aes(y=year, x=sen_yes, xend=sen_not),
                    size=2, color="gray", size_x=3, size_xend=3,
                    colour_x="steelblue4", colour_xend="steelblue2") + 
      geom_text(data=filter(df_o, year=="2015/16"),
                aes(x=sen_yes, y=year, label="Statement/EHC"),
                color="steelblue4", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=filter(df_o, year=="2015/16"),
                aes(x=sen_not, y=year, label="No SEN"),
                color="steelblue2", size=5, vjust=-2, fontface="bold") + 
      geom_text(data=df_o, aes(x=sen_yes, y=year, label=sen_yes),
                color="steelblue4", size=5, vjust=2.5) + 
      geom_text(data=df_o, color="steelblue2", size=5, vjust=2.5,
                aes(x=sen_not, y=year, label=sen_not)) + 
      geom_text(data=df_o, aes(label=diff, y=year, x=max(df_o$sen_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
      geom_text(data=filter(df_o, year=="2015/16"), aes(x=max(df_o$sen_yes)*1.2, y=year, label="diff"),
                color="#7a7d7e", size=5, vjust=-2) + 
      scale_x_continuous(expand=c(0,0), limits=c(0, max(df_o$sen_yes)*1.3)) + 
      labs(x=NULL, y=NULL) + 
      theme_bw() + 
      theme(panel.border=element_blank()) + 
      theme(axis.ticks=element_blank()) + 
      theme(axis.text.x=element_blank()) + 
      theme(plot.title=element_text(face="bold")) +
      theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.3))+
      theme(axis.text=element_text(size=12))
    
    return(op) 
    
  }
}


sen_prop <- function(category){
  
  data <- char_ud %>% filter(
    school_type == "Total",
    year == 201516,
    characteristic_desc %in% c('SEN_provision'),
    characteristic_1 != "SEN_Unclassified") %>%
    mutate(characteristic_1 = ifelse(
    characteristic_1 == "SEN_Provision_No_SEN","No SEN",
    ifelse(characteristic_1 == "SEN_provision_SEN_with_statement_EHC", "SEN with statement or EHC",
           ifelse(characteristic_1 == "SEN_provision_SEN_without_statement", "SEN without a statement or EHC",
                  ifelse(characteristic_1 == "Total", "Total","NA")))))
  
  if(category=='P') {
    
    sen_prop <- data %>% select(characteristic_1,perm_excl) 
    
    return(sen_prop %>%
             plot_ly(labels = ~characteristic_1, values = ~perm_excl) %>%
             add_pie(hole = 0.5) %>%
             layout(showlegend = T,legend = list(x = 0.3, y = -0.4),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                    , margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
  }
  
  if(category=='F') {
    
    sen_prop <- data %>% select(characteristic_1,fixed_excl) 
    
    return(sen_prop %>%
             plot_ly(labels = ~characteristic_1, values = ~fixed_excl) %>%
             add_pie(hole = 0.5) %>%
             layout(showlegend = T, legend = list(x = 0.3, y = -0.4),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
  }
  
  if(category=='O') {
    
    sen_prop <- data %>% select(characteristic_1,one_plus_fixed) 
    
    return(sen_prop %>%
             plot_ly(labels = ~characteristic_1, values = ~one_plus_fixed) %>%
             add_pie(hole = 0.5) %>%
             layout(showlegend = T, legend = list(x = 0.3, y = -0.4),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
  }
  
}


# gender <- char_ud %>% filter(school_type=='Total', characteristic_desc =='Gender', characteristic_1 !='Total') %>% 
#   select(year,characteristic_1,perm_excl_rate,fixed_excl_rate,one_plus_fixed_rate)
# 
# gender_perm <- gender %>% select(year,characteristic_1,perm_excl_rate) %>% spread(key = characteristic_1, value = perm_excl_rate) %>%
#   mutate(diff = as.numeric(Gender_male)-as.numeric(Gender_female))
# 
# 
# gender_fixed <- gender %>% select(year,characteristic_1,fixed_excl_rate) %>% spread(key = characteristic_1, value = fixed_excl_rate) %>%
#   mutate(diff = as.numeric(Gender_male)-as.numeric(Gender_female))
# 
# 
# gender_one_plus <- gender %>% select(year,characteristic_1,one_plus_fixed_rate) %>% spread(key = characteristic_1, value = one_plus_fixed_rate) %>%
#   mutate(diff = as.numeric(Gender_male)-as.numeric(Gender_female))
# 
# 
# 
# df_p <- gender_perm
# 
# df_p$year <- factor(formatyr(df_p$year))
# df_p$Gender_female <- as.numeric(df_p$Gender_female)
# df_p$Gender_male <- as.numeric(df_p$Gender_male)
# 
# pp <- ggplot() + 
#   geom_segment(data=df_p, aes(y=year, yend=year, x=0, xend=max(df_p$Gender_male)*1.1), color="#b2b2b2", size=0.15) +
#   geom_dumbbell(data=df_p, aes(y=year, x=Gender_male, xend=Gender_female),
#                          size=2, color="gray", size_x=3, size_xend=3,
#                          colour_x="steelblue4", colour_xend="steelblue2") + 
#   geom_text(data=filter(df_p, year=="2015/16"),
#                      aes(x=Gender_male, y=year, label="boys"),
#                      color="steelblue4", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=filter(df_p, year=="2015/16"),
#                      aes(x=Gender_female, y=year, label="girls"),
#                      color="steelblue2", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=df_p, aes(x=Gender_male, y=year, label=Gender_male),
#                      color="steelblue4", size=4, vjust=2.5) + 
#   geom_text(data=df_p, color="steelblue2", size=4, vjust=2.5,
#                      aes(x=Gender_female, y=year, label=Gender_female)) + 
#   geom_text(data=df_p, aes(label=diff, y=year, x=max(df_p$Gender_male)*1.2), color="#7a7d7e", size=4, vjust =2.5) +
#   geom_text(data=filter(df_p, year=="2015/16"), aes(x=max(df_p$Gender_male)*1.2, y=year, label="diff"),
#                      color="#7a7d7e", size=4, vjust=-2) + 
#   scale_x_continuous(expand=c(0,0), limits=c(0, max(df_p$Gender_male)*1.3)) + 
#   labs(x=NULL, y=NULL, title="Fixed period exclusion rate by gender") + 
#   theme_bw() + 
#   theme(panel.border=element_blank()) + 
#   theme(axis.ticks=element_blank()) + 
#   theme(axis.text.x=element_blank()) + 
#   theme(plot.title=element_text(face="bold"))
# 
# 
# 
# df_f <- gender_fixed
# 
# df_f$year <- factor(formatyr(df_f$year))
# df_f$Gender_female <- as.numeric(df_f$Gender_female)
# df_f$Gender_male <- as.numeric(df_f$Gender_male)
# 
# 
# ff <- ggplot() + 
#   geom_segment(data=df_f, aes(y=year, yend=year, x=0, xend=max(df_f$Gender_male)*1.1), color="#b2b2b2", size=0.15) +
#   geom_dumbbell(data=df_f, aes(y=year, x=Gender_male, xend=Gender_female),
#                 size=2, color="gray", size_x=3, size_xend=3,
#                 colour_x="steelblue4", colour_xend="steelblue2") + 
#   geom_text(data=filter(df_f, year=="2015/16"),
#             aes(x=Gender_male, y=year, label="boys"),
#             color="steelblue4", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=filter(df_f, year=="2015/16"),
#             aes(x=Gender_female, y=year, label="girls"),
#             color="steelblue2", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=df_f, aes(x=Gender_male, y=year, label=Gender_male),
#             color="steelblue4", size=4, vjust=2.5) + 
#   geom_text(data=df_f, color="steelblue2", size=4, vjust=2.5,
#             aes(x=Gender_female, y=year, label=Gender_female)) + 
#   geom_text(data=df_f, aes(label=diff, y=year, x=max(df_f$Gender_male)*1.2), color="#7a7d7e", size=4, vjust =2.5) +
#   geom_text(data=filter(df_f, year=="2015/16"), aes(x=max(df_f$Gender_male)*1.2, y=year, label="diff"),
#             color="#7a7d7e", size=4, vjust=-2) + 
#   scale_x_continuous(expand=c(0,0), limits=c(0, max(df_f$Gender_male)*1.3)) + 
#   labs(x=NULL, y=NULL, title="Fixed period exclusion rate by gender") + 
#   theme_bw() + 
#   theme(panel.border=element_blank()) + 
#   theme(axis.ticks=element_blank()) + 
#   theme(axis.text.x=element_blank()) + 
#   theme(plot.title=element_text(face="bold"))
# 
# 
# df_o <- gender_one_plus
# 
# df_o$year <- factor(formatyr(df_o$year))
# df_o$Gender_female <- as.numeric(df_o$Gender_female)
# df_o$Gender_male <- as.numeric(df_o$Gender_male)
# 
# 
# op <- ggplot() + 
#   geom_segment(data=df_o, aes(y=year, yend=year, x=0, xend=max(df_o$Gender_male)*1.1), color="#b2b2b2", size=0.15) +
#   geom_dumbbell(data=df_o, aes(y=year, x=Gender_male, xend=Gender_female),
#                 size=2, color="gray", size_x=3, size_xend=3,
#                 colour_x="steelblue4", colour_xend="steelblue2") + 
#   geom_text(data=filter(df_o, year=="2015/16"),
#             aes(x=Gender_male, y=year, label="boys"),
#             color="steelblue4", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=filter(df_o, year=="2015/16"),
#             aes(x=Gender_female, y=year, label="girls"),
#             color="steelblue2", size=4, vjust=-2, fontface="bold") + 
#   geom_text(data=df_o, aes(x=Gender_male, y=year, label=Gender_male),
#             color="steelblue4", size=4, vjust=2.5) + 
#   geom_text(data=df_o, color="steelblue2", size=4, vjust=2.5,
#             aes(x=Gender_female, y=year, label=Gender_female)) + 
#   geom_text(data=df_o, aes(label=diff, y=year, x=max(df_o$Gender_male)*1.2), color="#7a7d7e", size=4, vjust =2.5) +
#   geom_text(data=filter(df_o, year=="2015/16"), aes(x=max(df_o$Gender_male)*1.2, y=year, label="diff"),
#             color="#7a7d7e", size=4, vjust=-2) + 
#   scale_x_continuous(expand=c(0,0), limits=c(0, max(df_o$Gender_male)*1.3)) + 
#   labs(x=NULL, y=NULL, title="One or mre fixed period exclusion rate by gender") + 
#   theme_bw() + 
#   theme(panel.border=element_blank()) + 
#   theme(axis.ticks=element_blank()) + 
#   theme(axis.text.x=element_blank()) + 
#   theme(plot.title=element_text(face="bold"))
# 
# 
# pp
# ff
# op
# 
# 
# 
# 
# 
# 
# 
