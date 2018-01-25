
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
# 4. Front page ----

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

####
# 4. LA trends ----

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
# 4. School page ----

#school level info for a specific LA

la_sch_table <- function(la,refyear) {
  
  d <- filter(main_ud, level == "School") %>%
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


d <- filter(main_ud, level == "School") %>%
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




  e <- filter(main_ud, level == "School") %>%
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  dataset <- reactive({
    data <- d
    if (length(input$la_name)){
      data$c1 <- grepl(paste(input$la_name, collapse = "|"), data$la_name)
    } 
    else {
      data$c1 <- TRUE
    }
    
    if (length(input$laestab)){
      data$c2 <- grepl(paste(input$laestab, collapse = "|"), data$laestab)
    }
    else {
      data$c2 <- TRUE
    }
    
    data[data$c1 & data$c2, c("year", "la_name", "laestab", "school_type")]
    
  })
  

####
# 5. MAP ----

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


####
# 5. Reason page  ----

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
# 4. Characteristics ----


## FSM

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
  data <- data %>% mutate(t_var = perm_excl)
  }
  if(category=='F') {
  data <- data %>% mutate(t_var = fixed_excl)
  }
  if(category=='O') {   
  data <- data %>% mutate(t_var = one_plus_fixed)
  }
    
  fsm_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- fsm_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
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
    data <- data %>% mutate(t_var = perm_excl_rate)
  }
  if(category=='F') {
    data <- data %>% mutate(t_var = fixed_excl_rate)
  }
  if(category=='O') {   
    data <- data %>% mutate(t_var = one_plus_fixed_rate)
  }
  
  fsm_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- fsm_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
}


fsm_gap <- function(category) {
  
  fsm <- char_ud %>% filter(school_type=='Total', characteristic_desc =='FSM_Eligible', characteristic_1 !='Total') %>% 
    select(year,characteristic_1,perm_excl_rate,fixed_excl_rate,one_plus_fixed_rate) %>%
    filter(characteristic_1 != 'FSM_Unclassified')
  
  if(category=='P') {
    data <- fsm %>% select(year,characteristic_1,perm_excl_rate) %>% spread(key = characteristic_1, value = perm_excl_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
  }
  
  if(category=='F') {
    data <- fsm %>% select(year,characteristic_1,fixed_excl_rate) %>% spread(key = characteristic_1, value = fixed_excl_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
  }
  
  if(category=='O') {
    data <- fsm %>% select(year,characteristic_1,one_plus_fixed_rate) %>% spread(key = characteristic_1, value = one_plus_fixed_rate) %>%
      mutate(diff = as.numeric(FSM_Eligible)-as.numeric(FSM_NotEligible))
  }
  
  data$year <- factor(formatyr(data$year))
  data$fsm_not <- as.numeric(data$FSM_NotEligible)
  data$fsm_yes <- as.numeric(data$FSM_Eligible)
  
  gap_plot <- ggplot() + 
    geom_segment(data=data, aes(y=year, yend=year, x=0, xend=max(data$fsm_yes)*1.1), color="#b2b2b2", size=0.15) +
    geom_dumbbell(data=data, aes(y=year, x=fsm_yes, xend=fsm_not),
                  size=2, color="gray", size_x=3, size_xend=3,
                  colour_x="steelblue4", colour_xend="steelblue2") + 
    geom_text(data=filter(data, year=="2015/16"),
              aes(x=fsm_yes, y=year, label="FSM eligible"),
              color="steelblue4", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=filter(data, year=="2015/16"),
              aes(x=fsm_not, y=year, label="Non-FSM eligible"),
              color="steelblue2", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=data, aes(x=fsm_yes, y=year, label=fsm_yes),
              color="steelblue4", size=5, vjust=2.5) + 
    geom_text(data=data, color="steelblue2", size=5, vjust=2.5,
              aes(x=fsm_not, y=year, label=fsm_not)) + 
    geom_text(data=data, aes(label=diff, y=year, x=max(data$fsm_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
    geom_text(data=filter(data, year=="2015/16"), aes(x=max(data$fsm_yes)*1.2, y=year, label="diff"),
              color="#7a7d7e", size=5, vjust=-2) + 
    scale_x_continuous(expand=c(0,0), limits=c(0, max(data$fsm_yes)*1.3)) + 
    labs(x=NULL, y=NULL) + 
    theme_bw() + 
    theme(panel.border=element_blank()) + 
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_blank()) + 
    theme(plot.title=element_text(face="bold")) +
    theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
    theme(axis.text=element_text(size=12))
  
  return(gap_plot)
  
}
  
  
## SEN

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
    data <- data %>% mutate(t_var = perm_excl)
  }
  if(category=='F') {
    data <- data %>% mutate(t_var = fixed_excl)
  }
  if(category=='O') {   
    data <- data %>% mutate(t_var = one_plus_fixed)
  }
  
  sen_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- sen_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
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
    data <- data %>% mutate(t_var = perm_excl_rate)
  }
  if(category=='F') {
    data <- data %>% mutate(t_var = fixed_excl_rate)
  }
  if(category=='O') {   
    data <- data %>% mutate(t_var = one_plus_fixed_rate)
  }
  
  sen_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- sen_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
}


sen_gap <- function(category) {
  
  sen <- char_ud %>% filter(school_type=='Total', characteristic_desc =='SEN_provision', characteristic_1 !='Total') %>% 
    select(year,characteristic_1,perm_excl_rate,fixed_excl_rate,one_plus_fixed_rate) %>%
    filter(characteristic_1 %in% c('SEN_provision_SEN_with_statement_EHC', 'SEN_Provision_No_SEN'))
  
  if(category=='P') {
    data <- sen %>% select(year,characteristic_1,perm_excl_rate) %>% spread(key = characteristic_1, value = perm_excl_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
  }
  
  if(category=='F') {
    data <- sen %>% select(year,characteristic_1,fixed_excl_rate) %>% spread(key = characteristic_1, value = fixed_excl_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
  }
  
  if(category=='O') {
    data <- sen %>% select(year,characteristic_1,one_plus_fixed_rate) %>% spread(key = characteristic_1, value = one_plus_fixed_rate) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC)-as.numeric(SEN_Provision_No_SEN))
  }
  
  data$year <- factor(formatyr(data$year))
  data$sen_not <- as.numeric(data$SEN_Provision_No_SEN)
  data$sen_yes <- as.numeric(data$SEN_provision_SEN_with_statement_EHC)
  
  gap_plot <- ggplot() + 
    geom_segment(data=data, aes(y=year, yend=year, x=0, xend=max(data$sen_yes)*1.1), color="#b2b2b2", size=0.15) +
    geom_dumbbell(data=data, aes(y=year, x=sen_yes, xend=sen_not),
                  size=2, color="gray", size_x=3, size_xend=3,
                  colour_x="steelblue4", colour_xend="steelblue2") + 
    geom_text(data=filter(data, year=="2015/16"),
              aes(x=sen_yes, y=year, label="Statement/EHC"),
              color="steelblue4", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=filter(data, year=="2015/16"),
              aes(x=sen_not, y=year, label="No SEN"),
              color="steelblue2", size=5, vjust=-2, fontface="bold") + 
    geom_text(data=data, aes(x=sen_yes, y=year, label=sen_yes),
              color="steelblue4", size=5, vjust=2.5) + 
    geom_text(data=data, color="steelblue2", size=5, vjust=2.5,
              aes(x=sen_not, y=year, label=sen_not)) + 
    geom_text(data=data, aes(label=diff, y=year, x=max(data$sen_yes)*1.2), color="#7a7d7e", size=5, vjust =2.5) +
    geom_text(data=filter(data, year=="2015/16"), aes(x=max(data$sen_yes)*1.2, y=year, label="diff"),
              color="#7a7d7e", size=5, vjust=-2) + 
    scale_x_continuous(expand=c(0,0), limits=c(0, max(data$sen_yes)*1.3)) + 
    labs(x=NULL, y=NULL) + 
    theme_bw() + 
    theme(panel.border=element_blank()) + 
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_blank()) + 
    theme(plot.title=element_text(face="bold")) +
    theme(plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2))+
    theme(axis.text=element_text(size=12))
  
  return(gap_plot)
  
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


