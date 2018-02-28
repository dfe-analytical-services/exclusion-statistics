# LA trends tab

# Function to clean main_ud file ready for tab

clean_la_data <- function(x) {
  
  dplyr::select(
    filter(x, level == 'National' | level == 'Local authority' & school_type != 'dummy' & la_name != "."),
    year,
    level,
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
                    ifelse(school_type == "total", "Total", NA))))) %>%
    mutate(la_name = ifelse(is.na(la_name), "England",
                            ifelse(!is.na(la_name), la_name, NA))) %>%
    filter(!is.na(school_type))
  
}

clean_la_data_download_tab_1 <- function(x, select2) {
  
  dplyr::select(
    filter(x, level == 'National' | level == 'Local authority' & school_type != 'dummy'),
    year,
    level,
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
    mutate(la_name = ifelse(is.na(la_name), "England",
                            ifelse(!is.na(la_name), la_name, NA))) %>%
    filter(!is.na(school_type)) %>%
    filter(la_name == select2)
  
}

# La trend plot based on rate

la_plot_rate <- function(la, category) {
  
  d <- filter(clean_la_data(main_ud), la_name == la) 
  
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
      scale_colour_manual(values = gov_cols_2[c(1,3,9,8)]) + 
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = school_type),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
}

# la plot based on number

la_plot_num <- function(la, category) {
  
  d <- filter(clean_la_data(main_ud), la_name == la) 
  
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
      scale_colour_manual(values = gov_cols_2[c(1,3,9,8)]) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = school_type),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
}

# LA time series table based on number

la_table_num <- function(la, category) {
  
  d <-  filter(clean_la_data(main_ud), la_name == la)
  
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

# LA time series table based on rate

la_table_rate <- function(la, category) {
  
  d <- filter(clean_la_data(main_ud), la_name == la)
  
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
  
  d <- filter(clean_la_data(main_ud),la_name == la, year == refyear)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(perm_excl))
  
}

la_fixed_num <- function(la, refyear) {
  
  d <- filter(clean_la_data(main_ud), year == refyear,la_name == la)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(fixed_excl))
  
}

la_one_plus_num <- function(la, refyear) {
  
  d <- filter(clean_la_data(main_ud), year == refyear,la_name == la)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(one_plus_fixed))
  
}


la_perm_rate <- function(la, refyear) {
  
  d <- filter(clean_la_data(main_ud), year == refyear,la_name == la)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(perm_excl_rate))
  
}

la_fixed_rate <- function(la, refyear) {
  
  d <- filter(clean_la_data(main_ud), year == refyear,la_name == la)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(fixed_excl_rate))
  
}

la_one_plus_rate <- function(la, refyear) {
  
  d <- filter(clean_la_data(main_ud), year == refyear,la_name == la)
  
  return(filter(d, school_type == 'Total') %>%
           dplyr::select(one_or_more_fixed_excl_rate))
  
}


# Preparing data for LA, region, national comparison

comparison_la_data <- function(x) {
  
  dplyr::select(
    filter(x, (level == 'National' | level == 'Region' | level == 'Local authority') & school_type == 'total'),
    year,
    school_type,
    level,
    region_name,
    la_name,
    perm_excl_rate,
    fixed_excl_rate,
    one_or_more_fixed_excl_rate) %>%
  mutate(area = ifelse(is.na(la_name) & is.na(region_name), "England",
                          ifelse(is.na(la_name), region_name,la_name ))) 
  
}

# Preparing data for LA, region, national comparison downladd with all school types

comparison_la_data_download_prepare <- function(x) {
  
  dplyr::select(
    filter(x, (level == 'National' | level == 'Region' | level == 'Local authority')),
    year,
    school_type,
    level,
    region_name,
    la_name,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate) %>%
    mutate(area = ifelse(is.na(la_name) & is.na(region_name), "England",
                         ifelse(is.na(la_name), region_name,la_name ))) 
  
}
# LA, region, national comparison plot

la_compare_plot <- function(la, category) {
  
  reg <- (filter(comparison_la_data(main_ud), la_name == la) %>% select(region_name))[1,]
  
  d <- filter(comparison_la_data(main_ud), area %in% c(la, reg, 'England')) 
  
  if (category == 'P') {
    ylabtitle <- "Permanent exclusion percentage"
    d <- d %>% mutate(y_var = perm_excl_rate) #%>% filter(y_var != 'x') 
  }
  
  if (category == 'F') {
    ylabtitle <- "Fixed period exclusion percentage"
    d <- d %>% mutate(y_var = fixed_excl_rate) #%>% filter(y_var != 'x') 
  }
  
  if (category == 'O') {
    ylabtitle <- "One or more fixed period exclusion percentage"
    d <- d %>% mutate(y_var = one_or_more_fixed_excl_rate) #%>% filter(y_var != 'x') 
  }
  
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(y_var), 
          group = area, colour = as.factor(area)) +
      geom_path(size = 1) +
      xlab("Academic year") +
      ylab(ylabtitle) +
      scale_colour_manual(values = gov_cols_2[c(1,3,9,8)]) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = area),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))
}

# Download function for the la data comparison

comparison_la_data_download_tab_2 <- function(x, la) {
  
  
  reg <- (filter(comparison_la_data_download_prepare(x), la_name == la) %>% select(region_name))[1,]
  
  d <- filter(comparison_la_data_download_prepare(x), area %in% c(la, reg, 'England')) 
  
  download <- d %>% select(year, 
                           level, 
                           region_name, 
                           la_name, 
                           school_type,
                           headcount,
                           perm_excl,
                           perm_excl_rate,
                           fixed_excl,
                           fixed_excl_rate,
                           one_plus_fixed,
                           one_or_more_fixed_excl_rate)
  
}


# LA, region, national comparison table

la_compare_table <- function(la, category) {
  
  reg <- (filter(comparison_la_data(main_ud), la_name == la) %>% select(region_name))[1,]
  
  d <- filter(comparison_la_data(main_ud), area %in% c(la, reg, 'England')) 
  
  if(category=='P') { 
    d <- d %>% mutate(t_var = perm_excl_rate)
  }
  if(category=='F') {
    d <- d %>% mutate(t_var = fixed_excl_rate)
  }  
  if(category=='O') {
    d <- d %>% mutate(t_var = one_or_more_fixed_excl_rate)
  }  
  
  if(la != 'England') {t_order <- c("England", reg, la)}
  else {t_order <- c("England")}
    
  table <- d %>%
    mutate(
      yearf = formatyr(year),
      value = t_var,
      Type = factor(area, levels = t_order)
    ) %>%
    dplyr::select(yearf, Type, value) %>%
    spread(key = yearf, value) %>% 
    arrange(Type)
  
  row.names(table) <- NULL
  
  return(table)
  
}

