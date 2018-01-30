# Reason tab

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
