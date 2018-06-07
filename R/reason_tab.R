# Reason tab

#---------------------------------------------------------------------
#Read in data required

clean_la_exclusions_data <- read_csv('data/clean_la_exclusions_data.csv', col_types = cols(.default = "c"))

#---------------------------------------------------------------------
#Ordering for outputs

reason_order <- c('Physical assault against a pupil',
                  'Physical assault against an adult',
                  'Verbal abuse / threatening behaviour against a pupil',
                  'Verbal abuse / threatening behaviour against an adult',
                  'Bullying',
                  'Racist abuse',
                  'Sexual misconduct',
                  'Drug and alcohol related',
                  'Damage',
                  'Theft',
                  'Persistent disruptive behaviour',
                  'Other')

#---------------------------------------------------------------------
#Reason table function

exclusion_reason_table <- function(la_name_exclusion_select, schtype, category) {
  
  data_long <- clean_la_exclusions_data %>%
    gather(key = reason,
           value = exc,
           perm_physical_pupils:fixed_other) %>%
    mutate(exclusion_type = ifelse(startsWith(reason, "perm"),"Permanent",
                                   ifelse(startsWith(reason, "fixed"), "Fixed", NA)))  %>%
    filter(la_name == la_name_exclusion_select,
           school_type == schtype,
           exclusion_type == category) %>%
    select(year, la_name, exclusion_type, school_type, reason, exc)
  
  # re-code the reason labels
  data_long$reason <- recode(data_long$reason,
           fixed_bullying="Bullying",
           fixed_damage= "Damage",
           fixed_drug_alcohol="Drug and alcohol related",
           fixed_other= "Other",
           fixed_persistent_disruptive="Persistent disruptive behaviour",
           fixed_physical_adult="Physical assault against an adult",
           fixed_physical_pupils="Physical assault against a pupil",
           fixed_racist_abuse = "Racist abuse",                                              
           fixed_sexual_misconduct = "Sexual misconduct",                                                         
           fixed_theft = "Theft",                                                         
           fixed_verbal_adult = "Verbal abuse / threatening behaviour against an adult",                                                           
           fixed_verbal_pupil = "Verbal abuse / threatening behaviour against a pupil",                                                           
           perm_bullying="Bullying",
           perm_damage= "Damage",
           perm_drug_alcohol="Drug and alcohol related",
           perm_other= "Other",
           perm_persistent_disruptive="Persistent disruptive behaviour",
           perm_physical_adult="Physical assault against an adult",
           perm_physical_pupils="Physical assault against a pupil",
           perm_racist_abuse = "Racist abuse",                                              
           perm_sexual_misconduct = "Sexual misconduct",                                                         
           perm_theft = "Theft",                                                         
           perm_verbal_adult = "Verbal abuse / threatening behaviour against an adult",                                                           
           perm_verbal_pupil = "Verbal abuse / threatening behaviour against a pupil")
  
  # force order of reason presentation
  data_long$reason <- factor(data_long$reason, levels = reason_order)
  
  data_long$year <- sub("(.{4})(.*)", "\\1/\\2",  data_long$year)
  
  temp <- nth(data_long, 6) 
  data_long[, 6] <- sapply(temp, round_with_supressed_values, USE.NAMES = FALSE)
  
  # widen dataframe
  x <- data_long %>% 
    spread(key = year, value =  exc) %>% 
    rename(Reason = reason) %>%
    mutate(Trendline = 0)
    
    for (i in 1:nrow(x)){
    x[i, ncol(x)] <- (paste(x[i, 5:(ncol(x) - 1)], sep="", collapse = ","))
}
  # Spaklines can't handle 'X' so force replace with 0.
  x$Trendline <- str_replace_all(x$Trendline, "x", "0")
  
  
 x[, 6:ncol(x)-1] <- sapply(x[, 6:ncol(x)-1], format_with_supressed_numbers)
 
 x$Reason <- as.factor(x$Reason)
  
  return(x %>% arrange(Reason))
  
}

#---------------------------------------------------------------------
#Download data

exclusion_reason_table_download <- function(la_name_exclusion_select) {
  
  clean_la_exclusions_data %>%
    filter(la_name == la_name_exclusion_select) %>%
    select(year, la_name, school_type, perm_physical_pupils:fixed_other)

}

#---------------------------------------------------------------------
#Text to explain how the tab is working

# reason_text_explainer <- function(schtype, category, la_name_exclusion_select) {
#   
#   data_long <- clean_la_exclusions_data %>%
#     gather(key = reason,
#            value = exc,
#            perm_physical_pupils:fixed_other) %>%
#     mutate(exclusion_type = ifelse(startsWith(reason, "perm"),"Permanent",
#                                    ifelse(startsWith(reason, "fixed"), "Fixed", NA)))  %>%
#     filter(la_name == la_name_exclusion_select,
#            school_type == schtype,
#            exclusion_type == category) %>%
#     select(year, la_name, exclusion_type, school_type, reason, exc)
#   
# 
#   if (category == 'Permanent') {
#     
#     return(paste("The below table refers to permanent period school exclusions data for pupils in ", la_name_exclusion_select, "by reason for the exclusion by selected school type."))
#     
#   } else if (category == 'Fixed') {
#     
#     return(paste("The below table refers to fixed period school exclusions data for pupils in ", la_name_exclusion_select, "by reason for the exclusion by selected school type."))
#     
#   } else {
#     
#      return(paste(""))
#                  
#   }
#   
# }


#schtype <- "Total"
#category <- "Fixed"
#la_name_exclusion_select <- "Wigan"

#reason_text_explainer (schtype, category, la_name_exclusion_select)
