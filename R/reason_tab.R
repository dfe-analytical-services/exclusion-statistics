# Reason tab

clean_la_exclusions_data <- function(x) {
  
  dplyr::select(
    filter(x, level == 'National' | level == 'Local authority' & school_type != 'dummy'),
    year,
    la_name,
    school_type,
    perm_physical_pupils,
    perm_physical_adult,
    perm_verbal_pupil,          
    perm_verbal_adult,
    perm_bullying,
    perm_racist_abuse,          
    perm_sexual_misconduct,
    perm_drug_alcohol,
    perm_damage,                
    perm_theft,
    perm_persistent_disruptive,
    perm_other,
    fixed_physical_pupils,
    fixed_physical_adult,
    fixed_verbal_pupil,         
    fixed_verbal_adult,
    fixed_bullying,
    fixed_racist_abuse,         
    fixed_sexual_misconduct,
    fixed_drug_alcohol,
    fixed_damage,
    fixed_theft,
    fixed_persistent_disruptive,
    fixed_other) %>%
    mutate(la_name = ifelse(is.na(la_name), "England",
                            ifelse(!is.na(la_name), la_name, NA))) %>%
    filter(!is.na(school_type))
  
}

# set order for outputting reason info
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

exclusion_reason_table <- function(la_name_exclusion_select, schtype, category) {
  
  data_long <- clean_la_exclusions_data(reason_ud) %>%
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
  
  # widen dataframe
  x <- data_long %>% 
    spread(key = year, value =  exc) %>% 
    mutate(Trendline = as.factor(paste(`200607`,`200708`,`200809`,`200910`,`201011`, `201112`, `201213`, `201314`, `201415`, `201516`, sep=",")))
  
  # Spaklines can't handle 'X' so force replace with 0.
  x$Trendline <- str_replace_all(x$Trendline, "x", "0")
  
  return(x %>% arrange(reason))
  
}

