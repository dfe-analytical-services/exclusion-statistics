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



exclusion_reason_table <- function(la_name_exclusion_select, schtype, reason_for_exclusion) {
  
  data_long <- clean_la_exclusions_data(reason_ud) %>% 
    gather(key = reason, value = exc, perm_physical_pupils:fixed_other) %>%
    mutate(exclusion_type = ifelse(startsWith(reason, "perm"), "Permanent", 
                                   ifelse(startsWith(reason, "fixed"), "Fixed",NA))) %>%
          filter(year >= 200607,
                 !is.na(la_name),
                 la_name == la_name_exclusion_select,
                 school_type == schtype,
                 exclusion_type == reason_for_exclusion) %>%
    select(year, la_name, exclusion_type,school_type, reason, exc)
  
  
  return(data_long %>% spread(key = year, value =  exc) %>% mutate(Trendline = as.factor(paste(`200607`,`200708`,`200809`,`200910`,`201011`, `201112`, `201213`, `201314`, `201415`, `201516`, sep=","))))
  
}
