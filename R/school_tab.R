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

