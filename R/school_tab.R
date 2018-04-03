# school tab 

#---------------------------------------------------------------------
#Read in data required

school_data_sql <- read_csv('data/school_data_sql.csv')

#---------------------------------------------------------------------
# School summary

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

#---------------------------------------------------------------------
# School summary

school_data_sql %>%
  select(LA, LAEstab, SchoolName) %>%
  rename(laestab = LAEstab) -> school_data_sql

school_data_sql$laestab <- as.character(school_data_sql$laestab)

all_schools_data <- left_join(school_summary_table, school_data_sql, by = "laestab")

all_schools_data$SchoolName <- tolower(all_schools_data$SchoolName)

all_schools_data$SchoolName <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", all_schools_data$SchoolName, perl=TRUE)

all_schools_data %>%
  mutate(la_no_and_name = paste(LA, la_name, sep = " - "),
         laestab_school_name = paste(laestab, SchoolName, sep = " - ")) -> all_schools_data

all_schools_data %>%
  mutate(la_no_and_name = paste(LA, la_name, sep = " - "),
         laestab_school_name = paste(laestab, SchoolName, sep = " - ")) -> all_schools_data

all_schools_data %>%
  rename(`Academic year` = year,
         `School phase` = school_type,
         `Headcount` = headcount,
         `Permanent exclusions` = perm_excl,
         `Permanent exclusion rate` = perm_excl_rate,
         `Fixed period exclusions` = fixed_excl,
         `Fixed period exclusion rate` = fixed_excl_rate,
         `One or more fixed period exclusions` = one_plus_fixed,
         `One or more fixed period exclusion rate` = one_or_more_fixed_excl_rate) -> all_schools_data

