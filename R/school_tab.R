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




####
# School naming ----

# We have 3 problems to address here.
# 1. We want to have one name per school for our app
# 1. Some schools have multiple names on the same laestab number
# 2. Of those schools, some are currently open
# 3. Of those schools, some are shut

# This sequence below takes data from the get schools information website and determines the name of the school to be attached
# to the laestab number. 
# i. We start by taking those laestab numbers that are disctinct and keep those schools names 
# ii. Then we take those where there are schools with the same laestab number that are open and keep the name at  the time they were last open (NA)
# iii. Then we take those where there are schools with the same laestab number that are closed and then keep the school name at the latest closing date
# iv. Join all the data together 

school_names_raw$`LA (code)` <- as.character(school_names_raw$`LA (code)`)
school_names_raw$EstablishmentNumber <- as.character(school_names_raw$EstablishmentNumber)
school_names_raw$CloseDate <- as.Date(school_names_raw$CloseDate, "%d-%m-%Y")

# Create laestab value 
school_names_raw$laestab <- as.numeric(paste(school_names_raw$`LA (code)`, school_names_raw$EstablishmentNumber, sep = ""))

# Get distinct columns 
school_names_raw %>%
  filter(!is.na(`LA (code)`) & !is.na(EstablishmentNumber) & `LA (code)` != 0) %>%
  distinct(`LA (code)`, EstablishmentNumber, laestab, EstablishmentName, CloseDate)  %>% 
  arrange(laestab) -> arranged_schools_data

# i. Find laestabs that are distinct ----
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()<2) -> distinct_school_names

# Check this with a count
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()<2) %>%
  summarize(count=n()) -> no_of_distinct_schools_check

# Find laestabs that are not distinct ----
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()>1) -> non_distinct_school_names

# Check this with a count
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()>1) %>%
  summarize(count=n())-> no_of_non_distinct_schools_check

# ii. Take those schools that are not distinct but are still open, take those names ----
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()>1) %>%
  filter(is.na(CloseDate)) %>%
  arrange(laestab)-> open_schools_non_distinct

# iii. Take those schools where they have a close date and take their name at the latest year (using slice) of being open remove those schools that are open using the anti_join ----
arranged_schools_data %>%
  group_by(laestab) %>%
  filter(n()>1) %>%
  anti_join(open_schools_non_distinct, by = "laestab") %>%
  slice(which.max(CloseDate)) -> closed_schools_non_distinct

# iv. Join data together ----
rbind(distinct_school_names,
      open_schools_non_distinct,
      closed_schools_non_distinct) -> unique_school_data

(unique(unique_school_data$laestab))

unique_school_data %>%
  select(laestab, EstablishmentName) -> unique_school_names

# Join the data together to use as a table (add establishment name to the data by the laestab number)
school_summary_table$laestab <- as.numeric(school_summary_table$laestab)
all_schools_data <- left_join(school_summary_table, unique_school_names, by = "laestab")

# Arrange the data alphabetically by la_name THEN EstablishmentName
all_schools_data %>%
  arrange(la_name, EstablishmentName) -> all_schools_data


